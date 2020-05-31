library(data.table)
library("dplyr")
library(mice)

# Indica de la columna indicada.
indexOf <- function(dataset, name) {
    which(names(dataset) %in% c( name ))
}

# Excluye las columnas indicadas.
excludeCols <- function(dataset, name) {
    dataset[ , -which(names(dataset) %in% name)]
}

# Normaliza 'x' en '[min, max]'.
normalize <- function(x, min, max) {
    (x - min) / (max - min)
}

# Normaliza a [-1, 1]
normalizeInCero <- function(x, min, max) {
    x / max(abs(min), abs(max))
}


# Normaliza 'x' en '[min(X), max(x)]'. Calcula el mínimo y máximo según los datos de 'x'.
normalizeAut <- function(x) {
    normalize(x, min(x), max(x))
}

# Prepara los datos para hacer un submit a kaggle
submit <- function(data, predict, fileName) {
    submit <- data.frame(data$shot_id, as.numeric(as.character(predict)))
    names(submit) <- c( "shot_id", "shot_made_flag" )
    write.table(submit, file = fileName, sep = ",", row.names = FALSE, quote = FALSE)
}

# Convierte todas las columnas en numéricas, excepto las indicadas en 'names'.
asNumericExcept <- function(dataset, except) {
    partial <- dataset[ , -which(names(dataset) %in% except) ]

    #partial  <- lapply(partial, as.numeric)
    for(i in 1:ncol(partial)) {
        partial[ , i ] <- as.numeric(partial[ , i ])
    }

    dataset <- update(dataset, partial)
    dataset
}

asNumericAll <- function(dataset) {
    for(i in 1:ncol(dataset)) {
        dataset[ , i ] <- as.numeric(dataset[ , i ])
    }
    dataset
}

# Une todos los niveles cuya probabilidad en 'dataset' sea menor que 'prob'
mergeLevels <- function(dataset, class, var, prob) {
    #levels(data.filtered$action_type)
    max <- nrow(dataset)
    
    percentages <- dataset %>%
        select(var, class) %>%
        mutate(value = as.numeric(as.character( dataset[[ class ]] ))) %>%
        group_by_at(var) %>%
        summarise(per = n())
    
    prob <- prob * max / 100
    #print( paste("Corte: ", p ) )
    
    first  <- percentages %>% filter(per >= prob)
    second <- percentages %>% filter(per < prob)
    
    levels <- list()
    for (y in first[[ var ]]) {
        levels[[ as.character(y) ]] <- c( y )
    }
    
    levels[[ "other" ]] <- as.vector(as.character(second[[ var ]]))
    as.vector(levels)
}


# Filtra el nombre del factor 'f' para que no contenga los caracteres '())-+.' ni espacios.
filterFactorName <- function(f) {
    filterName( levels( f ) )
}

# Filtra el nombre del factor 'f' para que no contenga los caracteres '())-+.' ni espacios.
filterName <- function(f) {
    gsub("[)-+.]", "", gsub("([ ]+[(]?)|-|[(]", "_", f ))
}

# Mueve la variable 'name' (normalmente la clase) al final de 'dataset'.
moveToLast <- function(dataset, name) {
    class  <- dataset[ , name ]
    others <- dataset[ , -which(names(dataset) %in% c( name )) ]
    others[ name ] <- class
    others
}

# Discretiza la variable 'toDiscretize' del 'dataset' con variable clase 'class', usando 'discretizer'.
discretizeOne <- function(dataset, discretizer, toDiscretize, class) {
    # Se toman solo las columnas toDiscretize + class
    #subset        <- dataset %>% subset(select = c( toDiscretize, class))
    subset        <- data.frame(dataset[[ toDiscretize ]], dataset[[ class ]])
    names(subset) <- c(toDiscretize, class)
    disc          <- discretizer(subset)
    #disc          <- mdlp(subset)
    subset.disc   <- disc$dataset

    # Se obtiene la discretización de los datos de entrenamiento
    # Se actualiza train_set con los datos discretizados
    updateAndFactor(dataset, subset.disc)
}

# Discretiza todas las variables numéricas del 'dataset', usando 'discretizer'.
# La última columna es variable clase.
discretizeAll <- function(dataset, discretizer) {
    n         <- ncol(dataset)
    className <- names(dataset)[ n ]

    # Se toman solo las columnas numéricas + clase
    subset      <- selectNumericAndClass(dataset)
    disc        <- discretizer(subset)
    subset.disc <- disc$dataset
    
    # Se obtiene la discretización de los datos de entrenamiento
    # Se actualiza train_set con los datos discretizados
    updateAndFactor(dataset, disc$dataset)
}

# Indica si contine datos numéricos
containsNumeric <- function(dataset) {
    num <- (unlist(lapply(dataset, is.numeric)))
    sum(num) > 0
}

# Filtra todos las variables excepto las numéricas y la clase.
selectNumericAndClass <- function(dataset) {
    n <- ncol(dataset)
    className <- names(dataset)[ n ]

    # Se toman solo las columnas numéricas + clase
    dataset_num <- select_if(dataset, is.numeric)
    dataset_num[ className ] <- dataset[ , n ]
    dataset_num
}

# Filtra todos las variables excepto las numéricas.
selectNumeric <- function(dataset) {
    # Se toman solo las columnas numéricas
    dataset_num <- select_if(dataset, is.numeric)
    dataset_num
}

# Actualiza un dataset con los datos de otro. Si son numéricos, los convierte en factor ordenados.
updateAndFactor <- function(dataset, toUpdate) {
    COPY <- data.frame(dataset)
    for(i in 1:(ncol(toUpdate))) {
        name <- names( toUpdate )[i]
        if (is.numeric(toUpdate[ , name ])) {
            COPY[ name ] <- ordered(toUpdate[ , name ])
        } else {
            COPY[ name ] <- toUpdate[ , name ]
        }
    }
    COPY
}

# Actualiza un dataset con los datos de otro.
update <- function(dataset, toUpdate) {
    COPY <- data.frame(dataset)
    for(i in 1:(ncol(toUpdate))) {
        name <- names( toUpdate )[i]
        COPY[ name ] <- toUpdate[ , name ]
    }
    COPY
}

# Aplica los puntos de corte a una copia del dataset y lo devuelve.
# Añade como limite inferior y superior -1000000 y 1000000.
aplicarCutpoints <- function(dataset, cutpoints_list) {
    min <- -1000000
    max <-  1000000
    
    c_dataset <- copy(dataset)
    for(c in 1:(ncol(c_dataset) - 1)) {
        cutpoints <- cutpoints_list[ c ]
        col       <- c_dataset[[ c ]]
        
        # Se añade los extremos min y max a los cutpoints
        cutpoints <- c( min, unlist(cutpoints), max )
        
        c_dataset[[ c ]] <- cut(col, breaks = cutpoints, labels = FALSE, ordered_result = true)
    }
    c_dataset
}

# Funcion de perdida LogLoss
MyLogLoss<-function(actual, predicted) {
    predicted <- (pmax(predicted, 0.00001))
    predicted <- (pmin(predicted, 0.99999))
    result    <- (-1/length(actual)) * (sum((actual * log(predicted) + (1 - actual) * log(1 - predicted))))
    return(result)
}

# Discretizacion no supervisada.
discretizeUnsupervised <- function(dataset, method, breaks = 3) {
    set.seed(123)
    
    # Para todas las columnas excepto la clase (última columna)
    cutp <- list()
    for(i in 1:(ncol(dataset) - 1)) {
        set.seed(123)
        
        cutp[[i]] <- arules::discretize(dataset[,i], method = method, breaks = breaks, onlycuts = TRUE)
    }
    
    disc <- aplicarCutpoints(dataset, cutp)
    list( dataset = disc,
          cutpoints = cutp )
}


# Convierte todas las columnas en numéricas, excepto las indicadas en 'names'.
factor2Numeric <- function(trainTest, exclusions) {
    trainTest$train <- asNumericExcept(trainTest$train, exclusions)
    trainTest$test  <- asNumericExcept(trainTest$test, exclusions)
    trainTest
}

# Preprocesa las variables numéricas, por ejemplo, usando los métodos center o scale.
preprocessNumeric <- function(trainTest, method) {
    train_fold <- trainTest$train
    test_fold <- trainTest$test

    if (containsNumeric(train_fold)) {
        # Columnas con las variables numéricas
        train_num <- selectNumeric(train_fold)
        test_num  <- selectNumeric(test_fold)

        # Se preprocesa todas las variables numéricas
        set.seed(123)
        preProc   <- preProcess(train_num, method = method)
        train_num <- predict(preProc, train_num)
        test_num  <- predict(preProc, test_num)

        # Se actualizan los datos originales
        train_fold <- update(train_fold, train_num)
        test_fold  <- update(test_fold, test_num)
    }
    list( train = train_fold,
          test = test_fold )
}

# Preprocesa todas las variables, por ejemplo, usando los métodos pca o knnImpute.
preprocessAll <- function(trainTest, method) {
    train_fold <- trainTest$train
    test_fold <- trainTest$test

    # Se preprocesa todas las variables
    set.seed(123)
    preProc    <- preProcess(train_fold, method = method)
    train_fold <- predict(preProc, train_fold)
    test_fold  <- predict(preProc, test_fold)

    list( train = train_fold,
          test = test_fold )
}

# Aplica un ciclo de entrenamiento y después aplica un ciclo de test.
trainAndTest <- function(trainTest, trainMethod,
                         trControl = trainControl(method = "none"),
                         tuneLength = ifelse(trControl$method == "none", 1, 3),
                         params = list()) {
    train_fold <- trainTest$train
    test_fold <- trainTest$test

    set.seed(123)
    
    className <- names(train_fold)[ ncol(train_fold) ]
    
    # Se entrena el modelo usando los datos discretizados
    clasificador <- train(formula(paste(className, " ~ ." )), # class ~ .
                          data = train_fold,
                          method = trainMethod,
                          trControl = trControl,
                          tuneLength = tuneLength)
                          #trace = TRUE,
                          #allowParallel=TRUE)

    # Se evalua la precisión
    result <- evaluateAccuracy(clasificador, test_fold)
    list( model = clasificador,
          accuracy = result$accuracy,
          logLoss = result$logLoss )
}

onlyTrain2 <- function(trainTest, trainFun) {
    train_fold <- trainTest$train

    set.seed(123)
    
    className <- names(train_fold)[ ncol(train_fold) ]
    
    # Se entrena el modelo usando los datos discretizados
    clasificador <- trainFun(formula(paste(className, " ~ ." )), train_fold)
    
    # Se evalua la precisión
    result <- evaluateAccuracy(clasificador, test_fold)
    list( model    = clasificador,
          accuracy = result$accuracy,
          logLoss  = result$logLoss,
          y_pred   = result$y_pred )
}




# Aplica un ciclo de entrenamiento y después aplica un ciclo de test.
trainAndTest2 <- function(trainTest, trainFun) {
    train_fold <- trainTest$train
    test_fold <- trainTest$test

    set.seed(123)
    
    className <- names(train_fold)[ ncol(train_fold) ]
    
    # Se entrena el modelo usando los datos discretizados
    clasificador <- trainFun(formula(paste(className, " ~ ." )), train_fold)

    # Se evalua la precisión
    result <- evaluateAccuracy(clasificador, test_fold)
    list( model    = clasificador,
          accuracy = result$accuracy,
          logLoss  = result$logLoss,
          y_pred   = result$y_pred )
}

# Se evalua la precisión del clasificador para los datos de test
evaluateAccuracy <- function(clasificador, test_fold)  {
    # Se separan las propiedades de test y la clase del test (última columna)
    n           <- ncol(test_fold)
    x_test_fold <- test_fold[ , -n ]
    y_test_fold <- test_fold[[ n ]]

    set.seed(123)

    # Predicción sobre el modelo entrenado
    y_pred <- stats::predict(clasificador, newdata = x_test_fold)

    print("Datos reales:")
    sum(as.numeric(as.character(y_test_fold)))

    print("Predicción:")
    sum(as.numeric(as.character(y_pred)))

    # Se calcula la matriz de confusión y la precisión
    cm <- caret::confusionMatrix(data = y_pred, reference = y_test_fold)

    accuracy <- cm$overall["Accuracy"]
    print(paste("accuracy: ", accuracy))

    logLoss <- MyLogLoss(as.numeric(as.character(y_test_fold)), as.numeric(as.character(y_pred)))
    print(paste("logLoss: ", logLoss))

    logLoss2 <- LogLoss(as.numeric(as.character(y_pred)), as.numeric(as.character(y_test_fold)))
    print(paste("logLoss2: ", logLoss2))
    
    list( accuracy = accuracy,
          logLoss  = logLoss,
          y_pred   = y_pred )
}

# Discretiza los datos de entrenamiento y devuelve la discretización de los mismo, así como un método para discretizar,
# usando los mismos criterios, los datos de test.
discretize <- function(trainTest, discretizer) {
    train_fold <- trainTest$train
    test_fold <- trainTest$test
    
    if (containsNumeric(train_fold)) {
        className <- names(train_fold)[ ncol(train_fold) ]

        # Se toman solo las columnas numéricas + clase
        train_fold_num <- selectNumericAndClass(train_fold)
        test_fold_num  <- selectNumericAndClass(test_fold)

        # Se calcula la discretización a partir de los datos de entrenamiento
        discretizer.data <- discretizer(train_fold_num)

        # Se obtiene la discretización de los datos de entrenamiento
        train_fold_num.disc <- discretizer.data$dataset

        # Puntos de corte
        cutpoints <- discretizer.data$cutpoints

        # Se aplica la discretización YA CALCULADA sobre los datos de test
        test_fold_num.disc <- aplicarCutpoints(test_fold_num, cutpoints)

        # Se actualiza train_fold y test_fold con los datos discretizados
        train_fold <- updateAndFactor(train_fold, train_fold_num.disc)
        test_fold  <- updateAndFactor(test_fold, test_fold_num.disc)
    }
    list( train = train_fold,
          test = test_fold )
}

discretize_v2 <- function(trainTest, discretizer, names) {
    train_fold <- trainTest$train
    test_fold <- trainTest$test
    
    if (containsNumeric(train_fold)) {
        className <- names(train_fold)[ ncol(train_fold) ]

        # Se toman solo las columnas numéricas + clase
        train_fold_num <- train_fold[ ,which(names(train_fold) %in% names) ]
        test_fold_num  <- test_fold[ ,which(names(test_fold) %in% names) ]

        moveToLast(train_fold_num, className)
        moveToLast(test_fold_num, className)
        
        # Se calcula la discretización a partir de los datos de entrenamiento
        discretizer.data <- discretizer(train_fold_num)
        
        # Se obtiene la discretización de los datos de entrenamiento
        train_fold_num.disc <- discretizer.data$dataset
        
        # Puntos de corte
        cutpoints <- discretizer.data$cutpoints
        
        # Se aplica la discretización YA CALCULADA sobre los datos de test
        test_fold_num.disc <- aplicarCutpoints(test_fold_num, cutpoints)
        
        # Se actualiza train_fold y test_fold con los datos discretizados
        train_fold <- updateAndFactor(train_fold, train_fold_num.disc)
        test_fold  <- updateAndFactor(test_fold, test_fold_num.disc)
    }
    list( train = train_fold,
          test = test_fold )
}


# Aplica una validación cruzada.
crossValidation <- function(dataset, k, nextStep) {
    set.seed(333)
    
    # Se calculan los folds para la validación cruzada
    # La clase es la última columna
    #folds <- createFolds(dataset$class, k = k)
    folds <- createFolds(dataset[ ncol(dataset) ], k = k)
    
    # Validación cruzada
    cv <- lapply(folds, function(x) {
        train_fold <- dataset[-x, ]
        test_fold  <- dataset[x, ]
        
        set.seed(123)
        
        # Se aplica el método validador sobre los set de entranamiento y test
        result <- nextStep(train_fold, test_fold)
        result
    })
    
    # Se calcula la precisión como la media de todas las precisiones
    accuracy <- mean(as.numeric(cv$accuracy))
    logLoss  <- mean(as.numeric(cv$logLoss))

    best <- -1
    for (i in cv) {
        if (i$accuracy > best) {
            model <- i$model
        }
    }
    
    list( model = model, accuracy = accuracy, logLoss = logLoss )
}

# Aplica una validación partiendolo datos en train y test.
split <- function(dataset, p) {
    set.seed(123)
    inTrain  = createDataPartition(y = data.fullRecords$shot_made_flag, p = p, list = FALSE)
    train_fold <- dataset[inTrain,]
    test_fold  <- dataset[-inTrain,]

    list( train = train_fold,
          test = test_fold )
}


# Calcula los outliers IQR 
# Devuelve un vector TRUE/FALSE indicando si el registro i-ésimo 
# de "datos" es o no un outlier IQR con respecto a la columna de índice "indice"
# coef es 1.5 para los outliers normales y hay que pasarle 3 para los outliers extremos

outliersIQR = function (datos, indice, coef = 1.5){
    columna = datos[ , indice]

    q1 = quantile(columna)[2] # mínimo
    q3 = quantile(columna)[4] # máximo.
    iqr = q3 - q1

    #q1 = quantile(columna, 0.25)
    #q3 = quantile(columna, 0.75)
    #iqr = IQR(columna)
    
    max = (iqr * coef) + q3
    min = q1 - (iqr * coef)
    outlier = columna > max | columna < min
    return (outlier)
}

###########################################################################
# Devuelve un vector con las claves de los outliers IQR
# con respecto a la columna de índice "indice"
# coef es 1.5 para los outliers normales y hay que pasarle 3 para los outliers extremos

vector_claves_outliers_IQR = function(datos, indice, coef = 1.5){
    columna.datos = datos[,indice]
    vector.de.outliers = vector_es_outlier_IQR(datos, indice, coef)
    which(vector.de.outliers  == TRUE)
}


imputeOutliers <- function(dataset, whis = 1.5) {
    # Se calculan los cuartiles y se guardan en una lista
    qntile_list <- list()
    for(i in 1:(ncol(dataset) - 1)) {
        col <- dataset[,i]

        qntile_list[[ i ]] <- quantile( col, c(.25, .75), na.rm = FALSE )
    }

    # Se devuelve una funcion que imputa los outliers
    remove <- function(otherDataset) {
        copy <- data.frame(otherDataset)
        for(i in 1:(ncol(copy) - 1)) {
            qntile <- qntile_list[[ i ]]
            q25 <- qntile[1]
            q75 <- qntile[2]
            iqr = q75 - q25

            col <- copy[,i]

            col[ abs(col - mean(col)) > (whis * iqr) ] <- NA

            copy[,i] <- col
        }
        copy
    }
    remove
}

imputeOutliers_v1 <- function(dataset, indice, whis = 1.5) {
    column <- dataset[ , indice]

    caps <- quantile(column, probs = c(.05, .95), na.rm = T)
    q1   <- quantile(column)[2] # mínimo
    q3   <- quantile(column)[4] # máximo.
    iqr  <- q3 - q1
    
    min <- q1 - (iqr * whis)
    max <- q3 + (iqr * whis)
    
    dataset[ column < min, indice ] <- caps[1]
    dataset[ column > max, indice ] <- caps[2]
    dataset
}




### Deprecated
trainAndPredictOLD <- function(train_set, test_set, trainMethod) {
    set.seed(123)
    
    className <- names(train_set)[ ncol(train_set) ]
    
    # Se entrena el modelo usando los datos discretizados
    clasificador <- train(formula(paste(className, " ~ ." )), # class ~ .
                          data = train_set,
                          method = trainMethod,
                          trControl = trainControl(method = "none"))
    
    # Se separan las propiedades de test y la clase del test (última columna)
    n          <- ncol(test_set)
    x_test_set <- test_set[ , -n ]
    y_test_set <- test_set[[ n ]]
    
    set.seed(123)
    
    # Predicción sobre el modelo entrenado
    y_pred <- predict(clasificador, newdata = x_test_set)
    list( model = clasificador,
          predict = y_pred )
}

# Filtra todos las variables excepto las numéricas.
### Deprecated
selectNumericOLD <- function(dataset) {
    # Se toman solo las columnas numéricas + clase
    num <- (unlist(lapply(dataset, is.numeric)))
    if (sum(num) == 0) {
        stop("No se puede aplicar si no hay campos numéricos")
    } else if (sum(num) == 1) {
        dataset_num <- dataset[ num ]
    } else {
        dataset_num <- dataset[ , num ]
    }
    dataset_num
}


### Deprecated
discretizeOLD <- function(train_set, test_set, discretizer, names) {
    if (containsNumeric(train_set)) {
        className <- names(train_set)[ ncol(train_set) ]
        
        # Se toman solo las columnas numéricas + clase
        train_set_num <- train_set[ names ]
        test_set_num  <- test_set[ names ]
        
        # Se calcula la discretización a partir de los datos de entrenamiento
        discretizer.data <- discretizer(train_set_num)
        
        # Se obtiene la discretización de los datos de entrenamiento
        train_set_num.disc <- discretizer.data$dataset
        
        # Puntos de corte
        cutpoints <- discretizer.data$cutpoints
        
        # Se aplica la discretización YA CALCULADA sobre los datos de test
        test_set_num.disc <- aplicarCutpoints(test_set_num, cutpoints)
        
        # Se actualiza train_set y test_set con los datos discretizados
        train_set <- updateAndFactor(train_set, train_set_num.disc)
        test_set  <- updateAndFactor(test_set, test_set_num.disc)
    }
    list( train = train_set,
          test = test_set )
}

# Filtra todos las variables excepto las numéricas y la clase.
### Deprecated
selectNumericAndClassOLD <- function(dataset) {
    n <- ncol(dataset)
    className <- names(dataset)[ n ]
    
    # Se toman solo las columnas numéricas + clase
    num <- (unlist(lapply(dataset, is.numeric)))
    if (sum(num) == 0) {
        stop("No se puede aplicar si no hay campos numéricos")
    } else if (sum(num) == 1) {
        dataset_num <- dataset[ num ]
        dataset_num[ className ] <- dataset[ , n ]
    } else {
        dataset_num <- dataset[ , num ]
        dataset_num[ className ] <- dataset[ , n ]
    }
    dataset_num
}




imputeOutliers_v2 <- function(trainTest, index, imputeToNA = false, whis = 1.5) {
    train <- trainTest$train
    test  <- trainTest$test
    
    columnaTrain = train[ , index]
    columnaTest = test[ , index]
    
    print( "Cálculo de cuartiles" )
    caps <- quantile(columnaTrain, probs = c(.05, .95), na.rm = T)
    q1   <- quantile(columnaTrain)[2] # mínimo
    q3   <- quantile(columnaTrain)[4] # máximo.
    iqr  <- q3 - q1
    
    min <- q1 - (iqr * whis)
    max <- q3 + (iqr * whis)

    if (imputeToNA) {
        train[ columnaTrain < min, index ] <- NA
        train[ columnaTrain > max, index ] <- NA
        test[ columnaTest < min, index ]   <- NA
        test[ columnaTest > max, index ]   <- NA
    } else {
        train[ columnaTrain < min, index ] <- caps[1]
        train[ columnaTrain > max, index ] <- caps[2]
        test[ columnaTest < min, index ]   <- caps[1]
        test[ columnaTest > max, index ]   <- caps[2]
    }
    
    #outlierTrain = columnaTrain > max | columnaTrain < min
    #outlierTest = columnaTest > max | columnaTest < min
    
    #train[ outlierTrain, index ] <- NA
    #test[ outlierTest, index ] <- NA
    
    list( train = train,
          test = test )
}

outliersMultivariate <- function(dataset, className) {
    numeric <- selectNumeric(dataset)

    # Outliers multivariantes
    mod    <- lm(formula(paste(className, " ~ ." )), data = numeric)
    cooksd <- cooks.distance(mod)
    
    #plot(cooksd, pch="*", cex = 2, main = paste("Influential Obs by Cooks distance (shot_angle)"))
    #abline(h = 4*mean(cooksd, na.rm = TRUE), col = "red")
    #text(x = 1:length(cooksd) + 1, y = cooksd, labels = ifelse(cooksd > 4*mean(cooksd, na.rm = TRUE), names(cooksd),""), col = "red")
    
    influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm = TRUE))])
    numeric[ influential, className ] <- NA
    numeric <- complete( mice(numeric, printFlag = F, seed = 3) )

    update(dataset, numeric)
}


outliersMultivariate_v2 <- function(trainTest, names, className) {
    train <- trainTest$train
    test  <- trainTest$test

    index <- indexOf(train, className)

    numeric.train <- train[ ,which(names(train) %in% names) ]
    numeric.test  <- test[ ,which(names(test) %in% names) ]

    #numeric.train <- selectNumeric(train)
    #numeric.test  <- selectNumeric(test)
    
    # Outliers multivariantes
    print( "cooks train" )
    mod.train    <- lm(formula(paste(className, " ~ ." )), data = numeric.train)
    cooksd.train <- cooks.distance(mod.train)

    print( "cooks test" )
    mod.test    <- lm(formula(paste(className, " ~ ." )), data = numeric.test)
    cooksd.test <- cooks.distance(mod.test)
    
    #plot(cooksd, pch="*", cex = 2, main = paste("Influential Obs by Cooks distance (shot_angle)"))
    #abline(h = 4*mean(cooksd, na.rm = TRUE), col = "red")
    #text(x = 1:length(cooksd) + 1, y = cooksd, labels = ifelse(cooksd > 4*mean(cooksd, na.rm = TRUE), names(cooksd),""), col = "red")

    print( "mean" )
    mean <- mean(cooksd.train, na.rm = TRUE)

    print( "influential.train" )
    influential.train <- as.numeric(names(cooksd.train)[(cooksd.train > mean)])
    numeric.train[ influential.train, index ] <- NA
    View(numeric.train)

    print( "influential.test" )
    influential.test <- as.numeric(names(cooksd.test)[(cooksd.test > mean)])
    numeric.test[ influential.test, index ] <- NA
    
    train <- update(train, numeric.train)
    test  <- update(test, numeric.test)
    
    list( train = train,
          test = test )
}

impute_v2 <- function(trainTest) {
    train <- trainTest$train
    test  <- trainTest$test

    numeric.train <- selectNumeric(train)
    numeric.test  <- selectNumeric(test)

    numeric.train <- complete( mice(numeric.train, printFlag = F, seed = 3) )
    numeric.test  <- complete( mice(numeric.test, printFlag = F, seed = 3) )
    
    train <- update(train, numeric.train)
    test  <- update(test, numeric.test)

    list( train = train,
          test = test )
}

preprocess_v2 <- function(trainTest, method, thresh = 0.95) {
    train <- trainTest$train
    test  <- trainTest$test

    allCero <- function(x) {
        xxx <- all(x == 0)
        !is.na(xxx) && xxx
    }

    # Columnas con todos sus elementos a 0 en train y test
    train.zero <- names(train[ , apply(train, 2, allCero)])
    #train.zero <- names( train[, colSums(train != 0) == 0] )
    #print( train.zero )

    #test.zero  <- names(test[ , apply(test, 2, function(x) all(x == 0))])
    #test.zero  <- names( test[, colSums(test != 0) == 0 ] )
    test.zero  <- names(test[ , apply(test, 2, allCero)])
    #print( test.zero )

    namesToExclude <- c( train.zero, test.zero )
    #print( namesToExclude )
    
    # Se excluyen las columas a 0
    train.aux <- excludeCols( train, namesToExclude )
    test.aux  <- excludeCols( test, namesToExclude )
    
    pp        <- preProcess(train.aux, method = method, thresh = thresh)
    train.aux <- predict(pp, train.aux)
    test.aux  <- predict(pp, test.aux)

    train <- update(train, train.aux)
    test  <- update(test, test.aux)
    
    list( train = train,
          test = test )
}

stepwise <- function(trainTest, className) {
    train <- trainTest$train
    test  <- trainTest$test

    step.model <- train(formula(paste(className, " ~ ." )),
                        data = train,
                        method = "glmStepAIC", 
                        trControl = trainControl(method = "none"),
                        trace = FALSE)    

    # Model accuracy
    step.model$results
    # Final model coefficients
    step.model$finalModel
    # Summary of the model
    summary(step.model$finalModel)
}
