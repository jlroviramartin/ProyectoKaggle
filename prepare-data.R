source("external.R")

library("dplyr")

prepare <- function(data)  {
    # Se eliminan:
    #   shot_id: identificador de disparo
    #   team_id: es siempre un número 1610612747
    #   team_name: siempre Los Angeles Lakers
    #   lat y lon: correladas con loc_x y loc_y
    #   game_id y game_event_id: independientes
    data <- subset(data, select = -c( shot_id, team_id, game_event_id, game_id, team_name, lat, lon ))

    # Se convierte el atributo clase shot_made_flag en un Factor
    ###data$shot_made_flag <- factor(data$shot_made_flag)

    # Se ordena: shot_zone_range
    data$shot_zone_range <- ordered(data$shot_zone_range, levels = c("Less Than 8 ft.", "8-16 ft.", "16-24 ft.", "24+ ft.", "Back Court Shot" ))

    # Se ordena: season y game_date
    data$season    <- ordered(data$season, levels = levels(data$season))
    #data$game_date <- as.ordered(data$game_date, levels = levels(data$game_date))
    data$game_date <- as.Date(data$game_date)

    # Se convierte en factores: period y playoffs
    data$period   <- ordered(data$period)
    data$playoffs <- factor(data$playoffs)

    # Se limpia los nombre de los factores
    levels(data$action_type)        <- filterFactorName( data$action_type )
    levels(data$combined_shot_type) <- filterFactorName( data$combined_shot_type )
    levels(data$season)             <- filterFactorName( data$season )
    levels(data$shot_type)          <- filterFactorName( data$shot_type )
    levels(data$shot_zone_area)     <- filterFactorName( data$shot_zone_area )
    levels(data$shot_zone_basic)    <- filterFactorName( data$shot_zone_basic )
    levels(data$shot_zone_range)    <- filterFactorName( data$shot_zone_range )
    levels(data$opponent)           <- filterFactorName( data$opponent )

    # Se mueve la clase a la última columna
    data <- moveToLast(data, "shot_made_flag")
    data
}

dataTransformation <- function(data) {
    data <- data %>%
        mutate(
            remaining = minutes_remaining * 60 + seconds_remaining,
            
            # An "@" symbol in the matchup variable implies it is an away game.
            home = factor(ifelse(!grepl("@", as.character(matchup)), 1, 0)),
            
            shot_distance = floor(sqrt(data$loc_x**2 + data$loc_y**2)),
            
            shot_angle = atan2(data$loc_x, data$loc_y),
            
            season = ordered(as.numeric(substring(data$season, 1, 4)))
        ) %>%
        subset(select = -c( matchup, minutes_remaining, seconds_remaining))

    # Se mueve la clase a la última columna
    data <- moveToLast(data, "shot_made_flag")
    data
}

norm <- function(data) {
    maxRemaining <- 12*60
    maxDistance  <- round(sqrt(250**2 + 890**2))
    maxAngle     <- 2 * pi

    data$remaining     <- normalize(data$remaining, 0, maxRemaining)
    data$shot_distance <- normalize(data$shot_distance, 0, maxDistance)
    #data$shot_angle    <- normalize(data$shot_angle, -pi, pi)
    data$shot_angle    <- normalizeInCero(data$shot_angle, -pi, pi)

    data
}

exclude <- function(data) {
    # Las columnas minutes_remaining, seconds_remaining se agrupan en remaining
    # Se eliminan las columnas game_date, loc_x, loc_y, game_event_id, game_id, minutes_remaining, seconds_remaining
    #####data <- data %>%
    #####    subset(select = -c(game_date, playoffs, loc_x, loc_y, combined_shot_type, shot_type,
    #####                       shot_distance, shot_distance_range, shot_angle, period))

    data <- data %>%
        dplyr::select(action_type, shot_zone_basic, shot_zone_area, combined_shot_type,
               period, remaining, season,
               shot_angle, shot_distance,
               home, playoffs, opponent,
               loc_x, loc_y, shot_made_flag)
    
    # Se pone la clase al final
    data <- moveToLast( data, "shot_made_flag" )
    data
}

# Se preparan todos los datos
prepareFinal <- function(data) {
    mdlpDiscretizer <- function(dataset) {
        set.seed(123)
        ret <- mdlp(dataset)
        list( dataset = ret$Disc.data, cutpoints = ret$cutp )
    }

    data.prepared <- norm(exclude(dataTransformation(prepare( data ))))

    # Localización normalizada: NO REDUCE <-----
    #data.prepared$loc_x <- normalizeInCero(data.prepared$loc_x, -250, 250 )
    #data.prepared$loc_y <- normalizeInCero(data.prepared$loc_y, -50, 800 )

    # Indica si está en frente de la canasta.
    #data.prepared$inFrontOf <-as.factor(ifelse( data.prepared$loc_y >= 0, "X1", "X0" )) NO REDUCE <-----

    data.prepared$two_points <- ifelse(data.prepared$shot_zone_basic == "Mid_Range"
                                       || data.prepared$shot_zone_basic == "In_The_Paint_Non_RA"
                                       || data.prepared$shot_zone_basic == "Restricted_Area", 0, 1)
    
    # Se normaliza el periodo a [0, 1]
    data.prepared$period <- normalize(as.numeric(as.character(data.prepared$period)), 1, 7)

    data.prepared$season <- as.numeric(as.character(data.prepared$season))
    data.prepared$home   <- as.numeric(as.character(data.prepared$home))

    # Se añade un cluster
    set.seed(123);
    km_clusters <- data.prepared %>%
        dplyr::select(loc_x, loc_y) %>%
        kmeans(iter.max = 20, centers = 8, nstart = 50)
    data.prepared <- data.frame(cluster = as.factor(km_clusters$cluster), data.prepared)

    # Se crean una codificación OneShot mediante variables dummy
    dv  <- dummyVars(~ action_type
                     + combined_shot_type # REDUCE <-----
                     + cluster
                     + opponent,
                     data = data.prepared)
    data.prepared <- data.frame(predict(dv, newdata = data.prepared), data.prepared) %>%
        dplyr::select(-action_type,
                      -combined_shot_type, # REDUCE <-----
                      -cluster,
                      -shot_zone_basic,
                      -shot_zone_area,
                      -opponent,
                      -playoffs,
                      -shot_angle)

    # Se mueve shot_made_flag a la última posicion
    data.prepared <- moveToLast(data.prepared, "shot_made_flag")

    trainTest <- list(train = data.prepared[ complete.cases(data.prepared), ],
                      test  = data.prepared[ !complete.cases(data.prepared), ])

    # NO REDUCE <-----    
    #trainTest <- imputeOutliers_v2(trainTest, indexOf(trainTest$train, "shot_angle" ),    imputeToNA = FALSE)
    #trainTest <- imputeOutliers_v2(trainTest, indexOf(trainTest$train, "shot_distance" ), imputeToNA = FALSE)
    #trainTest <- imputeOutliers_v2(trainTest, indexOf(trainTest$train, "remaining" ),     imputeToNA = FALSE)

    #trainTest <- outliersMultivariate_v2(trainTest, c( "shot_angle", "shot_distance", "remaining", "loc_x", "loc_y" ), "shot_angle")

    # Se discretiza
    set.seed(123);
    trainTest <- discretize_v2(trainTest,
                               discretizer = mdlpDiscretizer,
                               names = c( "shot_angle", "shot_distance", "remaining", "loc_x", "loc_y", "shot_made_flag" ))

    trainTest
}

