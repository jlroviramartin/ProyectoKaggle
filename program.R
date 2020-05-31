source("external.R")
source("prepare-data.R")

data.filtered <- exclude( dataTransformation( prepare( data.fullRecords ) ) )

tmp <- data.frame(shot_distance = data.filtered$shot_distance,
                  shot_angle = data.filtered$shot_angle,
                  remaining = data.filtered$remaining,
                  shot_made_flag = data.filtered$shot_made_flag)
discretized <- mdlp(tmp)

data.filtered$shot_distance <- (discretized$Disc.data)$shot_distance
data.filtered$shot_angle    <- (discretized$Disc.data)$shot_angle
data.filtered$remaining     <- (discretized$Disc.data)$remaining
data.filtered$period        <- (discretized$Disc.data)$period
data.filtered$season        <- (discretized$Disc.data)$season

result <- split(data.filtered, p = 0.75,
                nextStep = function(train_fold, test_fold) {
                    
                    preprocessAll(train_fold, test_fold, method = c( "center", "scale" ),
                                  nextStep = function(train_fold.pre, test_fold.pre) {
                                      
                                      discretize(train_fold.pre, test_fold.pre, discretizer = mdlpDiscretizer,
                                                 nextStep = function(train_fold.disc, test_fold.disc) {
                                                     
                                                     trainAndTest(train_fold.disc, test_fold.disc, "mlp")
                                                 })
                                  })
                })
result$accuracy