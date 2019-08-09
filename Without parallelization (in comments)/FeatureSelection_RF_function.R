# Feature selection - Random forest

FeatureSelection_RF <- function(target, predictors, method, train_set) {
  
  # Extract the features
  if (grepl(",",predictors)){
    predictors <- as.character(unlist(strsplit(predictors,",")))
  }else{
    predictors <- as.character(unlist(strsplit(predictors," ")))
  }
  
  
  #create a task
  train_sampleTask <- makeRegrTask(data = train_set, target = target)
  train_sampleTask <- subsetTask(train_sampleTask, features = predictors)
  
  # Specify the search strategy
  ctrl = makeFeatSelControlSequential(method = method)
  
  # Select features
  rdesc = makeResampleDesc("CV", iters = 10)
  
  #create a learner
  regressor_RF <- makeLearner("regr.randomForest",  predict.type = "response")
  
  #parallelStartSocket(3)
  output <- selectFeatures(learner = regressor_RF, task = train_sampleTask, resampling = rdesc, 
                 control = ctrl, show.info = FALSE)
  #parallelStop()
  output$x
}

save(FeatureSelection_RF, file = "FeatureSelection_RF.rda")
