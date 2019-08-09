# Feature selection - GBM

FeatureSelection_GBM <- function(target, predictors, method, train_set) {
  
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
  regressor <- makeLearner("regr.gbm",  predict.type = "response")
  
  #parallelStartSocket(3)
  output <- selectFeatures(learner = regressor, task = train_sampleTask, resampling = rdesc, 
                 control = ctrl, show.info = FALSE)
  #parallelStop()
  output$x
}

save(FeatureSelection_GBM, file = "FeatureSelection_GBM.rda")
