setwd("")

# install.packages("shiny", dependencies = TRUE)
# install.packages("ggplot2", dependencies = TRUE)
# install.packages("shinythemes", dependencies = TRUE)
# install.packages("DT", dependencies = TRUE)
# install.packages("xlsx", dependencies = TRUE)
# install.packages("caTools", dependencies = TRUE)
# install.packages("Metrics", dependencies = TRUE)
# install.packages("mlr", dependencies = TRUE)
# install.packages("e1071", dependencies = TRUE)
# install.packages("gbm", dependencies = TRUE)
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("randomForest", dependencies = TRUE)
# install.packages("parallelMap", dependencies = TRUE)
# install.packages("caret", dependencies = TRUE)

library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)
library(xlsx)
library(caTools)
library(Metrics)
library(mlr)
library(e1071)
library(gbm)
library(tidyverse)
library(randomForest)
# library("parallelMap")
library(tidyr)
library(caret)



model_options <- list("Linear regression" = "lr",
                      "Random forest" = "rf",
                      "SVM for regression" = "svr",
                      "Gradient Boosted Regression Trees" = "gbm"
)

method_options <- list("Sequential Floating Forward Search" = "sffs",
                       "Sequential Floating Backward Search" = "sfbs")

ui <- fluidPage(theme = shinytheme("flatly"),
                
                titlePanel("Model"),
                
                tags$head(
                  tags$style(HTML("hr {border-top: 2px solid #3CB371;}"))
                ),
                
                # sidebar layout with the inputs and the outputs definitions
                sidebarLayout(
                  # Inputs definitions
                  sidebarPanel( 
                    tabsetPanel(
                      
                      # Data input panel
                      tabPanel("Data input",
                               # Horizontal line ----
                               tags$hr(),
                               # Input: Select a file ----
                               fileInput("file1", "Choose csv data file",
                                           multiple = TRUE,
                                           accept = c("csv")),
                                     
                               # empty line
                               tags$br(),
                                          
                               # Input: Checkbox if file has header ----
                               checkboxInput("header", "Header", TRUE),
                                          
                               # Input: Select separator ----
                               radioButtons("sep", "
                                            Separator",
                                              choices = c(Comma = ",",
                                                           Semicolon = ";",
                                                           Tab = "\t"),
                                              selected = ","),
                               tags$br(),

                               # Input: Select quotes ----
                               radioButtons("quote", "Quote",
                                              choices = c(None = "",
                                                          "Double Quote" = '"',
                                                          "Single Quote" = "'"),
                                              selected = '"'),
                                          
                               tags$br(),
                                          
                               # Input: Select number of rows to display ----
                               radioButtons("disp", "Display",
                                             choices = c(Head = "head",
                                                         Tail = "tail",
                                                         All = "all"),
                                             selected = "all")
                                          
                                          
                                 ),
                      
                      # Feature selection
                      tabPanel("Feature selection",
                               # Horizontal line ----
                               tags$hr(),
                               textInput('target', 'Target variable'),
                               textInput('predictors', 'Predictors:'),
                               actionButton("set_default_predictors", "Default"),
                               tags$hr(),
                               # Choosing the method for feature selection
                               selectInput("method", label = "Choose the method",
                                           choices = method_options,
                                           selected = "ssfs"),
                               actionButton("fs_lr", "Linear regression"),
                               actionButton("fs_rf", "Random forest"),
                               actionButton("fs_svr", "SVM regression"),
                               tags$br(),
                               actionButton("fs_gbm", "Gradient boosted trees")),
                      
                      # Model training panel
                      tabPanel("Model training",
                               # Horizontal line ----
                               tags$hr(),

                               # action buttons to run the traiining
                               actionButton("train_lr", "Train LR"),
                               actionButton("train_rf", "Train RF"),
                               actionButton("train_svr", "Train SVR"),
                               actionButton("train_gbm", "Train GBM"),
                               
                               # Saving the model
                               selectInput("best_model", label = "Choose the best model",
                                           choices = model_options,
                                           selected = "rf"),
                               actionButton("save_model", "Save chosen model"),
                               
                               # Parameters tuning set-up
                               tags$hr(),
                               HTML(paste("Enter the grid search for parameters for RF: ")),
                               textInput('ntree_gs', 'Number of trees', "1000"),
                               textInput('nodesize_gs', 'Size of the nodes (5)', "10, 30, 50"),
                               textInput('mtry_gs', 'Number of candidates at each split (p/3)', "2, 3"),
                               actionButton("set_default_rf", "Default"),
                               tags$hr(),
                               
                               HTML(paste("Enter the grid search for parameters for SVR: ")),
                               textInput('epsilon_gs', 'Epsilon', "0.1, 0.2, 0.3 ,0.4, 0.5 ,0.6, 0.7, 0.8, 0.9, 1"),
                               textInput('cost_gs', 'Cost', "10, 20, 30, 40, 50, 60, 70, 80, 90, 100"),
                               actionButton("set_default_svr", "Default"),
                               
                               tags$hr(),
                               HTML(paste("Enter the grid search for parameters for GBM: ")),
                               textInput('interaction.depth_gs', 'Highest level of variable interactions allowed', "1,3,5,7"),
                               textInput('n.trees_gs', 'Number of trees', "100, 300, 500, 700, 1000"),
                               textInput('shrinkage_gs', 'Learning rate (0.001-0.1)', "0.009, 0.03, 0.05, 0.07, 0.08, 0.1"),
                               textInput('n.minobsinnode_gs', 'Minimum number of observations in the terminal nodes (10)', "5, 10, 15, 20"),
                               actionButton("set_default_gbm", "Default")),
                      
                     
                      tabPanel("Unseen data input",
                               # Horizontal line ----
                               tags$hr(),
                               # Input: Select a file ----
                               fileInput("file2", "Choose csv data file",
                                         multiple = TRUE,
                                         accept = c("csv")),
                               
                               # Horizontal line ----
                               tags$br(),
                               
                               # Input: Checkbox if file has header ----
                               checkboxInput("header2", "Header", TRUE),
                               
                               # Input: Select separator ----
                               radioButtons("sep2", "Separator",
                                            choices = c(Comma = ",",
                                                        Semicolon = ";",
                                                        Tab = "\t"),
                                            selected = ","),
                               
                               tags$br(),
                               
                               # Input: Select quotes ----
                               radioButtons("quote2", "Quote",
                                            choices = c(None = "",
                                                        "Double Quote" = '"',
                                                        "Single Quote" = "'"),
                                            selected = '"'),
                               
                               # Horizontal line ----
                               tags$br(),
                               
                               # Input: Select number of rows to display ----
                               radioButtons("disp2", "Display",
                                            choices = c(Head = "head",
                                                        Tail = "tail",
                                                        All = "all"),
                                            selected = "all")),
                      
                      tabPanel("Predicting",
                               # Horizontal line ----
                               tags$hr(),
                                actionButton('show_model', 'Show the model', style='padding:4px; font-size:95%'), 
                                actionButton('Get_Predictions', 'Get the predictions', style='padding:4px; font-size:95%'),
                                actionButton('Save_Predictions', 'Save the predictions', style='padding:4px; font-size:95%')
                                 
                                 
                                 
                               ))
                    ),
                  
                  # Output definitions
                  mainPanel(
                    
                    tabsetPanel(
                      # Loaded Data
                      tabPanel("Data", dataTableOutput("data"),
                               verbatimTextOutput("data_summary")),
                      
                      # Feature selection
                      tabPanel("Feature selection",
                               HTML(paste("Predictors for Linear Regression:")),
                               verbatimTextOutput("lr_feature_selection"),
                               HTML(paste("Predictors for Random Forest:")),
                               verbatimTextOutput("rf_feature_selection"),
                               HTML(paste("Predictors for SVM for regression:")),
                               verbatimTextOutput("svr_feature_selection"),
                               HTML(paste("Predictors for Gradient Boosted Regression Trees:")),
                               verbatimTextOutput("gb_feature_selection")
                               ),
                               
                      # Model training output
                      tabPanel("Modeling",
                               HTML(paste("LR related outputs: ")),
                               verbatimTextOutput("lr_mae_train"),
                               verbatimTextOutput("lr_mae"),
                               verbatimTextOutput("Best_params_lr"),
                               HTML(paste("RF related outputs: ")),
                               verbatimTextOutput("rf_mae_train"),
                               verbatimTextOutput("rf_mae"),
                               verbatimTextOutput("Best_params_rf"),
                               HTML(paste("SVM for regression related outputs: ")),
                               verbatimTextOutput("svr_mae_train"),
                               verbatimTextOutput("svr_mae"),
                               verbatimTextOutput("Best_params_svr"),
                               HTML(paste("GBM related outputs: ")),
                               verbatimTextOutput("gbm_mae_train"),
                               verbatimTextOutput("gbm_mae"),
                               verbatimTextOutput("Best_params_gbm")),
                      
                      # Unseen data output
                      tabPanel("Unseen_data",
                               dataTableOutput("unseen_data")),
                      
                      # Predicting output
                      tabPanel("Predictions",
                               verbatimTextOutput("model"),
                               dataTableOutput("predictions"))

                  )
                ) 
))


# Define server function 
server <- function(input, output, session) {
  set.seed(1234)

  # Loading the data for modeling
  dataset <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                    header = input$header,
                    quote = input$quote,
                    sep = input$sep)
    df
    
  })
  
  # Displaying the data for modeling
  output$data <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                    header = input$header,
                    quote = input$quote,
                    sep = input$sep)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else if(input$disp == "tail") {
      return(tail(df))
    }
    else{df}
    
  })
  
  # Displaying the summary of the data for modeling
  output$data_summary <- renderPrint({
    req(dataset())
    print(summary(dataset()))
    
  })
  
  
  

   # Data modeling - part 1
  ###############################################################################################
  
  # Updating predictors list
  observeEvent(input$set_default_predictors,{
    req(dataset())

    dataset <- dataset()
    target <- input$target
    
    data <- dataset [, !names(dataset) %in% target, drop = FALSE]
    updateTextInput(session, "predictors", value=colnames(data))
  })
  
  # Creating the training set
  train_set <- reactive({
    req(dataset())
    
    dataset <- dataset()
    
    set.seed(123)
    split = sample.split(dataset[,length(colnames(dataset))], SplitRatio = 0.80)
    train_set = subset(dataset, split == TRUE)
    test_set = subset(dataset, split == FALSE)
    rm(split)
    
    print("train set created")
    train_set
  })
  
  # Creating the test set
  test_set <- reactive({

    dataset <- dataset()
    
    print("Creating test set...")
    set.seed(123)
    split = sample.split(dataset[,input$target], SplitRatio = 0.80)
    train_set = subset(dataset, split == TRUE)
    test_set = subset(dataset, split == FALSE)
    rm(split)
    
    print("test set created")
    test_set
  })
  

  
  # Creating formula for the algorithms
  formula <- reactive({
    target <- input$target

    if (grepl(",", input$predictors)){
      predictors <- as.character(unlist(strsplit(input$predictors,",")))
    }else{
      predictors <- as.character(unlist(strsplit(input$predictors," ")))
    }
    
    
    formula <- as.formula(
      paste(target,
            paste(predictors, collapse = " + "),
            sep = " ~ "))
    print(formula)
    formula
  })
  

  # Feature selection for Linear regression
  observeEvent(input$fs_lr, {
    req(train_set())
    train_set <- train_set()
    load(file = "FeatureSelection_LR.rda")
    
    output$lr_feature_selection <- renderText({
      print("LR feature selection is running")
      FeatureSelection_LR(isolate(input$target), isolate(input$predictors), isolate(input$method), train_set)
    })
  })
  
  # Feature selection for SVR
  observeEvent(input$fs_svr, {
    req(train_set())
    train_set <- train_set()
    load(file = "FeatureSelection_SVR.rda")

    output$svr_feature_selection <- renderText({
      print("SVR feature selection is running")
      FeatureSelection_SVR(isolate(input$target), isolate(input$predictors), isolate(input$method), train_set)
    })
  })
  
  # Feature selection for Random forest
  observeEvent(input$fs_rf, {
    req(train_set())
    train_set <- train_set()
    load(file = "FeatureSelection_RF.rda")
    
    output$rf_feature_selection <- renderText({
      print("RF feature selection is running")
      FeatureSelection_RF(isolate(input$target), isolate(input$predictors), isolate(input$method), train_set)
    })
  })
  
  # Feature selection for GBM
  observeEvent(input$fs_gbm, {
    req(train_set())
    train_set <- train_set()
    load(file = "FeatureSelection_GBM.rda")

    output$gb_feature_selection <- renderText({
      print("GBM feature selection is running")
      FeatureSelection_GBM(isolate(input$target), isolate(input$predictors), isolate(input$method), train_set)
    })
  })

  
  # Data modeling - part 2
  ###############################################################################################
  

  # LR
  BstModelLR <- eventReactive(input$train_lr,{
    req(train_set())
    req(formula())
    
    train_set <- train_set()
    formula <- formula()
    
    print("Training LR...")
    #Tune the LR model
    set.seed(1234)

    # parallelStartSocket(3)
  
    BstModelLR=caret::train(formula, data=train_set, method = 'lm')
    
    # parallelStop()
    BstModelLR
  })
  
  output$lr_mae_train <- renderPrint({
    req(BstModelLR())
    BstModelLR <- BstModelLR()
    print(BstModelLR$results)
  })

  output$lr_mae <- renderText({
    req(BstModelLR())
    req(test_set())
    
    BstModelLR <- BstModelLR()
    test_set <- test_set()
    
    
    PredYBstLR=predict(BstModelLR,test_set[,-which(names(test_set) == input$target)])
    mae = mae(PredYBstLR,test_set[,input$target])

    print(paste("Lnear regression mean absolute error on test set: ", mae))
  })
  
  # SVR
  OptModelSVR <- eventReactive(input$train_svr,{
    req(train_set())
    req(formula())
    
    train_set <- train_set()
    formula <- formula()
    
    print("Training SVR...")
    #Tune the SVM model
    set.seed(1234)
    epsilon_gs <- as.numeric(unlist(strsplit(isolate(input$epsilon_gs),",")))
    cost_gs <-  as.numeric(unlist(strsplit(isolate(input$cost_gs),",")))
    ranges <- list(epsilon=epsilon_gs, cost=cost_gs)
   # parallelStartSocket(3)
    
    OptModelSVR=tune(svm, formula, data=train_set,ranges=ranges)
    
   # parallelStop()
    OptModelSVR
  })

  # Find the best model
  BstModelSVR <- reactive({
    req(OptModelSVR())
    
    OptModelSVR <- OptModelSVR()
    
    BstModelSVR=OptModelSVR$best.model
    BstModelSVR
  })
  
  output$Best_params_svr <- renderPrint({
    req(BstModelSVR())
    BstModelSVR <- BstModelSVR()
    BstModelSVR

  })
  output$svr_mae_train <- renderPrint({
    req(OptModelSVR())
    OptModelSVR <- OptModelSVR()
    
    OptModelSVR$best.performance
  })
  
  output$svr_mae <- renderText({
    req(BstModelSVR())
    req(test_set())
    
    test_set <- test_set()
    BstModelSVR <- BstModelSVR()
    
    PredYBstSVR=predict(BstModelSVR,test_set[,-which(names(test_set) == input$target)])
    mae = round(mae(PredYBstSVR,test_set[,input$target]),2)
    mse = round(mse(PredYBstSVR,test_set[,input$target]),0)
    print(paste("SVM regression mean absolute error on test set: ", mae, "SVM regression mean absolute error on test set: ", mse))
  })
  

  # GBM
  BstModelGBM <- eventReactive(input$train_gbm,{
    req(train_set())
    req(formula())
    
    print("Training GBM...")
    train_set <- train_set()
    formula <- formula()
    
    #Tune the GBM model
    interaction.depth_gs <- as.numeric(unlist(strsplit(isolate(input$interaction.depth_gs),",")))
    n.trees_gs <- as.numeric(unlist(strsplit(isolate(input$n.trees_gs),",")))
    shrinkage_gs <- as.numeric(unlist(strsplit(isolate(input$shrinkage_gs),",")))
    n.minobsinnode_gs <- as.numeric(unlist(strsplit(isolate(input$n.minobsinnode_gs),",")))
    
    # parallelStartSocket(3)
    
    gbmGrid <- expand.grid(.interaction.depth = interaction.depth_gs,
                           .n.trees = n.trees_gs,
                           .shrinkage = shrinkage_gs,
                           .n.minobsinnode = n.minobsinnode_gs)
    
    set.seed(1234)
    gbmTune <- caret::train(formula, data = train_set,      
                            method = "gbm",
                            distribution = "gaussian",
                            tuneGrid = gbmGrid,
                            verbose = FALSE)
    
    # parallelStop()
    gbmTune
  })
  
  output$Best_params_gbm <- renderPrint({
    req(BstModelGBM())
    BstModelGBM <- BstModelGBM()
    print(BstModelGBM)
  })
  
  output$gbm_mae_train <- renderPrint({
    req(BstModelGBM())
    BstModelGBM <- BstModelGBM()
    print(BstModelGBM$results)
  })
  
  output$gbm_mae <- renderText({
    req(BstModelGBM())
    req(test_set())
    
    test_set <- test_set()
    BstModelGBM <- BstModelGBM()
    
    PredYBstGBM <- predict(BstModelGBM,test_set[,-which(names(test_set) == input$target)])
    mae = mae(PredYBstGBM,test_set[,input$target])
    print(paste("GBM regression mean absolute error on test set: ", mae))
  })


  
  
  ## Tuning RF
  
  BstModelRF <- eventReactive(input$train_rf,{
    req(train_set())
    train_set <- train_set()

  #create a task
    train_sampleTask <- makeRegrTask(data = train_set, target = input$target)
    if (grepl(",", input$predictors)){
      features <- as.character(unlist(strsplit(input$predictors,",")))
    }else{
      features <- as.character(unlist(strsplit(input$predictors," ")))
    }
    
    
    train_sampleTask <- subsetTask(train_sampleTask, features = features)

    #create a learner
    regressor_RF <- makeLearner("regr.randomForest", predict.type = "response")
    regressor_RF$par.vals <- list(
      importance = FALSE
    )
    
    #set tunable parameters
    #grid search to find hyperparameters
    ntree_gs <- as.numeric(unlist(strsplit(isolate(input$ntree_gs),",")))
    nodesize_gs <- as.numeric(unlist(strsplit(isolate(input$nodesize_gs),",")))
    mtry_gs <- as.numeric(unlist(strsplit(isolate(input$mtry_gs),",")))
    
    regressor_RF_param <- makeParamSet(
      makeDiscreteParam("ntree", values = ntree_gs),
      makeDiscreteParam("mtry", values = mtry_gs),
      makeDiscreteParam("nodesize", values = nodesize_gs)
    )
    
    #doing a grid search
    gscontrol <- makeTuneControlGrid()
    
    #set 5 fold cross validation
    set_cv <- makeResampleDesc('CV',iters = 5L)
    
    # parallelStartSocket(3)
    
    # hyperparameters tuning
    regressor_RF_tuned <- tuneParams(learner = regressor_RF, resampling = set_cv, task = train_sampleTask, par.set = regressor_RF_param, control = gscontrol)
    
    # parallelStop()
    
    
    # Apply optimal parameters to model
    BstModelRF <- setHyperPars(learner = regressor_RF,
                               par.vals = regressor_RF_tuned$x)
    
    # Train final model with tuned parameters
    BstModelRF <- mlr::train(learner = BstModelRF,task = train_sampleTask)
    
    BstModelRF
  })
  
  output$Best_params_rf <- renderPrint({
    req(BstModelRF())
    BstModelRF <- BstModelRF()
    print(BstModelRF)
  })
  
  output$rf_mae_train <- renderPrint({
    req(BstModelRF())
    
    BstModelRF <- BstModelRF()
    print(BstModelRF$learner.model)
    BstModelRF$learner.model
  })

  output$rf_mae <- renderText({
    req(BstModelRF())
    
    BstModelRF <- BstModelRF()
    test_set <- test_set()
    
    PredYBstRF=predict(BstModelRF,newdata = test_set[,-which(names(test_set) == input$target)])
    PredYBstRF = PredYBstRF$data$response
    mae = mae(PredYBstRF,test_set[,input$target])
    mse = mse(PredYBstRF,test_set[,input$target])
    print(paste("Random forest regression mean absolute error on test set: ", mae, "Random forest regression mean square error on test set: ", mse))
  })
  



  # # Setting default parameters
  observeEvent(input$set_default_rf,{  
    print("Setting RF to default")
    updateTextInput(session, "ntree_gs", value="1000")
    updateTextInput(session, "nodesize_gs", value = "10,30,50")
    updateTextInput(session, "mtry_gs", value = "2,3")
    print("Setting RF to default - finished")
  })
  
  observeEvent(input$set_default_svr,{  
    print("Setting SVR to default")
    updateTextInput(session, "epsilon_gs", value="0.1, 0.2, 0.3 ,0.4, 0.5 ,0.6, 0.7, 0.8, 0.9, 1")
    updateTextInput(session, "cost_gs", value = "10, 20, 30, 40, 50, 60, 70, 80, 90, 100")
    print("Setting SVR to default - finished")
  })
  
  observeEvent(input$set_default_gbm,{  
    print("Setting GBM to default")
    updateTextInput(session, "interaction.depth_gs", value="1,3,5,7")
    updateTextInput(session, "n.trees_gs", value = "100, 300, 500, 700, 1000")
    updateTextInput(session, "shrinkage_gs", value = "0.009, 0.03, 0.05, 0.07, 0.08, 0.1")
    updateTextInput(session, "n.minobsinnode_gs", value = "5, 10, 15, 20")
    print("Setting GBM to default - finished")
  })
  

  # Saving the model
  ###############################################################################################
  observeEvent(input$save_model,{
    
    train_set <- train_set()
    if(input$best_model == 'lr'){
      shiny::validate(
        need(BstModelLR(), "Linear regression hasn't been trained yet."))
      print("Saving linear regression...")
      regressor <- BstModelLR()
      save(regressor, file = 'regressor.rda')
      print("Linear regression saved.")
      
    }else if(input$best_model == "rf") {
      shiny::validate(
        need(BstModelRF(), "Random forest hasn't been trained yet."))
      print("Saving Random forest regressor...")
      
      regressor <- BstModelRF()
      save(regressor, file = 'regressor.rda')
      print("Random forest regressor saved.")
    } else if (input$best_model == "gbm") {
      shiny::validate(
        need(BstModelGBM(), "GBM hasn't been trained yet."))
      print("Saving GBM regressor...")
      regressor <- BstModelGBM()
      save(regressor, file = 'regressor.rda')
      print("GBM regressor saved.")
    } else if (input$best_model == "svr"){
      shiny::validate(
        need(BstModelSVR(), "SVM regressor hasn't been trained yet."))
      print("Saving SVM regressor...")
      regressor <- BstModelSVR()
      save(regressor, file = 'regressor.rda')
      print("SVM regressor saved.")
    }
    
  })
  
  # Displaying data for predicting
  output$unseen_data <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file2)
    
    df <- read.csv(input$file2$datapath,
                   header = input$header2,
                   quote = input$quote2,
                   sep = input$sep2)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else if(input$disp == "tail") {
      return(tail(df))
    }
    else{df}
    
  })
  
  # Loading the data for predicting
  unseen_data <- reactive({
    req(input$file2)
    df <- read.csv(input$file2$datapath,
                   header = input$header2,
                   quote = input$quote2,
                   sep = input$sep2)
    df
    
  })
  
  # Displaying the model
  output$model <- renderPrint({
    req(input$show_model)
    print("Displaying regressor")
    load(file = 'regressor.rda', .GlobalEnv)
    regressor
  })
  
  # Getting the predictions
  predictions <- eventReactive(input$Get_Predictions, {
    req(unseen_data())
    unseen_data <- unseen_data()
    
    
    print("Loading the model...")
    load(file = 'regressor.rda', .GlobalEnv)
    print("Running the model...")
    predictions <- predict(regressor, newdata = unseen_data)
    
    # Workaround for random forest model
    if(class(predictions) != "numeric"){
      predictions <- predictions$data$response
    }
    
    predictions <- round(predictions, 2)
    print("Running the model - finished")
    predicted_data <- cbind.data.frame(unseen_data, predictions)
    predicted_data
  })

 # Displaying predictions
  output$predictions <- DT::renderDataTable({
       req(predictions())
       predictions()
  })

  # Saving the predictions
   observeEvent(input$Save_Predictions,{  
      req(predictions())
      print("Saving the predictions")
      predictions <- predictions()
      write.csv(predictions, "predictions.csv", row.names = FALSE)
      print("Predictions are saved")
    })
  
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
