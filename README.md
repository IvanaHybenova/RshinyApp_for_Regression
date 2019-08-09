# RshinyApp_for_Regression
R shiny app for solving regression problems

Repository contains material for R-Ladies Copenhagen Meet-up:April is Shiny (practical part)

Rshiny app is designed to load any cleaned dataset (House_prices.csv is provided as an example), perform feature selection, 
if needed, train and tune different algorithms, save the chosen one and do predictions on unseen data.

Except Rshiny_Regression.R are provided for more R files, which contains functions for feature selection for different algorithms.

Steps:
1. Download all the files.
2. Open Rshiny_Regression.R and set the working directory in the first line of the file.
3. Run the file.
4. Upload the dataset.
5. Move to "Feature selection" tab on both left and write part and write down target variable. Press default for filling in the predictors. If you want to perform
   feature selection, press button with chosen algorithm and copy paste the result into "Predictors:" field.
6. Move to "Model training" tab on the left and "Modeling" on the write part of the app. Set the parameters grid search for algorithm
   of your choice and train it. It is possible to train multiple different algorithms. Choose from the drop down menu one you want to save
   and save it.
7. Move to "Unseen data input" on the left and "Unseen_data" on the right part of the app. Upload the data you want to do the predictions for.
8. Move to "Predicting" tab on the left and "Predictions" tab on the right. Press "Show the model" to double check the saved model. 
   Press "Get the predictions" and wait for the model to run. After pressing "Save the predictions", csv file will be saved in the working
   directory.
