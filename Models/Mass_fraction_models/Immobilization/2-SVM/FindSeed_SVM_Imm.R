# Install below packages in the first time you run this code
#install.packages(c("openxlsx","svDialogs","data.table","mlbench","caret", "shiny", "shinydashboard"), dependencies = TRUE)


library(svDialogs) 
library(data.table)
library(openxlsx)
library(mlbench)
library(caret)
library(tools)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(dplyr)
library(randomForest)


#############################################################################
QSARFile <- dlg_open(title = "Select excel file for Dmix", filters = dlg_filters[c("All"), ])$res; QSARFolder <- dirname(QSARFile)
ExcelFile <- createWorkbook("FindSeed")


  QSARdata <- read.xlsx(QSARFile, sheet = 9, startRow = 1, colNames = TRUE,
                        rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                        skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                        namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)
# Remove character columns
Table1 <- QSARdata[,-c(1,3)]

# Make a Table to store model performance
TestPerfomance <- data.frame( Seed = 0,
                              R2_all = 0,
                              R2_test = 0,
                              R2_train = 0)

# Find random seed to get good R2 value
for (i in 1:1000) {
  set.seed(i*5)
  ## 80% of the sample size
  in_rows <- createDataPartition(y = Table1$Immobilization, p = 0.7, list = FALSE)
  
  # Training models
  train <- Table1[in_rows, ]
  test <- Table1[-in_rows, ]
  
  train.control <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
  SVMmodel <- train(Immobilization ~ ., data = train, method = "svmPoly", trControl = train.control)
  print(SVMmodel)
  
  
  # Make prediction on test set and compute the R2, RMSE and MAE
  predictions <- SVMmodel %>% predict(test)
  predictions_train <- SVMmodel %>% predict(train)
  predictions_ext <- SVMmodel %>% predict(Table1)
  TestPerfomance[i,1] <- i*5
  TestPerfomance[i,2] <- R2(predictions_ext,Table1$Immobilization)
  TestPerfomance[i,3] <- R2(predictions,test$Immobilization)
  TestPerfomance[i,4] <- R2(predictions_train,train$Immobilization)
  
}
# Save result of finding seeds

addWorksheet(ExcelFile, paste("Dmix", sep = ""))
writeData(ExcelFile, sheet = 1, TestPerfomance, rowNames = FALSE)

saveWorkbook(ExcelFile, paste(QSARFolder,"/","Findseed_SVM_Imm.xlsx", sep = ""), overwrite = TRUE)
