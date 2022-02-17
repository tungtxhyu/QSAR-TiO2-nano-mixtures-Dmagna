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

#-------------------------------------------------------------------------------
# Read excel data file 
QSARFile <- dlg_open(title = "Select excel file for Dmix", filters = dlg_filters[c("All"), ])$res
QSARFolder <- dirname(QSARFile)

SeedTableFile <- dlg_open(title = "Select excel file for Seed table", filters = dlg_filters[c("All"), ])$res
SeedTable <- read.xlsx(SeedTableFile, sheet = 1, startRow = 1, colNames = TRUE,
                       rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                       skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                       namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)
Savefolder <- dlg_dir(title = "Select folder to save data output:", filters = dlg_filters[c("All"), ])$res
#-------------------------------------------------------------------------------
# Building models
for (i in c(1:9)) {
  Table1 <- read.xlsx(QSARFile, sheet = i, startRow = 1, colNames = TRUE,
                      rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                      skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                      namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

  Table2 <- Table1[, -c(1,3)]

  TestPerfomance <- data.frame( Seed = 0,
                                R2_test = 0,
                                R2_CV = 0,
                                R2_train = 0,
                                RMSE_test = 0,
                                RMSE_CV = 0,
                                RMSE_train = 0,
                                MAE_test = 0,
                                MAE_CV = 0,
                                MAE_train = 0)
  
  
  
for (j in SeedTable[,i]) {
  set.seed(j)
  # Split data into train/test with ratio 7/3 of the sample size
  in_rows <- createDataPartition(y = Table1$Immobilization, p = 0.7, list = FALSE)
  train <- Table2[in_rows, ]
  test <- Table2[-in_rows, ]
  
  # Training models
  train.control <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
  RFmodel <- train(Immobilization ~ ., data = train, method = "qrf", ntree = 500, trControl = train.control); 
  print(RFmodel)
  
  # Cross validation
  train.control <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
  trainCV <- train
  RFmodelCV <- train(Immobilization ~ ., data = trainCV, method = "qrf", ntree = 500, trControl = train.control); 
  print(RFmodelCV)
  CV <- RFmodelCV$results
  
  # Make prediction on test set 
  predictions_test <- RFmodel %>% predict(test)
  predictions_train <- RFmodel %>% predict(train)
  predictions_CV <- RFmodel %>% predict(trainCV)
  
  # Compute the R2, RMSE and MAE for train, test and cross validation
  
  TestPerfomance[1,1] <- j
  TestPerfomance[1,2] <- R2(predictions_test,test$Immobilization)
  TestPerfomance[1,3] <- R2(predictions_CV,trainCV$Immobilization)
  TestPerfomance[1,4] <- R2(predictions_train,train$Immobilization)
  TestPerfomance[1,5] <- RMSE(predictions_test,test$Immobilization)
  TestPerfomance[1,6] <- RMSE(predictions_CV,trainCV$Immobilization)
  TestPerfomance[1,7] <- RMSE(predictions_train,train$Immobilization)
  TestPerfomance[1,8] <- MAE(predictions_test,test$Immobilization)
  TestPerfomance[1,9] <- MAE(predictions_CV,trainCV$Immobilization)
  TestPerfomance[1,10] <- MAE(predictions_train,train$Immobilization)
  
  # Estimate Descriptor importance
  DescImportance <- data.frame(Descriptor = row.names(varImp(RFmodel, scale=TRUE)$importance),
                               Value = varImp(RFmodel, scale=TRUE)$importance)
  colnames(DescImportance) <- c("Descriptor", "Value")
  
  
  # Save output to excel file
  Table3 <- train; Table3$Predict.Immobilization <- predictions_train
  Table4 <- test; Table4$Predict.Immobilization <- predictions_test
  
  ExcelFile <- createWorkbook("TiO2")
  addWorksheet(ExcelFile, "train")
  writeData(ExcelFile, sheet = 1, Table3)
  addWorksheet(ExcelFile, "test")
  writeData(ExcelFile, sheet = 2, Table4)
  addWorksheet(ExcelFile, "Performance")
  writeData(ExcelFile, sheet = 3, TestPerfomance)
  addWorksheet(ExcelFile, "Desc_Importance")
  writeData(ExcelFile, sheet = 4, DescImportance)
  addWorksheet(ExcelFile, "ModelInfo")
  writeData(ExcelFile, sheet = 5, capture.output(RFmodel))
  saveWorkbook(ExcelFile, paste(Savefolder,"/",tools::file_path_sans_ext(basename(QSARFile)),i,'_seed_',j,'.xlsx', sep=""), overwrite = TRUE)
}
}


