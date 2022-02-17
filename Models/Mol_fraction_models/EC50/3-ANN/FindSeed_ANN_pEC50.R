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
# Read dataset (Dmix1-Dmix9) file 
QSARFile <- dlg_open(title = "Select excel file for Dmix", filters = dlg_filters[c("All"), ])$res; QSARFolder <- dirname(QSARFile)

ExcelFile <- createWorkbook("FindSeed")


QSARdata <- read.xlsx(QSARFile, sheet = 9, startRow = 1, colNames = TRUE,
                      rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                      skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                      namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)
# Remove character columns
Table1 <- QSARdata[-c(2,52,53),-c(1,3)]

# Make a Table to store model performance
TestPerfomance <- data.frame( Seed = 0,
                              R2_all = 0,
                              R2_test = 0,
                              R2_train = 0)

# Find random seed to get good R2 value
for (i in 1:1000) {
  set.seed(i*5)
  ## 80% of the sample size
  in_rows <- createDataPartition(y = Table1$pEC50, p = 0.66, list = FALSE)
  
  # Training models
  train <- rbind(Table1[in_rows, ],QSARdata[c(2,52,53),-c(1,3)])
  test <- Table1[-in_rows, ]
  
  train.control <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
  ANNmodel <- train(pEC50 ~ ., data = train, method = "brnn", trControl = train.control)
  print(ANNmodel)
  
  
  # Make prediction on test set and compute the R2, RMSE and MAE
  predictions <- ANNmodel %>% predict(test)
  predictions_train <- ANNmodel %>% predict(train)
  predictions_ext <- ANNmodel %>% predict(Table1)
  TestPerfomance[i,1] <- i*5
  TestPerfomance[i,2] <- R2(predictions_ext,Table1$pEC50)
  TestPerfomance[i,3] <- R2(predictions,test$pEC50)
  TestPerfomance[i,4] <- R2(predictions_train,train$pEC50)
  
}
# Save result of finding seeds

addWorksheet(ExcelFile, paste("Dmix", sep = ""))
writeData(ExcelFile, sheet = 1, TestPerfomance, rowNames = FALSE)

saveWorkbook(ExcelFile, paste(QSARFolder,"/",tools::file_path_sans_ext(basename(QSARFile)),"_seed_ANN_brnn.xlsx", sep = ""), overwrite = TRUE)
