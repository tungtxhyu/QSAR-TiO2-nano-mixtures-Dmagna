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


# Step 1: Read excel data file 
ModelSummary <- data.frame("Dataset" = NA,
           "Seed" = NA,
           "R2_test" = NA,
           "R2_CV" = NA,
           "R2_train" = NA,
           "RMSE_test" = NA,
           "RMSE_CV" = NA,
           "RMSE_train" = NA,
           "MAE_test" = NA,
           "MAE_CV" = NA,
           "MAE_train" = NA)
QSARFolder <- dlg_dir(title = "Select folder")$res
QSARFiles <- list.files(path = QSARFolder, pattern = ".xlsx", full.names = TRUE)
for (i in c(1:length(QSARFiles))) {
ModelPerformance <- read.xlsx(QSARFiles[i], sheet = 3, startRow = 1, colNames = TRUE,
                              rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                              skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                              namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)
ModelSummary[i,1] <- tools::file_path_sans_ext(basename(QSARFiles[i]))
ModelSummary[i,2] <- ModelPerformance$Seed
ModelSummary[i,3] <- ModelPerformance$R2_test
ModelSummary[i,4] <- ModelPerformance$R2_CV
ModelSummary[i,5] <- ModelPerformance$R2_train
ModelSummary[i,6] <- ModelPerformance$RMSE_test
ModelSummary[i,7] <- ModelPerformance$RMSE_CV
ModelSummary[i,8] <- ModelPerformance$RMSE_train
ModelSummary[i,9] <- ModelPerformance$MAE_test
ModelSummary[i,10] <- ModelPerformance$MAE_CV
ModelSummary[i,11] <- ModelPerformance$MAE_train
}
ExcelFile <- createWorkbook("TiO2")
addWorksheet(ExcelFile, "Performance")
writeData(ExcelFile, sheet = 1, ModelSummary)
saveWorkbook(ExcelFile, paste(dirname(QSARFolder),"/Summary_pEC50_ANN.xlsx", sep=""), overwrite = TRUE)

