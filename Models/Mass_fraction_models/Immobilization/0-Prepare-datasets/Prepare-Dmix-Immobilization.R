library(rmarkdown)
library(svDialogs) 
library(data.table)
library(ggplot2)
library(openxlsx)


## Read excel data files, the excel files are in same folder with this R code file

QMFile <- dlg_open(title = "Select excel file for Quantum mechanical (QM) descriptors", filters = dlg_filters[c("All"), ])$res; QMFolder <- dirname(QMFile)
MeOxFile <- dlg_open(title = "Select excel file for MeOx data", filters = dlg_filters[c("All"), ])$res; MeOxFolder <- dirname(MeOxFile)
QMdesc <- read.xlsx(QMFile, sheet = 1, startRow = 1, colNames = TRUE,
                    rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                    skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                    namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)
MeOx <- read.xlsx(MeOxFile, sheet = 1, startRow = 1, colNames = TRUE,
                  rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                  skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                  namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)


# Match Mat1 in MeOx data table with QMdesc table
a1 <- unlist(lapply(c(1:nrow(MeOx)), function(i){
  which(grepl(MeOx$Mat1[i], QMdesc$NAME))
}))
# Match Mat2 in MeOx data table with QMdesc table
a2 <- unlist(lapply(c(1:nrow(MeOx)), function(i){
  which(grepl(MeOx$Mat2[i], QMdesc$NAME))
}))


# Prepare common part of Dmix datasets
Dmix <- data.frame("Mat1" = MeOx$Mat1,
                    "x1"   = MeOx$x1,
                    "Mat2" = MeOx$Mat2,
                    "x2"   = MeOx$x2,
                    "ET"   = MeOx$ET,
                    "CS"   = MeOx$CS,
                    "Zeta"   = MeOx$Zeta,
                    "Immobilization"   = MeOx$Immobilization)


# ------------------------------------------------------------------------------
# Prepare Dmix1
Dmix1 <- Dmix
Dmix1$HOFmix1 <- MeOx$x1/100*QMdesc$HOF[a1] + MeOx$x2/100*QMdesc$HOF[a2]
Dmix1$TEmix1 <- MeOx$x1/100*QMdesc$TE[a1] + MeOx$x2/100*QMdesc$TE[a2]
Dmix1$EEmix1 <- MeOx$x1/100*QMdesc$EE[a1] + MeOx$x2/100*QMdesc$EE[a2]
Dmix1$CCRmix1 <- MeOx$x1/100*QMdesc$CCR[a1] + MeOx$x2/100*QMdesc$CCR[a2]
Dmix1$CAmix1 <- MeOx$x1/100*QMdesc$CA[a1] + MeOx$x2/100*QMdesc$CA[a2]
Dmix1$CVmix1 <- MeOx$x1/100*QMdesc$CV[a1] + MeOx$x2/100*QMdesc$CV[a2]
Dmix1$IPmix1 <- MeOx$x1/100*QMdesc$IP[a1] + MeOx$x2/100*QMdesc$IP[a2]
Dmix1$HOMOmix1 <- MeOx$x1/100*QMdesc$HOMO[a1] + MeOx$x2/100*QMdesc$HOMO[a2]
Dmix1$LUMOmix1 <- MeOx$x1/100*QMdesc$LUMO[a1] + MeOx$x2/100*QMdesc$LUMO[a2]
Dmix1$MEmix1 <- MeOx$x1/100*QMdesc$ME[a1] + MeOx$x2/100*QMdesc$ME[a2]
Dmix1$PPAHmix1 <- MeOx$x1/100*QMdesc$PPAH[a1] + MeOx$x2/100*QMdesc$PPAH[a2]



# ------------------------------------------------------------------------------
# Prepare Dmix2
Dmix2 <- Dmix
Dmix2$HOFmix2 <- (MeOx$x1/100*QMdesc$HOF[a1] + MeOx$x2/100*QMdesc$HOF[a2])^2
Dmix2$TEmix2 <- (MeOx$x1/100*QMdesc$TE[a1] + MeOx$x2/100*QMdesc$TE[a2])^2
Dmix2$EEmix2 <- (MeOx$x1/100*QMdesc$EE[a1] + MeOx$x2/100*QMdesc$EE[a2])^2
Dmix2$CCRmix2 <- (MeOx$x1/100*QMdesc$CCR[a1] + MeOx$x2/100*QMdesc$CCR[a2])^2
Dmix2$CAmix2 <- (MeOx$x1/100*QMdesc$CA[a1] + MeOx$x2/100*QMdesc$CA[a2])^2
Dmix2$CVmix2 <- (MeOx$x1/100*QMdesc$CV[a1] + MeOx$x2/100*QMdesc$CV[a2])^2
Dmix2$IPmix2 <- (MeOx$x1/100*QMdesc$IP[a1] + MeOx$x2/100*QMdesc$IP[a2])^2
Dmix2$HOMOmix2 <- (MeOx$x1/100*QMdesc$HOMO[a1] + MeOx$x2/100*QMdesc$HOMO[a2])^2
Dmix2$LUMOmix2 <- (MeOx$x1/100*QMdesc$LUMO[a1] + MeOx$x2/100*QMdesc$LUMO[a2])^2
Dmix2$MEmix2 <- (MeOx$x1/100*QMdesc$ME[a1] + MeOx$x2/100*QMdesc$ME[a2])^2
Dmix2$PPAHmix2 <- (MeOx$x1/100*QMdesc$PPAH[a1] + MeOx$x2/100*QMdesc$PPAH[a2])^2



# ------------------------------------------------------------------------------
# Prepare Dmix3
Dmix3 <- Dmix
Dmix3$HOFmix3 <- ((MeOx$x1/100*QMdesc$HOF[a1])^2 + (MeOx$x2/100*QMdesc$HOF[a2])^2)^0.5
Dmix3$TEmix3 <- ((MeOx$x1/100*QMdesc$TE[a1])^2 + (MeOx$x2/100*QMdesc$TE[a2])^2)^0.5
Dmix3$EEmix3 <- ((MeOx$x1/100*QMdesc$EE[a1])^2 + (MeOx$x2/100*QMdesc$EE[a2])^2)^0.5
Dmix3$CCRmix3 <- ((MeOx$x1/100*QMdesc$CCR[a1])^2 + (MeOx$x2/100*QMdesc$CCR[a2])^2)^0.5
Dmix3$CAmix3 <- ((MeOx$x1/100*QMdesc$CA[a1])^2 + (MeOx$x2/100*QMdesc$CA[a2])^2)^0.5
Dmix3$CVmix3 <- ((MeOx$x1/100*QMdesc$CV[a1])^2 + (MeOx$x2/100*QMdesc$CV[a2])^2)^0.5
Dmix3$IPmix3 <- ((MeOx$x1/100*QMdesc$IP[a1])^2 + (MeOx$x2/100*QMdesc$IP[a2])^2)^0.5
Dmix3$HOMOmix3 <- ((MeOx$x1/100*QMdesc$HOMO[a1])^2 + (MeOx$x2/100*QMdesc$HOMO[a2])^2)^0.5
Dmix3$LUMOmix3 <- ((MeOx$x1/100*QMdesc$LUMO[a1])^2 + (MeOx$x2/100*QMdesc$LUMO[a2])^2)^0.5
Dmix3$MEmix3 <- ((MeOx$x1/100*QMdesc$ME[a1])^2 + (MeOx$x2/100*QMdesc$ME[a2])^2)^0.5
Dmix3$PPAHmix3 <- ((MeOx$x1/100*QMdesc$PPAH[a1])^2 + (MeOx$x2/100*QMdesc$PPAH[a2])^2)^0.5


# ------------------------------------------------------------------------------
# Prepare Dmix4
Dmix4 <- Dmix
Dmix4$HOFmix4 <- (MeOx$x1/100)^0.5*QMdesc$HOF[a1] + (MeOx$x2/100)^0.5*QMdesc$HOF[a2]
Dmix4$TEmix4 <- (MeOx$x1/100)^0.5*QMdesc$TE[a1] + (MeOx$x2/100)^0.5*QMdesc$TE[a2]
Dmix4$EEmix4 <- (MeOx$x1/100)^0.5*QMdesc$EE[a1] + (MeOx$x2/100)^0.5*QMdesc$EE[a2]
Dmix4$CCRmix4 <- (MeOx$x1/100)^0.5*QMdesc$CCR[a1] + (MeOx$x2/100)^0.5*QMdesc$CCR[a2]
Dmix4$CAmix4 <- (MeOx$x1/100)^0.5*QMdesc$CA[a1] + (MeOx$x2/100)^0.5*QMdesc$CA[a2]
Dmix4$CVmix4 <- (MeOx$x1/100)^0.5*QMdesc$CV[a1] + (MeOx$x2/100)^0.5*QMdesc$CV[a2]
Dmix4$IPmix4 <- (MeOx$x1/100)^0.5*QMdesc$IP[a1] + (MeOx$x2/100)^0.5*QMdesc$IP[a2]
Dmix4$HOMOmix4 <- (MeOx$x1/100)^0.5*QMdesc$HOMO[a1] + (MeOx$x2/100)^0.5*QMdesc$HOMO[a2]
Dmix4$LUMOmix4 <- (MeOx$x1/100)^0.5*QMdesc$LUMO[a1] + (MeOx$x2/100)^0.5*QMdesc$LUMO[a2]
Dmix4$MEmix4 <- (MeOx$x1/100)^0.5*QMdesc$ME[a1] + (MeOx$x2/100)^0.5*QMdesc$ME[a2]
Dmix4$PPAHmix4 <- (MeOx$x1/100)^0.5*QMdesc$PPAH[a1] + (MeOx$x2/100)^0.5*QMdesc$PPAH[a2]


# ------------------------------------------------------------------------------
# Prepare Dmix5
Dmix5 <- Dmix
Dmix5$HOFmix5 <- abs(QMdesc$HOF[a1])^0.5 + abs(QMdesc$HOF[a2])^0.5
Dmix5$TEmix5 <- abs(QMdesc$TE[a1])^0.5 + abs(QMdesc$TE[a2])^0.5
Dmix5$EEmix5 <- abs(QMdesc$EE[a1])^0.5 + abs(QMdesc$EE[a2])^0.5
Dmix5$CCRmix5 <- abs(QMdesc$CCR[a1])^0.5 + abs(QMdesc$CCR[a2])^0.5
Dmix5$CAmix5 <- abs(QMdesc$CA[a1])^0.5 + abs(QMdesc$CA[a2])^0.5
Dmix5$CVmix5 <- abs(QMdesc$CV[a1])^0.5 + abs(QMdesc$CV[a2])^0.5
Dmix5$IPmix5 <- abs(QMdesc$IP[a1])^0.5 + abs(QMdesc$IP[a2])^0.5
Dmix5$HOMOmix5 <- abs(QMdesc$HOMO[a1])^0.5 + abs(QMdesc$HOMO[a2])^0.5
Dmix5$LUMOmix5 <- abs(QMdesc$LUMO[a1])^0.5 + abs(QMdesc$LUMO[a2])^0.5
Dmix5$MEmix5 <- abs(QMdesc$ME[a1])^0.5 + abs(QMdesc$ME[a2])^0.5
Dmix5$PPAHmix5 <- abs(QMdesc$PPAH[a1])^0.5 + abs(QMdesc$PPAH[a2])^0.5


# ------------------------------------------------------------------------------
# Prepare Dmix6
Dmix6 <- Dmix
Dmix6$HOFmix6 <- MeOx$x1/100*(QMdesc$HOF[a1])^2 + MeOx$x2/100*(QMdesc$HOF[a2])^2
Dmix6$TEmix6 <- MeOx$x1/100*(QMdesc$TE[a1])^2 + MeOx$x2/100*(QMdesc$TE[a2])^2
Dmix6$EEmix6 <- MeOx$x1/100*(QMdesc$EE[a1])^2 + MeOx$x2/100*(QMdesc$EE[a2])^2
Dmix6$CCRmix6 <- MeOx$x1/100*(QMdesc$CCR[a1])^2 + MeOx$x2/100*(QMdesc$CCR[a2])^2
Dmix6$CAmix6 <- MeOx$x1/100*(QMdesc$CA[a1])^2 + MeOx$x2/100*(QMdesc$CA[a2])^2
Dmix6$CVmix6 <- MeOx$x1/100*(QMdesc$CV[a1])^2 + MeOx$x2/100*(QMdesc$CV[a2])^2
Dmix6$IPmix6 <- MeOx$x1/100*(QMdesc$IP[a1])^2 + MeOx$x2/100*(QMdesc$IP[a2])^2
Dmix6$HOMOmix6 <- MeOx$x1/100*(QMdesc$HOMO[a1])^2 + MeOx$x2/100*(QMdesc$HOMO[a2])^2
Dmix6$LUMOmix6 <- MeOx$x1/100*(QMdesc$LUMO[a1])^2 + MeOx$x2/100*(QMdesc$LUMO[a2])^2
Dmix6$MEmix6 <- MeOx$x1/100*(QMdesc$ME[a1])^2 + MeOx$x2/100*(QMdesc$ME[a2])^2
Dmix6$PPAHmix6 <- MeOx$x1/100*(QMdesc$PPAH[a1])^2 + MeOx$x2/100*(QMdesc$PPAH[a2])^2


# ------------------------------------------------------------------------------
# Prepare Dmix7
Dmix7 <- Dmix
Dmix7$HOFmix7 <- (MeOx$x1/100)^2*QMdesc$HOF[a1] + (MeOx$x2/100)^2*QMdesc$HOF[a2]
Dmix7$TEmix7 <- (MeOx$x1/100)^2*QMdesc$TE[a1] + (MeOx$x2/100)^2*QMdesc$TE[a2]
Dmix7$EEmix7 <- (MeOx$x1/100)^2*QMdesc$EE[a1] + (MeOx$x2/100)^2*QMdesc$EE[a2]
Dmix7$CCRmix7 <- (MeOx$x1/100)^2*QMdesc$CCR[a1] + (MeOx$x2/100)^2*QMdesc$CCR[a2]
Dmix7$CAmix7 <- (MeOx$x1/100)^2*QMdesc$CA[a1] + (MeOx$x2/100)^2*QMdesc$CA[a2]
Dmix7$CVmix7 <- (MeOx$x1/100)^2*QMdesc$CV[a1] + (MeOx$x2/100)^2*QMdesc$CV[a2]
Dmix7$IPmix7 <- (MeOx$x1/100)^2*QMdesc$IP[a1] + (MeOx$x2/100)^2*QMdesc$IP[a2]
Dmix7$HOMOmix7 <- (MeOx$x1/100)^2*QMdesc$HOMO[a1] + (MeOx$x2/100)^2*QMdesc$HOMO[a2]
Dmix7$LUMOmix7 <- (MeOx$x1/100)^2*QMdesc$LUMO[a1] + (MeOx$x2/100)^2*QMdesc$LUMO[a2]
Dmix7$MEmix7 <- (MeOx$x1/100)^2*QMdesc$ME[a1] + (MeOx$x2/100)^2*QMdesc$ME[a2]
Dmix7$PPAHmix7 <- (MeOx$x1/100)^2*QMdesc$PPAH[a1] + (MeOx$x2/100)^2*QMdesc$PPAH[a2]


# ------------------------------------------------------------------------------
# Prepare Dmix8
Cubicroot <- function(x) {sign(x) * abs(x)^(1/3)}
Dmix8 <- Dmix
Dmix8$HOFmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$HOF[a1] + (MeOx$x2/100)^3*QMdesc$HOF[a2])
Dmix8$TEmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$TE[a1] + (MeOx$x2/100)^3*QMdesc$TE[a2])
Dmix8$EEmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$EE[a1] + (MeOx$x2/100)^3*QMdesc$EE[a2])
Dmix8$CCRmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$CCR[a1] + (MeOx$x2/100)^3*QMdesc$CCR[a2])
Dmix8$CAmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$CA[a1] + (MeOx$x2/100)^3*QMdesc$CA[a2])
Dmix8$CVmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$CV[a1] + (MeOx$x2/100)^3*QMdesc$CV[a2])
Dmix8$IPmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$IP[a1] + (MeOx$x2/100)^3*QMdesc$IP[a2])
Dmix8$HOMOmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$HOMO[a1] + (MeOx$x2/100)^3*QMdesc$HOMO[a2])
Dmix8$LUMOmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$LUMO[a1] + (MeOx$x2/100)^3*QMdesc$LUMO[a2])
Dmix8$MEmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$ME[a1] + (MeOx$x2/100)^3*QMdesc$ME[a2])
Dmix8$PPAHmix8 <- Cubicroot((MeOx$x1/100)^3*QMdesc$PPAH[a1] + (MeOx$x2/100)^3*QMdesc$PPAH[a2])


# ------------------------------------------------------------------------------
# Prepare Dmix9
Dmix9 <- Dmix
Dmix9$HOFmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$HOF[a1])*MeOx$x2/100*abs(QMdesc$HOF[a2]))
Dmix9$TEmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$TE[a1])*MeOx$x2/100*abs(QMdesc$TE[a2]))
Dmix9$EEmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$EE[a1])*MeOx$x2/100*abs(QMdesc$EE[a2]))
Dmix9$CCRmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$CCR[a1])*MeOx$x2/100*abs(QMdesc$CCR[a2]))
Dmix9$CAmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$CA[a1])*MeOx$x2/100*abs(QMdesc$CA[a2]))
Dmix9$CVmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$CV[a1])*MeOx$x2/100*abs(QMdesc$CV[a2]))
Dmix9$IPmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$IP[a1])*MeOx$x2/100*abs(QMdesc$IP[a2]))
Dmix9$HOMOmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$HOMO[a1])*MeOx$x2/100*abs(QMdesc$HOMO[a2]))
Dmix9$LUMOmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$LUMO[a1])*MeOx$x2/100*abs(QMdesc$LUMO[a2]))
Dmix9$MEmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$ME[a1])*MeOx$x2/100*abs(QMdesc$ME[a2]))
Dmix9$PPAHmix9 <- sqrt(MeOx$x1/100*abs(QMdesc$PPAH[a1])*MeOx$x2/100*abs(QMdesc$PPAH[a2]))


# ------------------------------------------------------------------------------
# Save the datasets Dmix with Quantum descriptors to excel file
ExcelFile <- createWorkbook("Dmix")
addWorksheet(ExcelFile, "Dmix1") 
writeData(ExcelFile, sheet = 1, Dmix1, rowNames = FALSE)
addWorksheet(ExcelFile, "Dmix2") 
writeData(ExcelFile, sheet = 2, Dmix2, rowNames = FALSE)
addWorksheet(ExcelFile, "Dmix3") 
writeData(ExcelFile, sheet = 3, Dmix3, rowNames = FALSE)
addWorksheet(ExcelFile, "Dmix4") 
writeData(ExcelFile, sheet = 4, Dmix4, rowNames = FALSE)
addWorksheet(ExcelFile, "Dmix5") 
writeData(ExcelFile, sheet = 5, Dmix5, rowNames = FALSE)
addWorksheet(ExcelFile, "Dmix6") 
writeData(ExcelFile, sheet = 6, Dmix6, rowNames = FALSE)
addWorksheet(ExcelFile, "Dmix7") 
writeData(ExcelFile, sheet = 7, Dmix7, rowNames = FALSE)
addWorksheet(ExcelFile, "Dmix8") 
writeData(ExcelFile, sheet = 8, Dmix8, rowNames = FALSE)
addWorksheet(ExcelFile, "Dmix9") 
writeData(ExcelFile, sheet = 9, Dmix9, rowNames = FALSE)
saveWorkbook(ExcelFile, paste(MeOxFolder,'/Dmix_Immobilization.xlsx', sep=""), overwrite = TRUE)

