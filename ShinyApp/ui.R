# Install below packages in the first time you run this code
#install.packages(c("openxlsx","svDialogs","data.table","mlbench","caret", "shiny", "shinydashboard"), dependencies = TRUE)

library(shiny)
library(svDialogs) 
library(data.table)
library(openxlsx)
library(mlbench)
library(caret)
library(tools)
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(shinydashboard)

# Step 1: Read excel data file 
QSARdata <- read.xlsx("dataset/Dmix.xlsx", sheet = 1, startRow = 1, colNames = TRUE,
                      rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                      skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                      namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)
Descriptors <- read.xlsx("dataset/Descriptors.xlsx", sheet = 1, startRow = 1, colNames = TRUE,
                         rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                         skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                         namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)


# Step 2: remove highly correlated descriptors (correlation greater than 0.95)
tmp <- cor(QSARdata)
tmp[!lower.tri(tmp)] <- 0
QSARdata1 <- QSARdata[,!apply(tmp,2,function(x) any(x >= 0.95))]

# Step 3: randomly split data into train set and test set
# Set random seed to reproduce this splitting
set.seed(199200)
## 70% of the sample size
split_size <- floor(0.70 * nrow(QSARdata1))

in_rows <- sample(c(1:nrow(QSARdata1)), size = split_size, replace = FALSE)

train <- QSARdata1[in_rows, ]
test <- QSARdata1[-in_rows, ]


# Step 4:Training models
RFmodel <- train(Imm ~ ., data = train, method = "rf", ntree = 100)

# estimate variable importance
importance <- varImp(RFmodel, scale=TRUE)

# summarize importance
print(importance)

# plot importance
plot(importance)


# Step 6: make prediction on test set
test$Imm.pred <- predict(RFmodel, test)
train$Imm.pred <- predict(RFmodel, train)
observed.train <- train$Imm
predicted.train <- train$Imm.pred
observed.test <- test$Imm
predicted.test <- test$Imm.pred
R2train <- 1 - (sum((observed.train-predicted.train)^2)/sum((observed.train-mean(observed.train))^2))
RMSEtrain <- RMSE(predicted.train,observed.train)
R2test <- 1 - (sum((observed.test-predicted.test)^2)/sum((observed.test-mean(observed.test))^2))
RMSEtest <- RMSE(predicted.test,observed.test)
legend1 <- paste("R2_train = ", round(R2train, digits = 3), "; ", "R2_test = ", round(R2test, digits = 3), "; ", "RMSE_train = ", round(RMSEtrain, digits = 2), "; ", "RMSE_test = ", round(RMSEtest, digits = 2), sep = "")


#Summary RF model
RFmodel$finalModel
RFmodel$results



# Step: make graphical user interface app 
# ui.R
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("QSAR for predicting toxicity of TiO2 based nano-mixtures to Daphnia magna"),
                sliderInput(inputId = "CSize", label = "Core size (nm) of TiO2 nanoparticles:", 
                            value = 25, min = 6, max = 100),
                sliderInput(inputId = "HSize", label = "Hydrodynamic size (nm) of TiO2 nanoparticles:", 
                            value = 80, min = 6, max = 600), 
                sliderInput(inputId = "ZetaPotential", label = "Zeta Potential (mV) of TiO2 nanoparticles:", 
                            value = -5, min = -20, max = -1),
                sliderInput(inputId = "SurfaceArea", label = "Surface Area (m2/g) of TiO2 nanoparticles:", 
                            value = 100, min = 10, max = 250), 
                sliderInput(inputId = "C1", label = "Concentration (mg/L) of TiO2 nanoparticles:", 
                            value = 5, min = 0.001, max = 300), 
                selectInput(inputId = "mixchem", "Mixed chemicals:",
                            c("AgNO3" = "AgNO3",
                              "Cd(NO3)2" = "Cd(NO3)2",
                              "CdCl2" = "CdCl2",
                              "Cu(NO3)2" = "Cu(NO3)2",
                              "CuSO4" = "CuSO4",
                              "Na2HAsO4" = "Na2HAsO4",
                              "Pentabromodiphenyl ether" = "Pentabromodiphenyl-ether",
                              "Pirimicarb" = "Pirimicarb",
                              "TritonX-100" = "TritonX-100",
                              "ZnCl2" = "ZnCl2")),
                sliderInput(inputId = "C2", label = "Concentration (mg/L) of the mixed chemical:", 
                            value = 2, min = 0.001, max = 10),
                sliderInput(inputId = "ETime", label = "Exposure time (h):", 
                            value = 24, min = 1, max = 96),
                sliderInput(inputId = "Endpoint", label = "Expected toxicity - Immobilization (%):", 
                            value = 50, min = 0, max = 100), 
                tableOutput("userdata"),
                plotOutput("modellinearity"),
                plotOutput("descriptorimportance")
                
)