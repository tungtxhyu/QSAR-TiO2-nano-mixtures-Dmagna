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

# server.R
server <- function(input, output) {
  output$userdata <- renderTable({
    #make table of user input data
    a <- which(apply(as.data.frame(Descriptors[,2]), 1, function(r) any(r %in% input$mixchem)))
    Cmix <- input$C1 + input$C2
    Mol1 <- input$C1/80/(input$C1/80+input$C2/Descriptors[a,14])
    Mol2 <- 1-Mol1
    table1 <- data.frame("CoreSize" = input$CSize,
                         "HSize" = input$HSize,
                         "ZetaPotential" = input$ZetaPotential,
                         "SurfaceArea"   = input$SurfaceArea,
                         "ExposureTime"  = input$ETime,
                         "C1"            = input$C1,
                         "C2"            = input$C2,
                         "Mol1"          = Mol1,
                         "Mol2"          = Mol2,
                         "HOF9"          = sqrt(Mol1*abs(Descriptors[9,3])*Mol2*abs(Descriptors[a,3])),
                         "CCR9"          = sqrt(Mol1*abs(Descriptors[9,6])*Mol2*abs(Descriptors[a,6])),
                         "LUMO9"         = sqrt(Mol1*abs(Descriptors[9,11])*Mol2*abs(Descriptors[a,11])),
                         "PPAH9"         = sqrt(Mol1*abs(Descriptors[9,13])*Mol2*abs(Descriptors[a,13])),
                         "Imm"           = input$Endpoint)
    
    table2 <- data.frame("Imm.Predict" = predict(RFmodel, table1))
    
    table3 <- cbind(table1,table2)
})
  output$modellinearity <- renderPlot({
    #make table of user input data
    a <- which(apply(as.data.frame(Descriptors[,2]), 1, function(r) any(r %in% input$mixchem)))
    Cmix <- input$C1 + input$C2
    Mol1 <- input$C1/80/(input$C1/80+input$C2/Descriptors[a,14])
    Mol2 <- 1-Mol1
    table1 <- data.frame("CoreSize" = input$CSize,
                         "HSize" = input$HSize,
                         "ZetaPotential" = input$ZetaPotential,
                         "SurfaceArea"   = input$SurfaceArea,
                         "ExposureTime"  = input$ETime,
                         "C1"            = input$C1,
                         "C2"            = input$C2,
                         "Mol1"          = Mol1,
                         "Mol2"          = Mol2,
                         "HOF9"          = sqrt(Mol1*abs(Descriptors[9,3])*Mol2*abs(Descriptors[a,3])),
                         "CCR9"          = sqrt(Mol1*abs(Descriptors[9,6])*Mol2*abs(Descriptors[a,6])),
                         "LUMO9"         = sqrt(Mol1*abs(Descriptors[9,11])*Mol2*abs(Descriptors[a,11])),
                         "PPAH9"         = sqrt(Mol1*abs(Descriptors[9,13])*Mol2*abs(Descriptors[a,13])),
                         "Imm"           = input$Endpoint)
    table2 <- data.frame("X" = input$Endpoint, "Y" = predict(RFmodel, table1))
    #Draw plots for linearity
    p1 <- ggplot(train, aes(x=Imm, y=Imm.pred)) + geom_point(aes(x=Imm, y=Imm.pred), color="red", alpha=1.0, size = 4, shape=1) +
      theme_bw() +
      xlab(expression(paste("Observed immobilization (%)"))) +
      ylab(expression(paste("Predicted immobilization (%)"))) +
      xlim(0,100) +
      ylim(0,100) +
      annotate("text", x = 50, y = 100, label = legend1) +
      annotate("pointrange", x = 5, y = 90, ymin = 90, ymax = 90, colour = "red", size = 1.0, shape=1)+
      annotate("pointrange", x = 5, y = 80, ymin = 80, ymax = 80, colour = "black", size = 1.0, shape=1)+
      theme(axis.ticks = element_blank(),
            axis.text = element_text(size=16),
            axis.title = element_text(size=24),
            axis.title.x = element_text(size=24),
            axis.ticks.length = unit(0.0, "cm"),
            legend.text=element_text(size=16),
            legend.position="top",
            legend.direction="horizontal",
            legend.box = "vertical",
            legend.margin =margin(0,0,0,0),
            plot.margin = unit(c(0.5, 0.5, 0.1, 0.1), "cm"),
            panel.spacing = unit(c(0.01, 0.01, 0.01, 0.01), "cm")) 
    p1 + geom_point(data=test, aes(x=Imm, y=Imm.pred), colour="black", alpha=1.0, size = 4, shape=1) +
      annotate("text", x = 15, y = 90, label = "Train set") + 
      annotate("text", x = 15, y = 80, label = "Test set") +
      geom_line(data=data.frame("X"=c(0,100), "Y"=c(0,100)),aes(x=X, y=Y), colour="black", alpha=1.0) +
      geom_point(data=table2, aes(x=X, y=Y), colour="blue", alpha=1.0, size = 10, shape=18)
    
  })
  output$descriptorimportance <- renderPlot({
    plot(importance)
  })
 
} 

shinyApp(server = server, ui = ui)

