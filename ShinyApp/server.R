#############################################
# Install packages if they are not installed yet
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
#ipak <- function(pkg){
#  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#  if (length(new.pkg)) 
#    install.packages(new.pkg, dependencies = TRUE)
#  sapply(pkg, require, character.only = TRUE)
#}
#packages <- c("caret", "randomForest", "data.table", "shiny", "shinythemes", "shinydashboard")
#ipak(packages)

# Load library
library(caret) 
library(randomForest)
library(data.table)
library(shiny)
library(shinythemes)
library(shinydashboard)
#############################################

#Load data and trained model
load("www/PfmDmix1.Rdata")
load("www/PfmDmix9.Rdata")
load("www/RFDmix1.Rdata")
load("www/RFDmix9.Rdata")
load("www/DescMOPAC.Rdata")


server <- function(input, output) {

# Read performance table
    output$PerformanceDmix1 <- renderTable({
    data.frame("Parameter" =  c("R2 test","R2 cross validation","R2 train","RMSE test","RMSE cross validation","RMSE train","MAE test","MAE cross validation","MAE train"),
               "Value" = c(PfmDmix1$R2_test[1],PfmDmix1$R2_CV[1],PfmDmix1$R2_train[1], PfmDmix1$RMSE_test[1],PfmDmix1$RMSE_CV[1],PfmDmix1$RMSE_train[1],PfmDmix1$MAE_test[1],PfmDmix1$MAE_CV[1],PfmDmix1$MAE_train[1]))
  }, digits = 3)
  
  output$PerformanceDmix9 <- renderTable({
    data.frame("Parameter" =  c("R2 test","R2 cross validation","R2 train","RMSE test","RMSE cross validation","RMSE train","MAE test","MAE cross validation","MAE train"),
               "Value" = c(PfmDmix9$R2_test[1],PfmDmix9$R2_CV[1],PfmDmix9$R2_train[1], PfmDmix9$RMSE_test[1],PfmDmix9$RMSE_CV[1],PfmDmix9$RMSE_train[1],PfmDmix9$MAE_test[1],PfmDmix9$MAE_CV[1],PfmDmix9$MAE_train[1]))
  }, digits = 3)
  
  #make table of user input data of TiO2 based nano-mixtures
  output$TiO2datainput <- renderTable({
    data.frame("Parameter" = c("MeOxNP",
                           "TiO2 core diameter (nm)",
                           "TiO2 concentration (ug/L)",
                           "Mixed chemical",
                           "Mixed chemical concentration (ug/L)",
                           "Exposure time (h)"),
               "Value" = c(input$MeOx,
                           input$CSize,
                           input$C1,
                           input$mixchem,
                           input$C2,
                           input$ETime)
               )
  })
  
  
  #make table of predicted data  
  output$Prediction <- renderValueBox({
    a <- which(apply(as.data.frame(DescMOPAC[,2]), 1, function(r) any(r %in% input$mixchem)))
    Cmix <- input$C1 + input$C2
    Mol1 <- input$C1/DescMOPAC[9,3]/(input$C1/DescMOPAC[9,3]+input$C2/DescMOPAC[a,3])
    Mol2 <- 1-Mol1
    table1 <- data.frame("CSize" = input$CSize,
                         "HSize" = input$HSize,
                         "Zeta" = input$Zeta,
                         "SArea"   = 6/(4.23*10^6*input$CSize*10^-9),
                         "ETime"  = input$ETime,
                         "C1"     = input$C1,
                         "C2"     = input$C2,
                         "HOF1"= Mol1*DescMOPAC[9,4] + Mol2*DescMOPAC[a,4],
                         "EE1"= Mol1*DescMOPAC[9,6] + Mol2*DescMOPAC[a,6],
                         "IP1"= Mol1*DescMOPAC[9,10] + Mol2*DescMOPAC[a,10],
                         "ME1"= Mol1*DescMOPAC[9,13] + Mol2*DescMOPAC[a,13],
                         "Immobilization" = input$Endpoint)
    Imm_TiO2Dmix <- predict(RFDmix1, table1)

    color_TiO2Dmix <- fifelse(Imm_TiO2Dmix <= 30, "green", 
                    fifelse(Imm_TiO2Dmix <= 50, "yellow",
                        fifelse(Imm_TiO2Dmix <= 75, "red","red"))) 
    icon_TiO2Dmix <- fifelse(Imm_TiO2Dmix <= 25, "fas fa-envira","exclamation-triangle")
    title_TiO2Dmix <- paste(round(Imm_TiO2Dmix, digits = 1), " %", sep = "" )
    subtitle_TiO2Dmix <- p("of ",em("Daphnia magna")," population might be immobilized")
    
    valueBox(title_TiO2Dmix, subtitle_TiO2Dmix, icon = icon(icon_TiO2Dmix), color = color_TiO2Dmix)
  })
  
  
}

