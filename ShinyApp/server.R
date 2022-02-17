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
library(greekLetters) 
library(randomForest)
library(data.table)
library(shiny)
library(shinythemes)
library(shinydashboard)
#############################################

#Load data and trained model
load("www/PfmDmix1EC.Rdata")
load("www/PfmDmix9EC.Rdata")
load("www/RFDmix1EC.Rdata")
load("www/RFDmix9EC.Rdata")
load("www/PfmDmix1Imm.Rdata")
load("www/PfmDmix9Imm.Rdata")
load("www/RFDmix1Imm.Rdata")
load("www/RFDmix9Imm.Rdata")
load("www/DescMOPAC.Rdata")

server <- function(input, output) {

# Performance table of EC50 model
    output$PerformanceDmixEC <- renderTable({
    if (input$DmixEC == "Dmix1") {
            data.frame("Parameter" =  c("R2 test","R2 cross validation","R2 train","RMSE test","RMSE cross validation","RMSE train","MAE test","MAE cross validation","MAE train"),
                        "Value" = c(PfmDmix1EC$R2_test[1],PfmDmix1EC$R2_CV[1],PfmDmix1EC$R2_train[1], PfmDmix1EC$RMSE_test[1],PfmDmix1EC$RMSE_CV[1],PfmDmix1EC$RMSE_train[1],PfmDmix1EC$MAE_test[1],PfmDmix1EC$MAE_CV[1],PfmDmix1EC$MAE_train[1]))
    } else {
            data.frame("Parameter" =  c("R2 test","R2 cross validation","R2 train","RMSE test","RMSE cross validation","RMSE train","MAE test","MAE cross validation","MAE train"),
                       "Value" = c(PfmDmix9EC$R2_test[1],PfmDmix9EC$R2_CV[1],PfmDmix9EC$R2_train[1], PfmDmix9EC$RMSE_test[1],PfmDmix9EC$RMSE_CV[1],PfmDmix9EC$RMSE_train[1],PfmDmix9EC$MAE_test[1],PfmDmix9EC$MAE_CV[1],PfmDmix9EC$MAE_train[1]))    
            }  
  }, digits = 3)


    
# Performance table of Immobilization model
    output$PerformanceDmixImm <- renderTable({
      if (input$DmixImm == "Dmix1") {
        data.frame("Parameter" =  c("R2 test","R2 cross validation","R2 train","RMSE test","RMSE cross validation","RMSE train","MAE test","MAE cross validation","MAE train"),
                   "Value" = c(PfmDmix1Imm$R2_test[1],PfmDmix1Imm$R2_CV[1],PfmDmix1Imm$R2_train[1], PfmDmix1Imm$RMSE_test[1],PfmDmix1Imm$RMSE_CV[1],PfmDmix1Imm$RMSE_train[1],PfmDmix1Imm$MAE_test[1],PfmDmix1Imm$MAE_CV[1],PfmDmix1Imm$MAE_train[1]))
      } else {
        data.frame("Parameter" =  c("R2 test","R2 cross validation","R2 train","RMSE test","RMSE cross validation","RMSE train","MAE test","MAE cross validation","MAE train"),
                   "Value" = c(PfmDmix9Imm$R2_test[1],PfmDmix9Imm$R2_CV[1],PfmDmix9Imm$R2_train[1], PfmDmix9Imm$RMSE_test[1],PfmDmix9Imm$RMSE_CV[1],PfmDmix9Imm$RMSE_train[1],PfmDmix9Imm$MAE_test[1],PfmDmix9Imm$MAE_CV[1],PfmDmix9Imm$MAE_train[1]))    
      }  
    }, digits = 3)
    
    
  
  
# Table of user input data for predicting Immobilization
  output$TiO2datainputImm <- renderTable({
    data.frame("Parameter" = c("MeOxNP",
                           "TiO2 core diameter",
                           "TiO2 concentration",
                           "Mixed chemical",
                           "Mixed chemical concentration",
                           "Exposure time"),
               "Value" = c(input$MeOx,
                           input$CS,
                           input$C1,
                           input$mixchem,
                           input$C2,
                           input$ET),
               "Unit" = c("none",
                          "nm",
                          paste(greeks("mu"), "g/L", sep=""),
                          "none",
                          paste(greeks("mu"), "g/L", sep=""),
                          "h")
               )
              })
  
# Table of user input data for predicting Immobilization
  output$TiO2datainputEC <- renderTable({
    data.frame("Parameter" = c("MeOxNP",
                               "TiO2 NPs fractrion",
                               "Mixed chemical",
                               "Mixed chemical fraction",
                               "Exposure time"),
               "Value" = c(input$MeOx,
                           input$x1EC50,
                           input$mixchem,
                           1-input$x1EC50,
                           48),
               "Unit" = c("",
                          "",
                          "",
                          "",
                          "h")
    )
  })
  
  
# Table of predicted EC50
  output$PredictEC50 <- renderValueBox({
    a <- which(apply(as.data.frame(DescMOPAC[,2]), 1, function(r) any(r %in% input$mixchem)))
    x1EC50 <- input$x1EC50
    x2EC50 <- 1 - input$x1EC50
    if (input$DmixEC == "Dmix1") {
            table1 <- data.frame("x1"     = x1EC50,
                                 "x2"     = x2EC50,
                                 "HOFmix1"= x1EC50*DescMOPAC[9,4] + x2EC50*DescMOPAC[a,4],
                                 "EEmix1" = x1EC50*DescMOPAC[9,5] + x2EC50*DescMOPAC[a,5],
                                 "HOMOmix1" = x1EC50*DescMOPAC[9,6] + x2EC50*DescMOPAC[a,6],
                                 "LUMOmix1" = x1EC50*DescMOPAC[9,7] + x2EC50*DescMOPAC[a,7],
                                 "MEmix1" = x1EC50*DescMOPAC[9,8] + x2EC50*DescMOPAC[a,8],
                                 "Immobilization" = input$EndpointEC50)
    } else {
            table1 <- data.frame("x1"     = x1EC50,
                                 "x2"     = x2EC50,
                                 "HOFmix9"= sqrt(x1EC50*abs(DescMOPAC[9,4]) * x2EC50*abs(DescMOPAC[a,4])),
                                 "EEmix9" = sqrt(x1EC50*abs(DescMOPAC[9,5]) * x2EC50*abs(DescMOPAC[a,5])),
                                 "HOMOmix9" = sqrt(x1EC50*abs(DescMOPAC[9,6]) * x2EC50*abs(DescMOPAC[a,6])),
                                 "LUMOmix9" = sqrt(x1EC50*abs(DescMOPAC[9,7]) * x2EC50*abs(DescMOPAC[a,7])),
                                 "MEmix9" = sqrt(x1EC50*abs(DescMOPAC[9,8]) * x2EC50*abs(DescMOPAC[a,8])),
                                 "Immobilization" = input$EndpointEC50)
          }
    
    if (input$DmixEC == "Dmix1") {
        EC50_TiO2Dmix <- predict(RFDmix1EC, table1)
    } else {
        EC50_TiO2Dmix <- predict(RFDmix9EC, table1)
            }

    color_TiO2Dmix <- fifelse(EC50_TiO2Dmix <= 1000, "red", 
                    fifelse(EC50_TiO2Dmix <= 4000, "yellow",
                        fifelse(EC50_TiO2Dmix < 10000, "green","green"))) 
    icon_TiO2Dmix <- fifelse(EC50_TiO2Dmix <= 4000, "exclamation-triangle", "fas fa-envira")
    title_TiO2Dmix <- HTML(paste0("EC",tags$sub("50mix"), " = ", round(EC50_TiO2Dmix, digits = 0), " ", greeks("mu"),"g/L" ))
    subtitle_TiO2Dmix <- p("Predicted EC50 of the nano-mixture")
    
    valueBox(title_TiO2Dmix, subtitle_TiO2Dmix, icon = icon(icon_TiO2Dmix), color = color_TiO2Dmix)

  })
  
  
# Table of predicted Immobilization
  output$PredictImm <- renderValueBox({
    a <- which(apply(as.data.frame(DescMOPAC[,2]), 1, function(r) any(r %in% input$mixchem)))
    x1 <- input$C1/(input$C1 + input$C2)
    x2 <- input$C2/(input$C1 + input$C2)
    if (input$DmixImm == "Dmix1") {
      table1 <- data.frame("CS"  = input$CS,
                           "HS"  = input$HS,
                           "Zeta"   = input$Zeta,
                           "SA"  = 6/(4.23*10^6*input$CS*10^-9),
                           "ET"  = input$ET,
                           "x1"     = x1,
                           "x2"     = x2,
                           "HOFmix1"= x1*DescMOPAC[9,4] + x2*DescMOPAC[a,4],
                           "EEmix1" = x1*DescMOPAC[9,5] + x2*DescMOPAC[a,5],
                           "HOMOmix1" = x1*DescMOPAC[9,6] + x2*DescMOPAC[a,6],
                           "LUMOmix1" = x1*DescMOPAC[9,7] + x2*DescMOPAC[a,7],
                           "MEmix1" = x1*DescMOPAC[9,8] + x2*DescMOPAC[a,8],
                           "Immobilization" = input$Endpoint)
    } else {
      table1 <- data.frame("CS"  = input$CS,
                           "HS"  = input$HS,
                           "Zeta"   = input$Zeta,
                           "SA"  = 6/(4.23*10^6*input$CS*10^-9),
                           "ET"  = input$ET,
                           "x1"     = x1,
                           "x2"     = x2,
                           "HOFmix9"= sqrt(x1*abs(DescMOPAC[9,4]) * x2*abs(DescMOPAC[a,4])),
                           "EEmix9" = sqrt(x1*abs(DescMOPAC[9,5]) * x2*abs(DescMOPAC[a,5])),
                           "HOMOmix9" = sqrt(x1*abs(DescMOPAC[9,6]) * x2*abs(DescMOPAC[a,6])),
                           "LUMOmix9" = sqrt(x1*abs(DescMOPAC[9,7]) * x2*abs(DescMOPAC[a,7])),
                           "MEmix9" = sqrt(x1*abs(DescMOPAC[9,8]) * x2*abs(DescMOPAC[a,8])),
                           "Immobilization" = input$Endpoint)
    }
    
    if (input$DmixImm == "Dmix1") {
      Imm_TiO2Dmix <- predict(RFDmix1Imm, table1)
    } else {
      Imm_TiO2Dmix <- predict(RFDmix9Imm, table1)
    }
    
    color_TiO2Dmix <- fifelse(Imm_TiO2Dmix <= 30, "green", 
                              fifelse(Imm_TiO2Dmix <= 50, "yellow",
                                      fifelse(Imm_TiO2Dmix <= 75, "red","red"))) 
    icon_TiO2Dmix <- fifelse(Imm_TiO2Dmix <= 30, "fas fa-envira","exclamation-triangle")
    title_TiO2Dmix <- paste("Immobilization = ", round(Imm_TiO2Dmix, digits = 1), " %", sep = "" )
    subtitle_TiO2Dmix <- p("Immobilization meaning: Percentage of ",em("Daphnia magna")," population which are immobilized")
    
    valueBox(title_TiO2Dmix, subtitle_TiO2Dmix, icon = icon(icon_TiO2Dmix), color = color_TiO2Dmix)
    
  })
  
  
# Make Dmix formula show up in the output panel of EC50
  output$DmixformEC <- renderUI({
    if (input$DmixEC == "Dmix1") {
      withMathJax("$$\\text{}D_{mix} = x_1 \\times{} D_1 + x_2  \\times{} D_2$$")
    } else {
        withMathJax("$$\\text{}D_{mix} = \\sqrt{(x_1 \\times{} D_1 \\times{} x_2  \\times{} D_2)} $$")
          }
  })

# Make Dmix formula show up in the output panel if Immobilization
  output$DmixformImm <- renderUI({
    if (input$DmixImm == "Dmix1") {
      withMathJax("$$\\text{}D_{mix} = x_1 \\times{} D_1 + x_2  \\times{} D_2$$")
    } else {
      withMathJax("$$\\text{}D_{mix} = \\sqrt{(x_1 \\times{} D_1 \\times{} x_2  \\times{} D_2)} $$")
    }
  })
  
}

