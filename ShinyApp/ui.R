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
library(shinyMobile)
library(charpente)
#############################################

#Load data and trained model
load("www/PfmDmix1.Rdata")
load("www/PfmDmix9.Rdata")
load("www/RFDmix1.Rdata")
load("www/RFDmix9.Rdata")
load("www/DescMOPAC.Rdata")


ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "QSAR for TiO2 based nano-mixtures", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Model TiO2 Daphnia magna", tabName = "Model4", icon = icon("desktop"))
      )
  ),
  dashboardBody(
      # Model 4 tab content
      tabItem(tabName = "Model4",
              # Summary of model
              column(
                width = 12,
                p(strong("Predicting accute totoxicity of TiO2-based nano-mixtures to",em("Daphnia magna")),
                  style="font-size:30px;
                          text-align:justify;
                          color:black;
                          background-color:papayawhip;
                          padding:15px;
                          border-radius:10px"),
                p(strong("Author:"), "Tung X. Trinh, Tae Hyun Yoon and Jongwoon Kim*",
                  style="font-size:16px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px"),
                p(strong("Summary of model:"), "This model predicts immobilization (%) of ", em("Daphnia magna"), "exposed to binary mixtures of TiO2 oxide (MeOx) nanoparticles and a mixed chemical
                (i.e., Pirimicarb, Triton X-100, ZnCl2, AgNO3, Cd(NO3)2, CdCl2, Cu(NO3)2, CuSO4, Na2HAsO4 and Pentabromodiphenyl Ether). 
                  When users input data by choosing values of parameters in the left panel (Model input), 
                  the model will predict immobilization value and show in the right panel (Model output). 
                  Higher immobilization value means higher toxicity to" , em("Daphnia magna."),
                  "The model based on random forest algorithm which was built by using package \"caret\" in R.
                  ",
                  style="font-size:16px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px")
              ),
              
              
              # Model input and output
              fluidRow(
                # Model input
                box(
                  width = 6,
                  height = 1300,
                  title = p(strong("Model input:"),
                            style="font-size:24px;
                          text-align:justify;
                          color:black;
                          background-color:papayawhip;
                          padding:15px;
                          border-radius:10px"),
                  p(strong("User input data:"),
                    style="font-size:20px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px"),
                  selectInput(inputId = "MeOx", "Metal oxide NPs:",
                              c("NanoTiO2" = "NanoTiO2")),
                  sliderInput(inputId = "CSize", label = "Core size (nm) of TiO2 nanoparticles:", 
                              value = 25, min = 5, max = 100, step = 0.1),
                  sliderInput(inputId = "HSize", label = "Hydrodynamic size (nm) of TiO2 nanoparticles:", 
                              value = 100, min = 10, max = 1000, step = 0.1),
                  sliderInput(inputId = "Zeta", label = "Zeta potential (mV) of TiO2 nanoparticles:", 
                              value = -15, min = -20, max = 0, step = 0.1),
                  selectInput(inputId = "mixchem", "Mixed chemicals:",
                              c("Pirimicarb" = "Pirimicarb",
                                "TritonX100" = "TritonX100",
                                "ZnCl2" = "ZnCl2",
                                "AgNO3" = "AgNO3",
                                "Cd(NO3)2" = "CdN2O6",
                                "CdCl2" = "CdCl2",
                                "Cu(NO3)2" = "CuN2O6",
                                "CuSO4" = "CuSO4",
                                "Na2HAsO4" = "Na2HAsO4",
                                "Pentabromodiphenyl Ether" = "Pentabromodiphenylether"
                                )),
                  sliderInput(inputId = "C1", label = "Concentration (ug/L) of TiO2 nanoparticles:", 
                              value = 100, min = 0.1, max = 10000, step = 1), 
                  sliderInput(inputId = "C2", label = "Concentration (ug/L) of the mixed chemical:", 
                              value = 5, min = 0.1, max = 300, step = 1),
                  sliderInput(inputId = "ETime", label = "Exposure time (h):", 
                              value = 48, min = 1, max = 96),
                  sliderInput(inputId = "Endpoint", label = "Expected toxicity - Immobilization (%):", 
                              value = 10, min = 0, max = 100), 
                  p(strong("Summary of input data:"),
                    style="font-size:16px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px"),
                  tableOutput("TiO2datainput")
                ),
                # Model output
                box(
                  width = 6,
                  height = 1300,
                  title = p(strong("Model output:"),
                            style="font-size:24px;
                          text-align:justify;
                          color:black;
                          background-color:papayawhip;
                          padding:15px;
                          border-radius:10px"),
                  
                  p(strong("Predicted toxicity:"),
                    style="font-size:20px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px"),
                  valueBoxOutput("Prediction", width = 12),
                  
                  column(tags$img(src="TiO2Dmagna.png",height="200px"),width=12),
                  
                  selectInput(inputId = "MixtureDescriptor", "Type of mixture descriptors",
                              c("Additive" = "Additive",
                                "None Additive" = "Non_Additive"
                              )),
                  #headerPanel(withMathJax("$$\\text{Display formula in heading }X_n=X_{n-1}-\\varepsilon$$")),
                  
                  column(
                    p(strong("Mixture descriptor formula: "),
                      style="font-size:16px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px"),
                    uiOutput("Dmixform"),
                    width=12),
                  
                  column(
                    p(strong("Performance of the predictive model: "),
                      style="font-size:16px;
                          text-align:justify;
                          color:black;
                          background-color:white;
                          padding:15px;
                          border-radius:10px"),
                    
                  tableOutput("PerformanceDmix1"),width=12),
                  
                  
                ),
                
                column(
                  br(),
                  p( strong("Acknowledgement:"),"This work was funded by the Korea Research Institute of Chemical Technology (KRICT) through the Development of Chemical Safety Platform Technologies (Project No. KK2052-10). J. Kim and T.H. Yoon acknowledge the support from the European Union's Horizon 2020 research and innovation program (SABYDOMA Project under Grant Agreement No. 862296).", style="font-size:16px;text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                  width=12), 
              )
              
      ),

      
    )
  )
