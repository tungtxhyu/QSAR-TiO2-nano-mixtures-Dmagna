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
#packages <- c("caret", "randomForest", "data.table", "shiny", "shinythemes", "shinydashboard", "remotes")
#ipak(packages)
#remotes::install_github("RinteRface/charpente")


# Load library
library(caret) 
library(greekLetters) 
library(randomForest)
library(data.table)
library(shiny)
library(shinythemes)
library(shinydashboard)
#############################################

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "QSAR for nano-mixtures", titleWidth = 300),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(
                                  menuItem("Predict EC50", tabName = "Model1", icon = icon("desktop")),
                                  menuItem("Predict Immobilization", tabName = "Model2", icon = icon("desktop"))
                                )
                                ),
                    dashboardBody(
                                  tabItems(
                        
                        # Begin of tab 1
                        tabItem(tabName = "Model1",
                                
                                # Model 1 (EC50)
                                column(
                                      width = 12,
                                      p(strong("Predicting ", HTML(paste0("EC",tags$sub("50mix")))," of ",HTML(paste0("TiO",tags$sub("2"))),"-based nano-mixtures"),
                                        style="font-size:30px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"
                                        ),
                                      p(strong("Author:"), "Tung X. Trinh, Myungwon Seo, Tae Hyun Yoon and Jongwoon Kim*",
                                        style="font-size:16px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                                        ),
                                      p(strong("Summary of model:"), 
                                           "This model predicts ",HTML(paste0("EC",tags$sub("50mix")))," value of binary mixtures of ",HTML(paste0("TiO",tags$sub("2")))," nanoparticles and a mixed chemical
                                            (i.e.,",
                                              HTML(paste0("AgNO",tags$sub("3"))),
                                              ", Benzylparaben, Benzophenone-3",
                                              HTML(paste0(", Cd(NO",tags$sub("3"),")",tags$sub("2"))),
                                              HTML(paste0(", Cu(NO",tags$sub("3"),")",tags$sub("2"))),
                                              HTML(paste0(", CuSO",tags$sub("4"))),
                                              HTML(paste0(", Na",tags$sub("2"),"HAsO",tags$sub("4"))),
                                              HTML(paste0(", and NaAsO",tags$sub("2"))),
                                           ". The model based on random forest algorithm which was built by using package \"", em("caret"), "\" in R.",
                                        style="font-size:16px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                                        )
                                      ),
                                
                                # Model input and output
                        fluidRow(
                          # Model input
                          box(
                            width = 6,
                            height = 1300,
                            title = p(strong("Model input:"),
                                      style="font-size:24px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"
                                      ),
                            p(strong("User input data:"),
                              style="font-size:20px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                              ),
                            selectInput(inputId = "MeOx", "Metal oxide NPs:",
                                        c("NanoTiO2" = "NanoTiO2")),
                            selectInput(inputId = "mixchem", "Mixed chemicals:",
                                        c("AgNO3" = "AgNO3",
                                          "Benzylparaben" = "Benzylparaben",
                                          "Benzophenone-3" = "Benzophenone-3",
                                          "Cd(NO3)2" = "CdN2O6",
                                          "Cu(NO3)2" = "CuN2O6",
                                          "CuSO4" = "CuSO4",
                                          "Na2HAsO4" = "Na2HAsO4",
                                          "NaAsO2" = "NaAsO2"
                                        )),
                            sliderInput(inputId = "x1EC50", label = "Concentration fraction of TiO2 NPs:", 
                                        value = 0.5, min = 0.001, max = 1, step = 0.001), 
                            sliderInput(inputId = "EndpointEC50", label = "Expected EC50mix (ug/L):", 
                                        value = 1000, min = 0, max = 10000), 
                            
                            p(strong("Summary of input data:"),
                              style="font-size:16px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                            ),
                            tableOutput("TiO2datainputEC")
                            
                            ),
                          # Model output
                          box(
                            width = 6,
                            height = 1300,
                            title = p(strong("Model output:"),
                                      style="font-size:24px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"),
                            p(strong("Predicted toxicity:"),
                              style="font-size:20px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                              ),

                            valueBoxOutput("PredictEC50", width = 12),
                            column(tags$img(src="TiO2Dmagna_2.png",height="200px"),width=12),
                            
                            selectInput(inputId = "DmixEC", "Type of mixture descriptors",
                                        c("Dmix1" = "Dmix1",
                                          "Dmix2" = "Dmix2"
                                        )),
                            column(
                              p(strong("Mixture descriptor formula: "),
                                style="font-size:16px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"),
                              uiOutput("DmixformEC"),
                              width=12),
                            column(
                              p(strong("Performance of the predictive model: "), 
                                style="font-size:16px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                                ),
                              tableOutput("PerformanceDmixEC"),
                              width=12),
                            
                            ),
                                                      
                          column(
                            br(),
                            p( strong("Acknowledgement:"),"This work was funded by the Korea Research Institute of Chemical Technology (KRICT) through the Development of Chemical Safety Platform Technologies (Project No. KK2052-10). J. Kim and T.H. Yoon acknowledge the support from the European Union's Horizon 2020 research and innovation program (SABYDOMA Project under Grant Agreement No. 862296).", style="font-size:16px;text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                            width=12), 
                            ),
                        
                        ), 
                        # End of tab 1
                        
                        
                        
                        # Begin of tab 2
                                # Model 2 (Immobilization)
                        tabItem(tabName = "Model2",
                                        # Summary of model
                                  column(
                                              width = 12,
                                              p(strong("Predicting accute toxicity of ",HTML(paste0("TiO",tags$sub("2"))), "-based nano-mixtures to",em("Daphnia magna")),
                                                style="font-size:30px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"
                                                ),
                                              p(strong("Author:"), "Tung X. Trinh, Myungwon Seo,  Tae Hyun Yoon and Jongwoon Kim*",
                                                style="font-size:16px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                                                ),
                                              p(strong("Summary of model:"), 
                                                        "This model predicts immobilization (%) of ", em("Daphnia magna"), "exposed to binary mixtures of ",HTML(paste0("TiO",tags$sub("2"))), "nanoparticles and a mixed chemical
                                                        (i.e., ", 
                                                        HTML(paste0("AgNO",tags$sub("3"))),
                                                        HTML(paste0(", Cd(NO",tags$sub("3"),")",tags$sub("2"))),
                                                        HTML(paste0(", Cu(NO",tags$sub("3"),")",tags$sub("2"))),
                                                        HTML(paste0(", CuSO",tags$sub("4"))),
                                                        HTML(paste0(", Na",tags$sub("2"),"HAsO",tags$sub("4"))),
                                                        HTML(paste0(", NaAsO",tags$sub("2"))),
                                                        ", Benzophenone-3, Benzylparaben, Pentabromodiphenyl Ether, Pirimicarb, and Triton X-100)." ,
                                                        "The model based on random forest algorithm which was built by using package \"", em("caret"), "\" in R.", 
                                                style="font-size:16px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                                                )
                                              ),
                                  
                                        
                                        # Model input and output
                                        fluidRow(
                                          # Model input
                                          box(
                                            width = 6,
                                            height = 1300,
                                            title = p(strong("Model input:"),
                                                      style="font-size:24px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"
                                                      ),
                                                    p(strong("User input data:"),
                                                      style="font-size:20px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                                                      ),
                                            selectInput(inputId = "MeOx", "Metal oxide NPs:",
                                                        c("NanoTiO2" = "NanoTiO2")),
                                            sliderInput(inputId = "CS", label = "Core size (nm) of TiO2 nanoparticles:", 
                                                        value = 20, min = 5, max = 100, step = 0.1),
                                            sliderInput(inputId = "HS", label = "Hydrodynamic size (nm) of TiO2 nanoparticles:", 
                                                        value = 100, min = 10, max = 1000, step = 0.1),
                                            sliderInput(inputId = "Zeta", label = "Zeta potential (mV) of TiO2 nanoparticles:", 
                                                        value = -15, min = -20, max = 10, step = 0.1),
                                            selectInput(inputId = "mixchem", "Mixed chemicals:",
                                                        c("AgNO3" = "AgNO3",
                                                          "Cd(NO3)2" = "CdN2O6",
                                                          "Cu(NO3)2" = "CuN2O6",
                                                          "CuSO4" = "CuSO4",
                                                          "Na2HAsO4" = "Na2HAsO4",
                                                          "NaAsO2" = "NaAsO2",
                                                          "Benzophenone-3" = "Benzophenone-3",
                                                          "Benzylparaben" = "Benzylparaben",
                                                          "Pentabromodiphenyl Ether" = "PentabromodiphenylEther",
                                                          "Pirimicarb" = "Pirimicarb",
                                                          "Triton X-100" = "TritonX100"
                                                          
                                                        )),
                                            sliderInput(inputId = "C1", label = "Concentration (ug/L) of TiO2 nanoparticles:", 
                                                        value = 100, min = 0.1, max = 10000, step = 1), 
                                            sliderInput(inputId = "C2", label = "Concentration (ug/L) of the mixed chemical:", 
                                                        value = 5, min = 0.1, max = 300, step = 1),
                                            sliderInput(inputId = "ET", label = "Exposure time (h):", 
                                                        value = 48, min = 24, max = 96, step = 24),
                                            sliderInput(inputId = "Endpoint", label = "Expected toxicity - Immobilization (%):", 
                                                        value = 10, min = 0, max = 100), 
                                            p(strong("Summary of input data:"),
                                                style="font-size:16px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                                              ),
                                            tableOutput("TiO2datainputImm")
                                          ),
                                          # Model output
                                          box(
                                            width = 6,
                                            height = 1300,
                                            title = p(strong("Model output:"),
                                                      style="font-size:24px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"),
                                                    p(strong("Predicted toxicity:"),
                                                      style="font-size:20px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                                                      ),
                                            valueBoxOutput("PredictImm", width = 12),
                                            
                                            column(tags$img(src="TiO2Dmagna.png",height="200px"),width=12),
                                            
                                            selectInput(inputId = "DmixImm", "Type of mixture descriptors",
                                                        c("Dmix1" = "Dmix1",
                                                          "Dmix2" = "Dmix2"
                                                        )),
                                            
                                            
                                            column(
                                              p(strong("Mixture descriptor formula: "),
                                                style="font-size:16px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"),
                                              uiOutput("DmixformImm"),
                                              width=12),
                                            
                                            column(
                                              p(strong("Performance of the predictive model: "),
                                                style="font-size:16px; text-align:justify; color:black; background-color:white; padding:15px; border-radius:10px"
                                                ),
                                              tableOutput("PerformanceDmixImm"),
                                              width=12),
                                              
                                              ),
                                          
                                          column(
                                            br(),
                                            p( strong("Acknowledgement:"),"This work was funded by the Korea Research Institute of Chemical Technology (KRICT) through the Development of Chemical Safety Platform Technologies (Project No. KK2052-10). J. Kim and T.H. Yoon acknowledge the support from the European Union's Horizon 2020 research and innovation program (SABYDOMA Project under Grant Agreement No. 862296).", style="font-size:16px;text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                                            width=12), 
                                        )
                                        
                                )
                        # End of tab 2
                        
                        
                      )
                    )
)