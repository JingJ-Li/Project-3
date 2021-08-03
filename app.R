library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
#Define UI part
ui <- dashboardPage(skin="blue",
                    
                    #add title
                    dashboardHeader(title="Project 3-Jingjing Li (Final)",titleWidth=1000),
                    
                    #define sidebar items
                    dashboardSidebar(
                      sidebarMenu(
                                  menuItem("About", tabName = "about", icon = icon("archive")),
                                  menuItem("Data", tabName="data", icon = icon("table")),
                                  menuItem("Data Exploration", tabName="data_explore", icon= icon ("bar-chart-o")),
                                  menuItem("Modeling", tabName = "model", icon = icon("laptop"))
                      )
                    ),
                    
                    #define the body of the app
                    dashboardBody(
                       tags$head(
                          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                        ),
                       tabItems(
                         
                        # First tab content
                         tabItem(tabName = "about",
                                 
                                 #First row of contents
                                 fluidRow(
                                   
                                  #Description of App
                                   column(6,
                                         h1("Introduction of App",style="color:blue;"),
                                         box(width=12,
                                              h3("This app utilizes an interface to allow users to choose parameter to subset, explore, model and make prediction on heart failure-related dataset.")
                                         )
                                  ),
                                  
                                  #Data page introduction
                                  column(6,
                                         h1("Introduction of Data",style="color:blue;"),
                                         #box to contain description
                                         box(width=12,
                                             h3("The data was downloaded from ",
                                                a(href="https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records","UCI Machine Learning Repository"),
                                                ", which is about predicting patients' survival from their medical data."
                                                )
                                         )
                                  )
                                ),
                                
                                #Second row of contents
                                fluidRow(
                                  
                                  #Introduction of Tabs
                                  column(6,
                                       h1("Introduction of Tabs",style="color:blue;"),
                                       box(width=12,
                                           h3("This app consists of four tabs named as 'About', 'Data' ,'Data Exploration' and 'modeling' respectively. 'About' page outlines the app.
                                              'Data' allows a user to subset data by choose different rows and columns. 'Data Exploration' page set up a plateform where a user can 
                                              make numeric summaries and visualize data by choose different variables. 'Modeling' page takes three methods including 'logistic regression',
                                              'tree based classification' and 'random forest' to fit the training data sampled from the original and make comparison on the test data. 
                                              Finally a prediction is made on single values of choosen variables using a predetermined model.")
                                       )
                                  ),
                                
                                  #log 
                                  column(6,
                                       h1("Logo",style="color:blue;"),
                                       box(width=12,
                                           img(src='heart-failure-checklist.png',height = 216, width = 216),
                                           a(href="https://www.americandatanetwork.com/portfolio-item/checklistheart-failure-checklist-how-to-cut-heart-failure-readmissions-from-20-to-2/","link")
                                       )
                                  )
                                )
                        ),
                  
                        # Second tab content
                        tabItem(tabName="data",
                                  column(h3("Parameter choice for subsetting data",style="color:blue;"),
                                         width=3,
                                         box(width=12,
                                             numericInput("mincol", 
                                                          label=("The minimum column number"), 
                                                          min=1,
                                                          max=13, 
                                                          value=1),
                                             numericInput("maxcol", 
                                                          label=("The maximum column number"), 
                                                          min=1,
                                                          max=13, 
                                                          value=13),
                                             numericInput("minrow", 
                                                          label=("The minimum row number"), 
                                                          min=1,
                                                          max=13, 
                                                          value=1),
                                             numericInput("maxrow", 
                                                          label=("The maximum row number"), 
                                                          min=1,
                                                          max=299, 
                                                          value=100)
                                             ),
                                         downloadButton('dataset',"Download the data")
                                    ),
                                    column(h3("Subset Data Table",style="color:blue;"),
                                           width=9,
                                           box(width=12,DT::dataTableOutput("Tab")
                                           )
                                    )  
                        ),
                        
                        # Third tab content
                        tabItem(tabName="data_explore",
                                fluidRow(
                                  column(h3("Datas subsetting parameter input ",style="color:blue;"),
                                         width=4,
                                         box( width=12,          
                                             #Set input choice of variable for numeric summary table
                                             selectInput("var", 
                                                          label=("Variables for subseting data "),
                                                         c("age"= "age", 
                                                           "creatinine_phosphokinase"="creatinine_phosphokinase",
                                                           "diabetes"="diabetes",
                                                            "anaemia"="anaemia",
                                                           "ejection_fraction"= "ejection_fraction",
                                                           "high_blood_pressure"="high_blood_pressure",
                                                           "platelets"="platelets",
                                                            "serum_creatinine"= "serum_creatinine",
                                                            "serum_sodium"=  "serum_sodium",
                                                            "sex"="sex",
                                                            "smoking"="smoking",
                                                            "time"= "time",
                                                            "DEATH_EVENT"= "DEATH_EVENT"
                                                           ) 
                                             ),               
                                            # input variable values in response to chosen variables
                                             conditionalPanel(condition="input.var == 'age'",
                                                             numericInput("age","age",min=0, max=100, value=50)
                                             ),
                                             conditionalPanel(condition="input.var == 'anaemia'",
                                                            numericInput("anaemia","anaemia",min=0, max=1,value=1)
                                              ),                
                                             conditionalPanel(condition="input.var ==  'creatinine_phosphokinase'",                                                                        
                                                             numericInput("creatinine_phosphokinase","creatinine_phosphokinase",min=0, max=8000, value=1000)
                                              ),
                                             conditionalPanel(condition="input.var == 'diabetes'",
                                                             numericInput("diabetes","diabetes",min=0, max=1,value=1)
                                              ),
                                             conditionalPanel(condition="input.var == 'ejection_fraction'",
                                                             numericInput("ejection_fraction","ejection_fraction",min=0, max=70,value=35)
                                              ),
                                              conditionalPanel(condition="input.var == 'high_blood_pressure'",
                                                             numericInput("high_blood_pressure","high_blood_pressure",min=0, max=1,value=1))
                                              ,
                                              conditionalPanel(condition="input.var == 'platelets'",
                                                             numericInput("platelets","platelets",min=0, max=263358.03,value=188000)
                                              ),
                                               conditionalPanel(condition="input.var == 'serum_creatinine'",                                               
                                                             numericInput("serum_creatinine","serum_creatinine",min=0, max=10,value=5)
                                              ),
                                              conditionalPanel(condition="input.var == 'serum_sodium'",     
                                                             numericInput("serum_sodium","serum_sodium",min=0, max=200,value=140)
                                              ),
                                              conditionalPanel(condition="input.var== 'sex'", 
                                                             numericInput("sex","sex",min=0, max=1, value=1)
                                              ),
                                              conditionalPanel(condition="input.var == 'smoking'", 
                                                             numericInput("smoking","smoking",min=0, max=1, value=1)
                                              ),
                                              conditionalPanel(condition="input.var == 'time'",             
                                                             numericInput("time","time",min=0, max=90,value=45)
                                              ),
                                              conditionalPanel(condition="input.var == 'DEATH_EVENT'",             
                                                             numericInput("DEATH_EVENT","DEATH_EVENT",min=0, max=1,value=1)                                                             
                                              ),
                                              #set input variable for summary table
                                              selectInput("svar", 
                                                          label=("Variables for summary table"),
                                                         c("age"= "age", 
                                                           "creatinine_phosphokinase"="creatinine_phosphokinase",
                                                           "diabetes"="diabetes",
                                                            "anaemia"="anaemia",
                                                           "ejection_fraction"= "ejection_fraction",
                                                           "high_blood_pressure"="high_blood_pressure",
                                                           "platelets"="platelets",
                                                            "serum_creatinine"= "serum_creatinine",
                                                            "serum_sodium"=  "serum_sodium",
                                                            "sex"="sex",
                                                            "smoking"="smoking",
                                                            "time"= "time",
                                                            "DEATH_EVENT"= "DEATH_EVENT"
                                                           ) 
                                             ),
                                             
                                             #Set input choice of statistic summary type
                                             selectInput("sumtype", 
                                                        label=("Summary Type"),
                                                         c("Avg"= "mean", 
                                                           "Median"="median",
                                                           "Minimum"="min",
                                                           "Maximum"= "maxi",
                                                           "quantile"="quantile",
                                                           "SD"="sd" 
                                                         )
                                              ),
                                                
                                             # set input choice of quantile parameters upon quantile summary
                                             conditionalPanel(condition="input.sumtype == 'quantile'",
                                                              sliderInput("qvalue",
                                                                          label="quantile input",
                                                                          min=0,
                                                                          max=100,
                                                                          value=25
                                                                          )                                                              
                                             ),
                                             
                                             #set row variable input choice for contingency table
                                             selectInput("var2", 
                                                         label=("Row Variables For Contingency table"),
                                                         c("sex"="sex",
                                                           "high_blood_pressure"="high_blood_pressure",                                                           
                                                           "smoking"="smoking",
                                                           "DEATH_EVENT"= "DEATH_EVENT",
                                                           "creatinine_phosphokinase"="creatinine_phosphokinase",
                                                           "ejection_fraction"= "ejection_fraction",
                                                           "platelets"="platelets",
                                                           "serum_creatinine"= "serum_creatinine",
                                                           "serum_sodium"=  "serum_sodium",
                                                           "time"= "time",
                                                           "Age"= "age_range"
                                                         ) 
                                             ),
                                             
                                             ##set column variable input choice for contingency table
                                             selectInput("var3", 
                                                         label=("Column Variables For Contingency table"),
                                                         c(
                                                           "high_blood_pressure"="high_blood_pressure",
                                                           "sex"="sex",
                                                           "smoking"="smoking",
                                                           "DEATH_EVENT"= "DEATH_EVENT",
                                                           "creatinine_phosphokinase"="creatinine_phosphokinase",
                                                           "ejection_fraction"= "ejection_fraction",
                                                           "platelets"="platelets",
                                                           "serum_creatinine"= "serum_creatinine",
                                                           "serum_sodium"=  "serum_sodium",
                                                           "time"= "time",
                                                           "Age"= "age_range" 

                                                         ) 
                                             )
                                         )
                                     ),
                                   
                                     #Output statistic summary table
                                     column(h3("Statistic Summary Table",style="color:blue;"),
                                            width=4,
                                            box(width=12,
                                                DT::dataTableOutput("Tab2")
                                            )
                                      ),
                                      
                                      #Output Contingency table
                                      column(h3("Contingency Table",style="color:blue;"),
                                             width=4,
                                             box(width=12, 
                                                 DT::dataTableOutput('Tab3')
                                                 
                                             )
                                   ),
                                   #Add a breakline
                                                                                                  
                                  ),
                                  br(),
                                   #set region for plots
                                   fluidRow(
                                     column(width=3,h3("Plot parameter input",style="color:blue;"),
                                            box(width=12,                                                 
                                                # Set plot type input choice
                                                selectInput("plottype", 
                                                           label=("Plot Type"),
                                                           c("Barplot"="bar",
                                                             "Histogram"= "histogram",                                                              
                                                             "Boxplot"="box",
                                                             "Scatterplot"= "point"
                                                            )
                                                 ),
                                                
                                                #set conditional variable input upon histogram and bar plot
                                                conditionalPanel(condition="input.plottype == bar",
                                                             selectInput("xvar1A",
                                                                            h5("X variable for bar",
                                                                               style ="color:grey;"),
                                                                            c(
                                                                              "diabetes"="diabetes",
                                                                              "ejection_fraction"= "ejection_fraction",
                                                                              "high_blood_pressure"="high_blood_pressure",
                                                                              "platelets"="platelets",
                                                                              "serum_creatinine"= "serum_creatinine",
                                                                              "serum_sodium"=  "serum_sodium",
                                                                              "sex"="sex",
                                                                              "smoking"="smoking",
                                                                              "time"= "time",
                                                                              "DEATH_EVENT"= "DEATH_EVENT",
                                                                              "Age"= "age_range", 
                                                                              "creatinine_phosphokinase"="creatinine_phosphokinase"
                                                                            )
                                                               )
                                                  
                                                ),
                                                conditionalPanel(condition="input.plottype == histogram",
                                                             selectInput("xvar1B",
                                                                            h5("X variable for histogram",
                                                                               style ="color:grey;"),
                                                                            c("Age"= "age", 
                                                                              "creatinine_phosphokinase"="creatinine_phosphokinase",                                                                             
                                                                              "ejection_fraction"= "ejection_fraction",                                                                             
                                                                              "platelets"="platelets",
                                                                              "serum_creatinine"= "serum_creatinine",
                                                                              "serum_sodium"=  "serum_sodium",                                                                             
                                                                              "time"= "time",
                                                                              "DEATH_EVENT"= "DEATH_EVENT"
                                                                            )
                                                               )
                                                  
                                                ),
                                                #set conditional variable input as x uponhbox and scatter plot
                                                conditionalPanel(condition="input.plottype == box|point",
                                                                   selectInput("xvar2",
                                                                                  h5("X variable for box/scatter",
                                                                                  style ="color:grey;"),
                                                                                 c("Age"= "age_range", 
                                                                                   "creatinine_phosphokinase"="creatinine_phosphokinase",
                                                                                   "diabetes"="diabetes",
                                                                                    "ejection_fraction"= "ejection_fraction",
                                                                                   "high_blood_pressure"="high_blood_pressure",
                                                                                   "platelets"="platelets",
                                                                                   "serum_creatinine"= "serum_creatinine",
                                                                                   "serum_sodium"=  "serum_sodium",
                                                                                   "sex"="sex",
                                                                                   "smoking"="smoking",
                                                                                   "time"= "time",
                                                                                   "DEATH_EVENT"= "DEATH_EVENT"
                                                                                   )
                                                                               )
                                                                    ),
                                               
                                                #set conditional variable input as y uponhbox and scatter plot
                                                                    selectInput("yvar",
                                                                                   h5("Y variable for box/scatter plot",
                                                                                   style ="color:grey;"),
                                                                                   c( 
                                                                                     "creatinine_phosphokinase"="creatinine_phosphokinase",
                                                                                     "diabetes"="diabetes",
                                                                                     "ejection_fraction"= "ejection_fraction",
                                                                                     "high_blood_pressure"="high_blood_pressure",
                                                                                     "platelets"="platelets",
                                                                                     "serum_creatinine"= "serum_creatinine",
                                                                                     "serum_sodium"=  "serum_sodium",
                                                                                     "sex"="sex",
                                                                                     "smoking"="smoking",
                                                                                     "time"= "time",
                                                                                    "DEATH_EVENT"= "DEATH_EVENT",
                                                                                    "Age"= "age"
                                                                                      )
                                                                     )
                                                )
                                
                                            
                                              ),

                                      # Output plots 
                                      column(h3("Graphic summary", style="color:blue;"),
                                             width=9,
                                            box(width=12, 
                                                plotOutput("displot"),
                                                plotlyOutput("scatterTxt")
                                            ),
                                            # download plot
                                            downloadButton("fig")      
                                      )
                                    
                                    )
                                ),
                        
                                # Forth tab content
                                  tabItem(tabName="model",
                                          tabsetPanel(

                                            #Modeling introduction page
                                            tabPanel( 
                                                     "Modeling Info",
                                                     column(width=4,
                                                           h3("Generalized linear regression model", style="color:blue;"),
                                                           box(width=12,
                                                               fluidRow(width=9,
                                                                       h3("Brief introduction"),
                                                                       h4("Logistic linear gression model is one of generalized linear 
                                                                           regression models, which models the probability of a class
                                                                           response in responding to value change of predictors. A 
                                                                           benefit of logistic model is its values is always between 0 
                                                                           and 1, which is suitable for binomial reponse. In addition,
                                                                           it can be easily interpreted. Disadvantage include that 
                                                                           response has to be converted into logistic form and no straigt
                                                                           linear relationship  between predictors and response. On the
                                                                           other hand, it can easily lead to overfit."  )
                                                                ),
                                                               fluidRow(width=9,
                                                                     withMathJax(),
                                                                     h3("Formula"),
                                                                      p("\\(\\log(\\frac{P}{1-P})=\\beta_0+\\beta_1*x+\\beta_2*x_2+...+\\beta_p*x_p\\)"),
                                                                      "where P denotes probability, ", 
                                                                       span("\\(x_i\\)")," predictors,",
                                                                       span("\\(\\beta_0\\)")," the intercept,",
                                                                       span("\\(\\beta_i\\)"),"slops" 
                                                               )
                                                            )                                                           
                                                    ),
                                                    column(width=4,
                                                           h3("Classification tree", style="color:blue;"),
                                                           box(width =12,
                                                               fluidRow(width=12,
                                                                    h3("Brief introduction"),
                                                                    h4("Tree model is kind of method that splits predictor space into regions and assigns 
                                                                       different predictors into each region.Classification aims to classify group membership
                                                                       using tree method. The advantages include easy understanding and interpretion, no need 
                                                                       to scale predictors,built-in variable selection and no statistical assumption. 
                                                                       The disadvantages comprise larege variation and frequent prune requirement" 
                                                                       )                                                                    
                                                               ),
                                                               fluidRow(width=12,
                                                                    h3("Formula"),
                                                                    p("\\(Gini: 2p(1-p)\\)"),
                                                                    "where Gini is the index, P denotes probability of correct classification", 
                                                                    "Alternatively" , 
                                                                    p("\\(Deviance: −2plog(p) − 2(1 − p)log(1 − p)\\)")
                                                                )
                                                            ),                                                           
                                                    ),
                                                    column(width=4,
                                                           h3("Random forest model", style="color:blue;"),
                                                           box(width=12,
                                                               fluidRow(width=12,
                                                                        h3("Brief introduction"),
                                                                        h4("Random forest is ensemble tree method, which involves in bootstrap sampling. An
                                                                            advantage of random forest is that it can randomly select predictors avoiding the
                                                                            domiance of very few predictors which is often seen in bagging model. A disadvantage 
                                                                            is the difficulty to intepret the model.")
                                                                ),
                                                               fluidRow(width=12,
                                                                    h3("Formula"),
                                                                    "selection of predictors:" ,
                                                                    p("\\(m=\\sqrt{p}\\)")
                                                                )
                                                           ),                                                           
                                                     )
                                             ),
                                            tabPanel("Modeling Fit",
                                                     column( "Parameter Input",
                                                             width=3,
                                                             box(width=12,           
                                                                 #Set sampling size
                                                                 sliderInput("slider",
                                                                             label="Train data portion",
                                                                             min=0.1,
                                                                             max=0.9,
                                                                             value=0.6
                                                                            ),
                                                                 #set variable input choice for modeling
                                                                 uiOutput("independent"),
                                                                 numericInput("cp1",
                                                                              label="treeMod cp1",
                                                                              min=0,
                                                                              max=1,
                                                                              value=0
                                                                  ),
                                                                  #set start but to launch the modeling
                                                                  actionButton(
                                                                              "start",
                                                                              label="start"
                                                                  )
                                                             )
                                                    ) ,
                                                    
                                                    #Modeling subpage
                                                    column("modeling results",
                                                           width=3,
                                                           verbatimTextOutput("linMod"),
                                                           
                                                           #set tuning parameters for different models
                                                           fluidRow(
                                                             width=12,
                                                             
                                                             #set tuning parameter for tree classification model
                                                             numericInput("cp2",
                                                                          label="treeMod cp2",
                                                                          min=0,
                                                                          max=12,
                                                                          value=0.05
                                                             ),
                                                             numericInput("cp3",
                                                                          label="treeMod cp3",
                                                                          min=0,
                                                                          max=1,
                                                                          value=0.003
                                                             ),                                                             
                                                             #set tuning parameter for random forest model
                                                             numericInput("mtry1",
                                                                          label="rfeMod mtry1",
                                                                          min=0,
                                                                          max=12,
                                                                          value=3
                                                             ),
                                                             numericInput("mtry2",
                                                                          label="rfeMod mtry2",
                                                                          min=0,
                                                                          max=12,
                                                                          value=6
                                                             ),
                                                             numericInput("mtry3",
                                                                          label="rfeMod mtry3",
                                                                          min=0,
                                                                          max=12,
                                                                          value=12
                                                             ),
                                                           )
                                                    ),

                                                    #Output modeling results
                                                    column(width=3,
                                                           verbatimTextOutput("treeMod")
                                                    ),
                                                    column(width=3,
                                                            verbatimTextOutput("rfMod")
                                                    ),
                                                    verbatimTextOutput("sumMisclass")
                                              ), 
                                            
                                              #Predict subpage
                                              tabPanel("Prediction",
                                                       column(width=3,
                                                            #Choose model type
                                                             selectInput("modtype2",
                                                                          label="Model Type",
                                                                          c("Linear Regression"="glm",
                                                                             "Tree classification"="rpart",
                                                                             "Random forest"="random_forest"
                                                                          )
                                                              ),
                                                              #Define the range of variable value for prediction
                                                             numericInput("age","age",min=0, max=100, value=50),
                                                             numericInput("anaemia","anaemia",min=0, max=1,value=1),
                                                             numericInput("creatinine_phosphokinase","creatinine_phosphokinase",min=0, max=8000, value=1000),
                                                             numericInput("diabetes","diabetes",min=0, max=1,value=1),
                                                             numericInput("ejection_fraction","ejection_fraction",min=0, max=70,value=35),
                                                             numericInput("high_blood_pressure","high_blood_pressure",min=0, max=1,value=1),
                                                             numericInput("platelets","platelets",min=0, max=263358.03,value=188000),
                                                             numericInput("serum_creatinine","serum_creatinine",min=0, max=10,value=5),
                                                            ), 
                                                      column(width=3, 
                                                            numericInput("serum_sodium","serum_sodium",min=0, max=200,value=140),
                                                            numericInput("sex","sex",min=0, max=1, value=1),
                                                            numericInput("smoking","smoking",min=0, max=1, value=1),
                                                            numericInput("time","time",min=0, max=90,value=45),
 
                                                            #Set start button to launch prediction 
                                                            actionButton(
                                                              "predict",
                                                              label="predict"
                                                            )
                                                      ),
                                                      
                                                      #Output prediction results
                                                      column(width=3,  
                                                            verbatimTextOutput("pred")     
                                                      )                                                       
                                               )
                                          )                                                 
                                  )
                       )                      
                    )
) 
                       
# Define server  
server <- shinyServer(function(input, output) {
    # read in data and convert forms
    heartData <- read_csv("heart_failure_clinical_records_dataset.csv")
    heartData$DEATH_EVENT<-as.factor(heartData$DEATH_EVENT)
    heartData$age_range <- cut(heartData$age, c(0,40,50,60,70,80,100) )
    heartData2 <- mutate(heartData, age_range)
    heartData2$anaemia <-as.factor(heartData2$anaemia)
    heartData2$high_blood_pressure <-as.factor(heartData2$high_blood_pressure)
    heartData2$sex <-as.factor(heartData2$sex)
    heartData2$smoking  <-as.factor(heartData2$smoking)
    heartData2$DEATH_EVENT<-as.factor(heartData2$DEATH_EVENT)
    
  #subset data and activate
  get_tab <- reactive({
    mincol <- input$mincol
    maxcol <- input$maxcol
    minrow <- input$minrow
    maxrow <- input$maxrow
    data.frame(heartData[minrow:maxrow, mincol:maxcol])
  })
  
  #output subset table
  output$Tab <- DT::renderDataTable (
    get_tab (),
    options = list(scrollX = TRUE)
  )
  #Download subset table
  output$dataset<- downloadHandler(
  filename = "Tab.csv", 
   content = function(file){
    write.csv(get_tab(), file, row.names = FALSE)
   }
  )
   output$filter <- renderUI({
     checkboxGroupInput("filter", "filter Variable", names(heartData))
   })

  hrtdata <- reactive( 
    
   if (input$var=='age'){  
     filter(heartData2,heartData2$age == input$age)
     } else if (input$var=='anaemia'){ 
         filter(heartData2,heartData2$anaemia == input$anaemia)      
          } else if (input$var=='creatinine_phosphokinase'){ 
          filter(heartData2,heartData2$creatinine_phosphokinase == input$creatinine_phosphokinase) 
          } else if (input$var=='diabetes'){ 
          filter(heartData2,heartData2$diabetes == input$diabetes)
          } else if (input$var=='ejection_fraction'){ 
          filter(heartData2,heartData2$ejection_fraction == input$ejection_fraction)
          } else if (input$var=='high_blood_pressure'){ 
          filter(heartData2,heartData2$high_blood_pressure == input$high_blood_pressure)
          } else if (input$var=='platelets'){ 
          filter(heartData2,heartData2$platelets == input$platelets)
          } else if (input$var=='serum_creatinine'){ 
          filter(heartData2,heartData2$serum_creatinine== input$serum_creatinine)
          } else if (input$var=='serum_sodium'){ 
          filter(heartData2,heartData2$serum_sodium== input$serum_sodium)
          } else if (input$var=='smoking'){ 
          filter(heartData2,heartData2$smoking== input$smoking)
          } else if (input$var=='time'){ 
          filter(heartData2,heartData2$time== input$time)
          } else if (input$var=='sex'){ 
          filter(heartData2,heartData2$sex == input$sex)      
          }   
          else if (input$var=='DEATH_EVENT'){ 
          filter(heartData2,heartData2$DEATH_EVENT == input$DEATH_EVENT)      
          }    
  )

  #output numeric statistic summary table
  output$Tab2 <- DT::renderDataTable ({
    var <-input$svar
    if (input$sumtype =="mean") {data.frame(var,round(summarise (hrtdata(), Avg=mean(hrtdata()[[var]])),2))}
      else if (input$sumtype =="median") {data.frame(var,round(summarise (hrtdata(),Median=median(hrtdata()[[var]])),2))}
        else if (input$sumtype =="min") {data.frame(var,round(summarise (hrtdata(),Min=min(hrtdata()[[var]])),2))}
          else if (input$sumtype =="maxi") {data.frame(var,round(summarise (hrtdata(),Maxi=max(hrtdata()[[var]])),2))}
            else if (input$sumtype =="quantile") {data.frame(var,round(summarise (hrtdata(),Quantile=quantile(hrtdata()[[var]],probes=input$qvalue/100)),2))}
           else if (input$sumtype =="sd") {data.frame(var,round(summarise (hrtdata(),SD=sd(hrtdata()[[var]])),2))}
  })
  
  #Create a contingency table
  output$Tab3 <- DT::renderDataTable({
    validate(need(input$var2,''),
             need(input$var3,'')
             )
    addmargins(xtabs(as.formula(paste0("~",input$var2,"+",input$var3)), hrtdata()))

   })
  
  #create plots and make plots based on one variable and different variables respectively 
  plot <- reactive({
    if (input$plottype =="bar" ) {
      g <- ggplot (data=hrtdata(), aes(x=as.factor(hrtdata()[[input$xvar1A]])))
      g + geom_bar()+xlab(input$xvar1A) + ggtitle(paste("Counts of different", input$xvar1A))    
    } else if (input$plottype=="histogram" ) { 
      g <- ggplot (data=hrtdata(), aes(x=hrtdata()[[input$xvar1B]]))
      g+ geom_histogram()+xlab(input$xvar1B) + ggtitle(paste("Histogram of ", input$xvar1B))  
    } else if (input$plottype =="box" ) {
      g <- ggplot (data=hrtdata(), aes(x=as.factor(hrtdata()[[input$xvar2]]), y=hrtdata()[[input$yvar]]))
      g + geom_boxplot()+geom_jitter(alpha=0.5)+xlab(input$xvar2)+ylab(input$yvar) + ggtitle(paste("Boxplot of ",  input$xvar2, "vs",input$yvar))      
    }
  })
  
  #Output plot
  output$displot <- renderPlot({
    plot()     
  })
  
  #Output a scatter plot with hovering text  
  output$scatterTxt <- renderPlotly({
    x=hrtdata()[[input$xvar2]]
    y=hrtdata()[[input$yvar]]
    if (input$plottype =="point"){
       ggplotly( ggplot (data=hrtdata(), aes(x, y))
       + geom_point()+ xlab(input$xvar2)+ylab(input$yvar))      
    } 
  })

  #Save a plot 
  output$fig = downloadHandler(
    filename = 'figure.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot(), device = device)
    })
  
  #Set data reactive for sampling
  InputData <- reactive({
    heartData 
  })
  
  #Set sampling index for train data
  set.seed(20)
  trainRowIndex <- reactive ({
    sampleNum <- input$slider
    sample(1:nrow(InputData()), size=nrow(InputData())*sampleNum)
  })

  #Subsett training data
  trainData <- reactive({
    subsetData <- heartData[trainRowIndex(), ]    
  })
  
  #Subsett test data
  testData <- reactive({
     testset  <- heartData[-trainRowIndex(), ]
  })
  
  #Set independent variables for choice input
   output$independent <- renderUI({
     checkboxGroupInput("independent", "Independent Variable", names(InputData()))
   })
   
  #Set Cross-Validation parameter
  trCntrl <- trainControl(method="repeatedcv", number=10, repeats=5)

  #Fit training data using logistic regression model
  linMod <-  reactive({
    if (input$start) {
        withProgress(message ='Linear regression Modeling', value=1, {
         train(
          as.formula(paste("DEATH_EVENT"," ~ ", paste(input$independent,collapse="+"))), 
          data=trainData(), 
          method="glm",
          family="binomial",
          trControl=trCntrl,
          na.action = na.exclude
          ) 
          
        })
    }
  })
  
  #Fit training data using tree classification model
  treeMod <- reactive({
        if (input$start) { 
          withProgress(message ='Tree Classification Modeling', value=1, {train(
          as.formula(paste("DEATH_EVENT"," ~ ", paste(input$independent,collapse="+"))), 
          data=trainData(), 
          method="rpart",
          preProcess=c("center","scale"),
          trControl=trCntrl,
          na.action = na.exclude,
          tuneGrid=expand.grid(cp = seq(input$cp1, input$cp2, input$cp3))
          )
          })
        }
  })
  
  #Fit training data using random forest model
  rfMod <- reactive({
    if (input$start) {
      withProgress(message ='Random Forest Modeling', value=1, {train (
          as.formula(paste("DEATH_EVENT"," ~ ", paste(input$independent,collapse="+"))),  
          data=trainData(),
          method = "rf",
          trControl =  trCntrl,
          preProcess = c("center", "scale"),
          tuneGrid=expand.grid(mtry=seq(input$mtry1,input$mtry2, input$mtry3)),
          na.action = na.exclude
        )
      })
     }
  })
  
  #Output logistic regression model result
  output$linMod <- renderPrint({
    linMod()
  })
  
  #Output tree classification model result
  output$treeMod <- renderPrint({
      treeMod()
  })
  
  #Output random forest model result
  output$rfMod <- renderPrint({
      rfMod()
  })
  
  #Compare three models using summarized prediction results upon running on test data
  sumMisclass <- reactive (
    if (input$start){
    pred_LM <- predict(linMod(), newdata=testData())
    pred_TM <- predict(treeMod(), newdata=testData() )
    pred_RM <- predict(rfMod(), newdata=testData() )
    LM <-confusionMatrix(pred_LM, testData()$DEATH_EVENT)
    TM <-confusionMatrix(pred_TM, testData()$DEATH_EVENT)
    RM <-confusionMatrix(pred_RM, testData()$DEATH_EVENT)    
    mis_LM <- round (1- sum(diag(LM$table))/sum(LM$table),3)
    mis_TM <- round(1- sum(diag(TM$table))/sum(LM$table),3)
    mis_RM <- round(1- sum(diag(RM$table))/sum(LM$table),3)
    misclassDf <- data.frame(c("linMod","treeMod","rfMod"),c(mis_LM,mis_TM, mis_RM))
    rename(misclassDf, "Model"=c..linMod....treeMod....rfMod..,"Misclass"=c.mis_LM..mis_TM..mis_RM.)    
  })

  #Output misclass values of three models
  output$sumMisclass <- renderPrint({
    sumMisclass()
  })

  #Generate data set from single value of each variable for modeling and facilitate it reactive
  inputData2<- reactive({data.frame(age=input$age,anaemia=input$anaemia,creatinine_phosphokinase=input$creatinine_phosphokinase, 
                                    diabetes=input$diabetes, ejection_fraction=input$ejection_fraction,high_blood_pressure=input$high_blood_pressure,
                                    platelets=input$platelets,serum_creatinine=input$serum_creatinine,
                                    serum_sodium=input$serum_sodium,sex=input$sex, smoking=input$smoking, time=input$time)
  })

  #Predict the response from input variable values
  pred <- reactive( if (input$predict) {    
    if (input$modtype2 == "glm"){
       predict(linMod(), newdata=inputData2())
    } else if (input$modtype2 == "rpart"){
      predict(treeMod(), newdata=inputData2())
    } else if (input$modtype2 == "rf"){
      predict(rfMod(), newdata=inputData2())
    }    
  })

  #Output prediction result
  output$pred <- renderPrint(if (input$predict) {
    pred()
  })   
}) 

shinyApp(ui = ui, server = server)
