library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(dplyr)
library(ggplot2)
library(caret)

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
                       tabItems(
                        # First tab content
                         tabItem(tabName = "about",
                                 fluidRow(
                                  #two columns for each of the two items
                                   column(6,
                                         #Description of App
                                         h1("Introduction of App"),
                                         #box to contain description
                                         box(width=12,
                                             h2("This app aims at allowing users to choose data for subesting, exploration, modeling and prediction.")
                                         )
                                  ),
                                  column(6,
                                         #How to use the app
                                         h1("Introduction of Data"),
                                         #box to contain description
                                         box(width=12,
                                             h2("The data was downloaded from ",
                                                a(href="https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records","UCI Machine Learning Repository"),
                                                ", which is about predicting patients' survial from their medical data."
                                                )
                                         )
                                  )
                                ),
                                fluidRow(
                                  column(6,
                                       #Description of App
                                       h1("Introduction of Tabs"),
                                       #box to contain description
                                       box(background="light-blue",width=12
                                       )
                                  ),
                                
                                  column(6,
                                       #How to use the app
                                       h1("Logo"),
                                       #box to contain description
                                       box(background="light-blue",width=12
                                       )
                                  )
                                )
                        ),
                  
                        tabItem(tabName="data",
                                fluidRow(
                                  column(width=3,
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
                                                          value=13)
                                             ),
                                         downloadButton('dataset',"Download the data")
                                    ),
                                    column(width=9,
                                           box(width=12, DT::dataTableOutput("Tab")
                                           )
                                    )  
                                        
                                )
                        ),
                        tabItem(tabName="data_explore",
                                
                                fluidRow(
                                  column(h2("Numerical Summary"),
                                         width=6,
                                         box( width=12,          
                                             selectInput("var", 
                                                          label=("Variables to Summarize"),
                                                         c("Age"= "age", 
                                                           "Crtn_phos"="creatinine_phosphokinase",
                                                           "diabetes"="diabetes",
                                                           "EjctFrt"= "ejection_fraction",
                                                           "Hi_Bld_Pres"="high_blood_pressure",
                                                           "platelets"="platelets",
                                                            "serum_creatinine"= "serum_creatinine",
                                                            "serum_sodium"=  "serum_sodium",
                                                            "sex"="sex",
                                                            "smoking"="smoking",
                                                          "time"= "time",
                                                          "DEATH_EVENT"= "DEATH_EVENT"
                                                           ) 
                                             ),
                                         
                                             selectInput("sumtype", 
                                                        label=("Summary Type"),
                                                         c("Ave"= "mean", 
                                                           "Median"="median",
                                                           "Minimum"="min",
                                                           "MAximum"= "maxi",
                                                           "quantile"="quantile",
                                                           "SD"="sd" 
                                                         )
                                              ),
                                             conditionalPanel(condition="input.sumtype == quantile",
                                                              sliderInput("qvalue",
                                                                          label="quantvalue",
                                                                          min=0,
                                                                          max=100,
                                                                          value=25
                                                                          )
                                             )
                                         )
                                     ),
                                     column(width=6,
                                            box(width=12, DT::dataTableOutput(("Tab2")
                                            ) 
                                      )
                                   ),
                                   br(),
                                   fluidRow( 
                                     column(width=3,h2("Graphic Summary"),
                                            box(width=12, 
                                                selectInput("plottype", 
                                                           label=("Plot Type"),
                                                           c("Histogram"= "histogram", 
                                                             "Barplot"="bar",
                                                             "Boxplot"="box",
                                                             "Scatterplot"= "point"
                                                            )
                                                 ),
                                          
                                                conditionalPanel(condition="input.plottype == histogram|bar",
                                                             selectInput("xvar1",
                                                                            h5("X variable for count",
                                                                               style ="color:grey;"),
                                                                            c("Age"= "age2", 
                                                                              "Crtn_phos"="creatinine_phosphokinase",
                                                                              "diabetes"="diabetes",
                                                                              "EjctFrt"= "ejection_fraction",
                                                                              "Hi_Bld_Pres"="high_blood_pressure",
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
                                                
                                                conditionalPanel(condition="input.plottype == box|point",
                                                                   selectInput("xvar2",
                                                                                  h5("X variable",
                                                                                  style ="color:grey;"),
                                                                                 c("Age"= "age2", 
                                                                                   "Crtn_phos"="creatinine_phosphokinase",
                                                                                   "diabetes"="diabetes",
                                                                                    "EjctFrt"= "ejection_fraction",
                                                                                   "Hi_Bld_Pres"="high_blood_pressure",
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
                                               
                                                                    selectInput("yvar",
                                                                                   h5("Y variable",
                                                                                   style ="color:grey;"),
                                                                                   c("Age"= "age", 
                                                                                     "Crtn_phos"="creatinine_phosphokinase",
                                                                                     "diabetes"="diabetes",
                                                                                     "EjctFrt"= "ejection_fraction",
                                                                                     "Hi_Bld_Pres"="high_blood_pressure",
                                                                                     "platelets"="platelets",
                                                                                     "serum_creatinine"= "serum_creatinine",
                                                                                     "serum_sodium"=  "serum_sodium",
                                                                                     "sex"="sex",
                                                                                     "smoking"="smoking",
                                                                                     "time"= "time",
                                                                                    "DEATH_EVENT"= "DEATH_EVENT"
                                                                                      )
                                                                     )
                                                  )
                                            
                                                )
                                      ),
                                      column(width=9,
                                            box(width=12, 
                                                plotOutput("displot")
                                            ),
                                            downloadButton('fig')      
                                      )
                                    
                                    )
                                  ),
                                  tabItem(tabName="model",
                                          tabsetPanel (
                                            tabPanel( 
                                                     h1("Modeling Info"),
                                                     column(width=4,
                                                           h2("generalized linear regression model"),
                                                           fluidRow(width=12,
                                                                    h3("brief introduction"),
                                                                    h4("Logistic linear gression model is one of generalized linear regression models, which 
                                                                     models the probability of a class response in responding to value change of predictors. 
                                                                       A benefit of logistic model is its values is always between 0 and 1, which is suitable
                                                                       for binomial reponse. In addition, it can be easily interpreted. Disadvantage include 
                                                                       that response has to be converted into logistic form and no straigt linear relationship 
                                                                       between predictors and response. On the other hand, it can easily lead to overfit."  )
                                                            ),
                                                            fluidRow(width=12,
                                                                     withMathJax(),
                                                                     h3("Formula"),
                                                                      p("\\(\\log(\\frac{P}{1-P})=\\beta_0+\\beta_1*x+\\beta_2*x_2+...+\\beta_p*x_p\\)"),
                                                                      "where P denotes probability, ", 
                                                                       h4(p("\\(x_i\\)")," predictors,"),
                                                                       p("\\(\\beta_0\\)"), " the intercept,",
                                                                         p("\\(\\beta_i\\)"),"slops" 
                                                                      
                                                            )
                                                    ),
                                                    column(width=4,
                                                           h2("classification tree"),
                                                           fluidRow(width=12,
                                                                    h3("brief introduction"),
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
                                                    column(width=4,
                                                           h2("random forest model"),
                                                           fluidRow(width=12,
                                                            h3("brief introduction")
                                                           ),
                                                           fluidRow(width=12,
                                                                    h3("Formula")
                                                           )
                                                     )
                                             ),
                                            tabPanel( 
                                                     h1("Modeling Fit"),
                                                     column(
                                                             h2("Parameter Input"),
                                                             width=3,
                                                             box(width=12,           
                                                                 sliderInput("train",
                                                                             label="Train data portion",
                                                                               min=0.1,
                                                                             max=0.9,
                                                                             value=0.6
                                                                            ),
                                                                  selectInput("modtype",
                                                                              label="Model Type",
                                                                              c("Linear Regression"="glm",
                                                                               "Tree classification"="rpart",
                                                                               "Random forest"="random_forest"
                                                                              )
                                                                  ),
                                                                  uiOutput("independent"),
                                                                  actionButton(
                                                                             "start",
                                                                              label="start"
                                                                  )
                                                    )
                                              
                                            ) ,
                                            column( h2("modeling results"),
                                                    width=3,
                                                    verbatimTextOutput("linMod")
                                            ),
                                            column(width=3,
                                                   verbatimTextOutput("treeMod")
                                            ),
                                            column(width=3,
                                                   verbatimTextOutput("rfMod")
                                            )
                                            
                                          ),   
                                                      tabPanel( 
                                                                h1("Prediction")
                                                       
                                                      )
                                          )
                                                 
                                  )
                       )
                      
                    )
) 
                        
# Define server logic required to draw the plots
server <- shinyServer(function(input, output) {
    heartData <- read_csv("heart_failure_clinical_records_dataset.csv")
    heartData$age2 <- cut(heartData$age, c(0,40,50,60,70,80,100) )
    heartData2 <- mutate(heartData, age2)
    heartData2$anaemia <-as.factor(heartData2$anaemia)
    heartData2$high_blood_pressure <-as.factor(heartData2$high_blood_pressure)
    heartData2$sex <-as.factor(heartData2$sex)
    heartData2$smoking  <-as.factor(heartData2$smoking)
    heartData2$DEATH_EVENT<-as.factor(heartData2$DEATH_EVENT)
    
  get_tab <- reactive({
    mincol <- input$mincol
    maxcol <- input$maxcol
    minrow <- input$minrow
    maxrow <- input$maxrow
    data.frame(heartData[minrow:maxrow, mincol:maxcol])
  })
  
  output$Tab <- DT::renderDataTable ({
    get_tab ()
  })
  output$Tab2 <- DT::renderDataTable ({
    var <-input$var
    if (input$sumtype =="mean") {data.frame(var,round(summarise (heartData, Avg=mean(heartData[[var]])),2))}
      else if (input$sumtype =="median") {data.frame(var,round(summarise (heartData,Median=median(heartData[[var]])),2))}
        else if (input$sumtype =="min") {data.frame(var,round(summarise (heartData,Min=min(heartData[[var]])),2))}
          else if (input$sumtype =="maxi") {data.frame(var,round(summarise (heartData,Maxi=max(heartData[[var]])),2))}
            else if (input$sumtype =="quantile") {data.frame(var,round(summarise (heartData,Quantile=quantile(heartData[[input$qvalue/100]])),2))}
           else if (input$sumtype =="sd") {data.frame(var,round(summarise (heartData,SD=sd(heartData[[var]])),2))}
  })
  
  output$displot <- renderPlot({
    #create plots
    #make plots based on one variable and different variables respectively 
    if (input$plottype =="bar" ) {
      g <- ggplot (data=heartData2, aes(x=as.factor(heartData2[[input$xvar1]])))
      g+geom_bar()+xlab(input$xvar1)
    } else if (input$plottype=="histogram" ) { 
      g <- ggplot (data=heartData2, aes(x=heartData2[[input$xvar1]]))
      g+ geom_histogram()+xlab(input$xvar1)
    } else if (input$plottype =="box" ) {
      g <- ggplot (data=heartData2, aes(x=as.factor(heartData2[[input$xvar2]]), y=heartData2[[input$yvar]]))
      g+geom_boxplot()+geom_jitter(alpha=0.5)+xlab(input$xvar2)+ylab(input$yvar)
    } else if (input$plottype =="point" ){
      g <- ggplot (data=heartData2, aes(x=heartData2[[input$xvar2]], y=heartData2[[input$yvar]]))
      g+geom_point()+xlab(input$xvar2)+ylab(input$yvar)
    }
  })
   
   
  InputData <- reactive({
    heartData
  })
  
  output$Data <- renderDT(InputDataset())

  trainData <- reactive({
    set.seed(20)
    sampleNum <- input$train
    trainset <- sample(1:nrow(heartData), size=nrow(heartData)*sampleNum)
    heartData[trainset, ]
    trainData$DEATH_EVENT<- as.factor(trainData$DEATH_EVENT)
  })
  
  tetData <- reactive({
     testset  <- setdiff(1:nrow(heartData),trainset)
     testData <- heartData[testset, ]
  })
  
   output$independent <- renderUI({
     checkboxGroupInput("independent", "Independent Variable", names(InputData()))
   })
   
  trCntrl <- trainControl(method="repeatedcv", number=10, repeats=5)
  linMod <- renderUI({
       if (input$modtype=="glm"){
          train(
          as.formula(paste(DEATH_EVENT," ~ ", paste(input$independent,collapse="+"))), 
          data=trainData(), 
          method="glm",
          family="binomial",
          trControl=trCntrl,
          na.action = na.exclude
          )
       }
       })
  treeMod <- reactive({
    if (input$modtype=="rpart"){
        tGrid <- expand.grid(cp = seq(0, 0.05, .003))
        train(
          as.formula(paste(DEATH_EVENT," ~ ", paste(input$independent,collapse="+"))), 
          data=trainData(), 
          method="rpart",
          preProcess=c("center","scale"),
          trControl=trCntrl,
          na.action = na.exclude,
          tuneGrid=tGrid
        )
      }
    })
  
  rfMod <- reactive({
  if (input$modtype=="random_forest"){
         train (
          as.formula(paste(DEATH_EVENT," ~ ", paste(input$independent,collapse="+"))),  
          data=trainData(),
          method = "rf",
          trControl =  trCntrl,
          preProcess = c("center", "scale"),
          na.action = na.exclude
        )
      }
  })
  
  output$linMod <- renderPrint({
   if (input$start) {
    linMod()
   }
  })
  output$treeMod <- renderPrint({
    if (input$start) {
      treeMod()
    }
  })
  output$rfMod <- renderPrint({
    if (input$start) {
      rfMod()
    }
  })
  
 output$fig = downloadHandler(
    filename = 'test.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = displot(), device = device)
    })

 output$dataset<- downloadHandler(
  filename = "Tab.csv", 
   content = function(file){
    write.csv(get_tab(), file, row.names = FALSE)
   }
 )
 output$Mod <- renderUI({
   Mod()
 })
}) 

shinyApp(ui = ui, server = server)
