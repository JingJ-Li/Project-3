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
                                      
                                       box(background="light-blue",width=12
                                       )
                                  ),
                                
                                  column(6,
                                      
                                       h1("Logo"),
                                       
                                       box(background="light-blue",width=12
                                       )
                                  )
                                )
                        ),
                  
                        tabItem(tabName="data",
                                fluidRow(
                                  column(h3("Parameter choice for subsetting data"),
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
                                                          value=13)
                                             ),
                                         downloadButton('dataset',"Download the data")
                                    ),
                                    column(width=9,
                                           box(width=12,DT::dataTableOutput("Tab")
                                           )
                                    )  
                                        
                                )
                        ),
                        tabItem(tabName="data_explore",
                                fluidRow(
                                  column(h2("Numerical Summary"),
                                         width=4,
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
                                                              
                                             ),
                                             selectInput("var2", 
                                                         label=("Row Variables For Contingency table"),
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
                                             selectInput("var3", 
                                                         label=("Column Variables For Two-way table"),
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
                                     ),
                                     column(width=4,
                                            box(width=12,
                                                DT::dataTableOutput("Tab2")
                                            )
                                      ),
                                      column(width=4,
                                             box(width=12, 
                                                 tableOutput('Tab3')
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
                                            downloadButton("fig")      
                                      )
                                    
                                    )
                                  ),
                                  tabItem(tabName="model",
                                          tabsetPanel(
                                            tabPanel( 
                                                     "Modeling Info",
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
                                                            h3("brief introduction"),
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
                                                     )
                                             ),
                                            tabPanel( 
                                                     "Modeling Fit",
                                                     column(
                                                             "Parameter Input",
                                                             width=3,
                                                             box(width=12,           
                                                                 sliderInput("slider",
                                                                             label="Train data portion",
                                                                             min=0.1,
                                                                             max=0.9,
                                                                             value=0.6
                                                                            ),
                                                                 uiOutput("independent"),
                                                                 numericInput("cp1",
                                                                              label="treeMod cp1",
                                                                              min=0,
                                                                              max=1,
                                                                              value=0
                                                                  ),
                                                                  actionButton(
                                                                              "start",
                                                                              label="start"
                                                                  )
                                                             )
                                                    ) ,
                                                    column( "modeling results",
                                                           width=3,
                                                           verbatimTextOutput("linMod"),
                                                           fluidRow(
                                                             width=12,
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
                                                             numericInput("mtry",
                                                                          label="rfeMod mtry1",
                                                                          min=0,
                                                                          max=12,
                                                                          value=3
                                                             ),
                                                             numericInput("cp3",
                                                                          label="rfeMod mtry2",
                                                                          min=0,
                                                                          max=12,
                                                                          value=6
                                                             ),
                                                             numericInput("cp3",
                                                                          label="rfeMod mtry3",
                                                                          min=0,
                                                                          max=12,
                                                                          value=12
                                                             ),
                                                           )
                                                    ),
                                                    column(width=3,
                                                           verbatimTextOutput("treeMod")
                                                    ),
                                                    column(width=3,
                                                            verbatimTextOutput("rfMod")
                                                    ),
                                                    verbatimTextOutput("sumMisclass")
                                              ),   
                                              tabPanel( 
                                                       "Prediction",
                                                      column(width=3,
                                                             selectInput("modtype2",
                                                                          label="Model Type",
                                                                          c("Linear Regression"="glm",
                                                                             "Tree classification"="rpart",
                                                                             "Random forest"="random_forest"
                                                                          )
                                                                         ),
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
                                                            actionButton(
                                                              "start",
                                                              label="start"
                                                            )
                                                      ),
                                                      column(width=3,  
                                                            verbatimTextOutput("pred")     
                                                     )
                                                       
                                               )
                                          )
                                                 
                                  )
                       )
                      
                    )
) 
                        
# Define server logic required to draw the plots
server <- shinyServer(function(input, output) {
    heartData <- read_csv("heart_failure_clinical_records_dataset.csv")
    heartData$DEATH_EVENT<-as.factor(heartData$DEATH_EVENT)
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
  
  output$Tab <- DT::renderDataTable (
    get_tab (),
    options = list(scrollX = TRUE)
  )
  output$Tab2 <- DT::renderDataTable ({
    var <-input$var
    if (input$sumtype =="mean") {data.frame(var,round(summarise (heartData, Avg=mean(heartData[[var]])),2))}
      else if (input$sumtype =="median") {data.frame(var,round(summarise (heartData,Median=median(heartData[[var]])),2))}
        else if (input$sumtype =="min") {data.frame(var,round(summarise (heartData,Min=min(heartData[[var]])),2))}
          else if (input$sumtype =="maxi") {data.frame(var,round(summarise (heartData,Maxi=max(heartData[[var]])),2))}
            else if (input$sumtype =="quantile") {data.frame(var,round(summarise (heartData,Quantile=quantile(heartData[[var]],probes=input$qvalue/100)),2))}
           else if (input$sumtype =="sd") {data.frame(var,round(summarise (heartData,SD=sd(heartData[[var]])),2))}
  })
  
  
  output$Tab3 <- renderTable({
    var2 <- input$var2
    var3 <- input$var3
    as.data.frame.matrix(heartData$var2, heartData$var3)
  })
  
  plot <- reactive({
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
  
  output$displot <- renderPlot({
    plot()
  })
   
  output$fig = downloadHandler(
    filename = 'figure.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot(), device = device)
    })
  
  InputData <- reactive({
    heartData 
  })
  
  output$Data <- renderDT(InputDataset())
  
  set.seed(20)
  trainRowIndex <- reactive ({
    sampleNum <- input$slider
    sample(1:nrow(InputData()), size=nrow(InputData())*sampleNum)
  })

  trainData <- reactive({
    subsetData <- heartData[trainRowIndex(), ]
    
  })
  
  testData <- reactive({
     testset  <- heartData[-trainRowIndex(), ]
  })
  
   output$independent <- renderUI({
     checkboxGroupInput("independent", "Independent Variable", names(InputData()))
   })
   
  trCntrl <- trainControl(method="repeatedcv", number=10, repeats=5)
  linMod <-  reactive({
    if (input$start) {train(
          as.formula(paste("DEATH_EVENT"," ~ ", paste(input$independent,collapse="+"))), 
          data=trainData(), 
          method="glm",
          family="binomial",
          trControl=trCntrl,
          na.action = na.exclude
          )
    }
  })
  
  treeMod <- reactive({
        if (input$start) { train(
          as.formula(paste("DEATH_EVENT"," ~ ", paste(input$independent,collapse="+"))), 
          data=trainData(), 
          method="rpart",
          preProcess=c("center","scale"),
          trControl=trCntrl,
          na.action = na.exclude,
          tuneGrid=expand.grid(cp = seq(input$cp1, input$cp2, input$cp3))
        )
        }
  })
  
  rfMod <- reactive({
    if (input$start) { train (
          as.formula(paste("DEATH_EVENT"," ~ ", paste(input$independent,collapse="+"))),  
          data=trainData(),
          method = "rf",
          trControl =  trCntrl,
          preProcess = c("center", "scale"),
          tuneGrid=expand.grid(mtry=c(3, 6, 12)),
          na.action = na.exclude
        )
     }
  })
  
  output$linMod <- renderPrint({
    linMod()
  })
  
  output$treeMod <- renderPrint({
      treeMod()
  })
  
  output$rfMod <- renderPrint({
      rfMod()
  })
  
  sumMisclass <- reactive ({
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
  output$sumMisclass <- renderPrint({
    sumMisclass()
  })
  inputData2<- reactive({data.frame(age=input$age,anaemia=input$anaemia,creatinine_phosphokinase=input$creatinine_phosphokinase, 
                                    diabetes=input$diabetes, ejection_fraction=input$ejection_fraction,high_blood_pressure=input$high_blood_pressure,
                                    platelets=input$platelets,serum_creatinine=input$serum_creatinine,
                                    serum_sodium=input$serum_sodium,sex=input$sex, smoking=input$smoking, time=input$time)
  })
  pred <- reactive({
    if (!input$start) {stop}
     else if (input$modtype2 == "glm"){
       predict(linMod(), newdata=inputData2())
    } else if (input$modtype2 == "rpart"){
      predict(treeMod(), newdata=inputData2())
    } else if (input$modtype2 == "rf"){
      predict(rfMod(), newdata=inputData2())
    }
    
  })
  output$pred <- renderPrint({
    pred()
  })   
 

 output$dataset<- downloadHandler(
  filename = "Tab.csv", 
   content = function(file){
    write.csv(get_tab(), file, row.names = FALSE)
   }
 )

}) 

shinyApp(ui = ui, server = server)
