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
                                                ", which is about predicting patients' survial from their medical data.")
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
                                         downloadButton('download',"Download the data")
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
                                            ) 
                                            
                                     )
                                    
                                  )
                        ),
                                  tabItem(tabName="model",
                                          tabsetPanel (
                                              tabPanel( 
                                                      h1("Modeling Info"),
                                                      
                                               ),
                                              
                                             
                                              tabPanel( 
                                                       h1("Modeling Fit"),
                                                       column(h2("Parameter Input"),
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
                                                                             "Random forest"="rf"
                                                                           )
                                                                          ),
                                                           checkboxGroupInput("selectx", 
                                                                       label=("Select Predictors"),
                                                                       choices=list("age", 
                                                                                 "creatinine_phosphokinase",
                                                                                 "diabetes",
                                                                                 "ejection_fraction",
                                                                                 "high_blood_pressure",
                                                                                 "platelets",
                                                                                 "serum_creatinine",
                                                                                 "serum_sodium",
                                                                                 "sex",
                                                                                 "smoking",
                                                                                 "time"
                                                                                  ),
                                                                     
                                                                       selected=list(
                                                                                  "age", 
                                                                                  "creatinine_phosphokinase",
                                                                                  "diabetes",
                                                                                  "ejection_fraction",
                                                                                  "high_blood_pressure",
                                                                                  "platelets",
                                                                                  "serum_creatinine",
                                                                                  "serum_sodium",
                                                                                 "sex",
                                                                                  "smoking",
                                                                                 "time"
                                                                                  )
                                                                       ) 
                                                            ),
                                                          actionButton(
                                                            "start",
                                                            label="start"
                                                          )
                                                          ),
                                                       
                                                       fluidRow( 
                                                          column(width=9,
                                                            uiOutput("Mod")
                                                          )
                                                       )
                                                      ),          
                                              
                                              
                                               tabPanel( 
                                                       h1("Prediction")
                                                       
                                               )
                                                      
                                            )
                                          
                                  ) 
                              
                                            
                                           
                             
                                 ),
                               
                                       
                                 
                                
                          
                        
                         
                    
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
    mincol <- input$mincol
    maxcol <- input$maxcol
    minrow <- input$minrow
    maxrow <- input$maxrow
    data.frame(heartData[minrow:maxrow, mincol:maxcol])
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
 
  output$Mod <- renderUI({
    if (input$start) {
      DEATH_EVENT<- as.factor(heartData$DEATH_EVENT)
      trainset <- sample(1:nrow(heartData), size=nrow(heartData)*input$train )
      trainData <- heartData[trainset, ]
      testset  <- setdiff(1:nrow(heartData),trainset )
      testData <- heartData[testset, ]
      
      trCntrl <- trainControl(method="repeatedcv", number=10, repeats=5)
      if (input$modtype=="glm"){
        linMod <- train(
          as.formula(paste(DEATH_EVENT," ~ ",paste(input$selectx,collapse=" + "))), 
          data=trainData, 
          method="glm",
          family="binomial",
          trControl=trCntrl,
          na.action = na.exclude
        )
        linMod 
      } else if (input$modtype=="rpart"){
        tGrid <- expand.grid(cp = seq(0, 0.05, .003))
        treeMod <-train(
          as.formula(paste(DEATH_EVENT," ~ ",paste(input$selectx,collapse=" + "))), 
          data=trainData, 
          method="rpart",
          preProcess=c("center","scale"),
          trControl=trCntrl,
          na.action = na.exclude,
          tuneGrid=tGrid
        )
        treeMod
      } else if (input$modtype=="rf"){
        rfMod <- train (
          as.formula(paste(DEATH_EVENT," ~ ",paste(input$selectx,collapse=" + "))), 
          data=trainData,
          method = "rf",
          trControl =  trCntrl,
          preProcess = c("center", "scale"),
          na.action = na.exclude
        )
        rfMod
      }
    }
    
  })
 

 output$download <- downloadHandler(
  filename = "Tab.csv", 
   content = function(file){
    write.csv(get_tab(), file, row.names = FALSE)
   }
 )
}) 

shinyApp(ui = ui, server = server)
