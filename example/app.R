library(shiny)
library(shinydashboard)
library(ECharts2Shiny)
dat1 <- data.frame(c(75,68,80,528),
                   c(40,33,30,100),
                   c(50,38,54,98),
                   c(25,30,30,430))
names(dat1) <- c("Visitors", "Referred", "Male", "Female")
row.names(dat1) <- c("Baksa", "Barpeta","Biswanath","Bongaigaon")

ui <- shinyUI(
    dashboardPage(dashboardHeader(title = "Sanj"),
                  dashboardSidebar(sidebarMenu(menuItem("ABC", tabName = "Sanj", 
                                                        menuSubItem("Baksa"), 
                                                        menuSubItem("Barpeta"), 
                                                        menuSubItem("Biswanath"), 
                                                        menuSubItem("Bongaigaon")))),
                  dashboardBody(fluidPage(h1("Sanj"),
                                          mainPanel(tabsetPanel(type = "tab",
                                                                tabPanel(h5("Visitors vs. Referred"),
                                                                         loadEChartsLibrary(), tags$div(id="test1", 
                                                                                                        style="width:150%;height:500px;"),
                                                                         deliverChart(div_id = "test1")))))
                  )))
                                server <- shinyServer(function(input,output){
                                    renderBarChart(div_id = "test1", grid_left = '1%', direction = "vertical",
                                                   data = dat1)})
                                
                                shinyApp(ui, server)
                                

