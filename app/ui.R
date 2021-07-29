library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Test Shiny App"),
    
    sidebarPanel(
        selectInput("dependent", "Dependent Variable:", c("x","y","z")),
        uiOutput("independent")
    ),
    
    mainPanel(tableOutput("regTab"))
))