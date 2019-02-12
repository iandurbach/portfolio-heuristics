library(shiny)
#library(shinyjs)
#library(shinythemes)

#if (interactive()) {
  ui<-fluidPage(
    #theme = shinytheme("sandstone"),
    #useShinyjs(),
    includeCSS("styles.css"),
    tags$head(tags$style(HTML(".shiny-text-output { font-size: 20px; background-color:#fff;}"))),
    
    tabsetPanel(
      # "shinyjs with navbarPage",
      # id = "navbar",
      tabPanel(tags$h1("Step 3 - Experiment"),
               value = "mytab2",
               sidebarLayout(
                 sidebarPanel(
                   fluidRow(
                     column(8, tags$h4("Table 1 - Select Projects")),
                     column(4,verbatimTextOutput("display1"))
                     
                   ),
                   fluidRow(
                     column(4, checkboxGroupInput("cbg", "from list below", c("Project 1"="1","Project 2"="2","Project 3"="3","Project 4"="4","Project 5"="5"))),
                     column(4, tableOutput("data3"))
                   )
                   
                 ),
                 mainPanel(
                   #titlePanel("Portfolio Selection Experiment"),
                   fluidRow(
                     column(12,verbatimTextOutput("eventTimeRemaining"))
                     ),
                   fluidRow(
                     column(4,verbatimTextOutput("code"))
                   ),
                   fluidRow(
                     column(3, textOutput("message"),
                     tags$head(tags$style("#message{color: red;font-size: 20px;font-style: italic;}"))
                     ),
                     column(4, tags$h4("Table 2 - Summary Table"))
                     ),
                   fluidRow(
                     column(3, ""),
                     column(4, tableOutput("data")),
                     column(4, "")
                   ),
                   fluidRow(column(4, tags$h4("Table3 - Potential Changes in Overall Values if project selected")),
                            column(1, ""),
                            column(4, tags$h4("Table4 - Potential Changes in Overall Values if project deselected"))   
                            
                   ),
                   fluidRow(
                     column(4, tableOutput("data1")),
                     column(1, ""),
                     column(4, tableOutput("data2"))
                   ),
                   fluidRow(
                     column(2, ""),
                     column(2, ""),
                     column(2, ""),
                     column(2, ""),
                     column(4,actionButton(inputId = "go1",label = "Submit"))
                   )
                   
                 )
               ))
      
      )
      
    )
#}