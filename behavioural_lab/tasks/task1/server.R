library(shiny)
#library(shinyjs)
#library(lubridate)
#library(RODBC)


server<-function(input,output,session){
  
  output$data5 <- renderTable({
    data2()[,3:5]
    
  }, rownames = TRUE, digits = 0)
  
  
  output$data6 <- renderTable({
    data2()[,6:8]
  }, rownames = TRUE, digits = 0)
  
  
  output$data7 <- renderTable({
    data2()[,1:2]
    
  }, rownames = TRUE, digits = 0)
  
   numbers<-reactive({as.numeric(input$cbg)})
   
   data <- reactive({ 
     
     source("code/value_project2.R")
     numbers <- as.numeric(input$cbg)
     
     table1=value_proj2(numbers)
     return(table1)
     
   })
   
   
   output$data <- renderTable({
     
     source("code/value_project.R")
     numbers <- as.numeric(input$cbg)
     
     table=value_proj(numbers)
     
   }, rownames = TRUE, digits = 0)
   
   output$data1 <- renderTable({
     data()[,3:5]
     
   }, rownames = TRUE, digits = 0)
   
   
   output$data2 <- renderTable({
     data()[,6:8]
   }, rownames = TRUE, digits = 0)
   
   
   output$data3 <- renderTable({
     data()[,1:2]
     
   }, rownames = TRUE, digits = 0)
   
}
