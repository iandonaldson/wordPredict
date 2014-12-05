
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
source("mylib.R")



shinyServer(function(input, output) {
  #output$iSay <- reactive(input$userSays)
  #output$iSay <- renderText({return("the test worked")})
  #output$iSay <- renderText({testFunction()})
  #output$iSay <- renderText({preProcessLine(input$userSays)})
  output$iSay <- renderText({predictNextWord(inputString=input$userSays, cutoff=5, all=FALSE)})
})
