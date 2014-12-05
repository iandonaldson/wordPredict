
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)


shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("wordPredict"),
  
  # Sidebar with a text input box
  sidebarPanel(
    textInput(inputId="userSays", label="Enter a few words", value = "")
  ),
  
  # Show the next possible word
  mainPanel(
    textOutput("iSay")
  )
))
