source("usePackages.R")
loadPkgs(c("shiny","DBI","tidyverse"))

library(shinydashboard)
library(shiny)

source("setAWSPassword.R")

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student033",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student033",
    password = getOption("AWSPassword"))
  conn
}

getRandomSentence <- function(conn){
  #Given a connection, call the View 'RandomSentence' and return the resulting name
  result <- dbGetQuery(conn,"SELECT * FROM RandomSentence")
  # result should be a dataframe with a single row and a column named 'randomname'
  randomname <- result$randomname[1]
  # To test what happens when there is a duplicate entry, we can override the random result
  #randomname <- "SophisticatedImaginaryZoo" # This matches an existing player in my database
  randomname
}


  

# UI 
ui <- dashboardPage(
  dashboardHeader(title ="Skills Workshop Game"),
  # Side bar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Instruction",tabName = "Instruction", icon = icon("info")),
      menuItem("Game", tabName="Game", icon = icon("gamepad"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # instruction
      tabItem(tabName = "Instruction",
              h2("Welcome! Here are the instructions for the Skills Workshop game"),
              tags$h4("Instructions"),
              tags$p("Instruction text")
              
      ),
      
      # game
      tabItem(tabName = "Game",
              h2("Welcome to Skills Workshop game"),
              # start button to initialze timer
              actionButton("Start","Start"),
              fluidRow(
                box(
                  # timer 
                  h2(textOutput("Time")),
                  # Display teh sentence
                  h2(textOutput("Sentence")),
                  #textfield for player input
                  textInput("text", h3("Text input"), 
                            value = ""))   
              )))))
    
      

      
server <- function(input, output,session) {
  
  vals <- reactiveValues(sentence=NULL,status=TRUE)
  output$Sentence <- renderText({
    # Game reactive values:
    conn <- getAWSConnection()
    vals$sentence <- getRandomSentence(conn)
    vals$sentence 
  })
  # timer render by click start button
  time_r <- reactiveVal(value = 0)
  started <- reactiveVal(value = FALSE)
  observeEvent(input$Start, {
    time_r(0)
    started(TRUE)
  }, ignoreInit = TRUE)
  observe({
    if (started()) {
      invalidateLater(100, session)
      isolate({
        newTime <- time_r() + 0.1
        time_r(newTime)
      })
    }
  })
  
  output$Time <- renderText({
    if(vals$status){
      paste("Time:",as.character(time_r()))}
    else{
      started(FALSE)
      paste("Game End:",as.character(time_r()))
    }
  })

  observe({
    if (started()) {
    req(input$text)
    if(input$text==vals$sentence){
      # Stop the game
      vals$status <- FALSE
    }}
      })
  
  }
  
                  
shinyApp(ui, server)
   