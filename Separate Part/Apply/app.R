" This file is the main for Apply function use dashboard"
"Detail : 4x4 whack a mole game 
N: number of trials in total
n: number of flashes each time
t: time interval unit 0.1second"
N <- 5
t <- 20
n <- 2
"Sedo ariable"
GRIDSIZE <- 4 
# Library
library(shinydashboard)
library(shiny)


# Help functions
# 1- Random position generator 
"A postition(x,y) 4x4"
random <- function(){
  mole <- matrix(rep(c(0,0),n*n),nrow=n,ncol=n,byrow=TRUE)
  for(i in 1:n){
    x <- sample(1:4,1)
    y <- sample(1:4,1)
    mole[i,1] <- x
    mole[i,2] <- y}
  return(mole)
}

# UI 
ui <- dashboardPage(
  dashboardHeader(title ="Apply Game"),
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
                   h2("Welcome! Here is the instruction for Apply game"),
                   tags$h4("Instructions"),
                   tags$p("Instruction text")
                   
    ),
    
    # game
    tabItem(tabName = "Game",
            h2("Welcome to Apply game"),
            # start button to initialze timer
            actionButton("Start","Start"),
            fluidRow(
              box(
              # timer
              h2(textOutput("Time")),
              h2(textOutput("Score")),
              # background imag width height need to change
              img(src="Background.jpg",style="position:absolute;z-order:0;opacity: 0.2;",width="415px",height="415px"),
            
              # holes 4x4 
              imageOutput("acell11",height="100px",width="100px",click="click11",inline=TRUE), # height and width are for the containing div, not the image itself
              imageOutput("acell12",height="100px",width="100px",click="click12",inline=TRUE),  # height and width are for the containing div, not the image itself
              imageOutput("acell13",height="100px",width="100px",click="click13",inline=TRUE), # height and width are for the containing div, not the image itself
              imageOutput("acell14",height="100px",width="100px",click="click14",inline=TRUE),  # height and width are for the containing div, not the image itself
              tags$br(),
              imageOutput("acell21",height="100px",width="100px",click="click21",inline=TRUE), # height and width are for the containing div, not the image itself
              imageOutput("acell22",height="100px",width="100px",click="click22",inline=TRUE),  # height and width are for the containing div, not the image itself
              imageOutput("acell23",height="100px",width="100px",click="click23",inline=TRUE), # height and width are for the containing div, not the image itself
              imageOutput("acell24",height="100px",width="100px",click="click24",inline=TRUE),  # height and width are for the containing div, not the image itself
              tags$br(),
              imageOutput("acell31",height="100px",width="100px",click="click31",inline=TRUE), # height and width are for the containing div, not the image itself
              imageOutput("acell32",height="100px",width="100px",click="click32",inline=TRUE),  # height and width are for the containing div, not the image itself
              imageOutput("acell33",height="100px",width="100px",click="click33",inline=TRUE), # height and width are for the containing div, not the image itself
              imageOutput("acell34",height="100px",width="100px",click="click34",inline=TRUE),  # height and width are for the containing div, not the image itself
              tags$br(),
              imageOutput("acell41",height="100px",width="100px",click="click41",inline=TRUE), # height and width are for the containing div, not the image itself
              imageOutput("acell42",height="100px",width="100px",click="click42",inline=TRUE),  # height and width are for the containing div, not the image itself
              imageOutput("acell43",height="100px",width="100px",click="click43",inline=TRUE), # height and width are for the containing div, not the image itself
              imageOutput("acell44",height="100px",width="100px",click="click44",inline=TRUE) # height and width are for the containing div, not the image itself
              
              )
    )
  ))))


server <- function(input, output,session) { 
  # ReactiveValues
  " Status reactive matrix : 1: Flash 0:normal"
  Status <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  mole <- random()
  score <- 0
  gamevals <- reactiveValues(mole=mole,Status=Status,score=score)
  # timer render by click start button
  time_r <- reactiveVal(value = 0)
  started <- reactiveVal(value = FALSE)
  observeEvent(input$Start, {
    time_r(0)
    started(TRUE)
    gamevals$score=0
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
    if(N*t*0.1-time_r()<0){
      started(FALSE)
      "Time Runout"
    }
    else{if(as.integer(as.double(time_r())*10) %/% t<=N){
    paste("Time:",as.character(N*t*0.1-time_r()))}}
  })
  

  # random position update
  observe({if((as.integer(as.double(time_r())*10)%%t==0) && (as.integer((as.double(time_r()))*10) %/% t<N) && (started()) ){ 
    gamevals$mole <- random()
    gamevals$Status <- Status
    show2()
    }})
  
  # display the hole and moles
  renderCell2 <- function(gridrow,gridcol){
    renderImage({
      imgsrc=("www/cross.png")
      for(i in 1:n){
      if (gridrow==gamevals$mole[i,1]&gridcol==gamevals$mole[i,2]){
        imgsrc=("www/mole.png")
        gamevals$Status[gamevals$mole[i,1],gamevals$mole[i,2]]=1
        break
      }}
      # display the holes 
      list(src=imgsrc,style="position:relative;z-order:999") 
    },deleteFile=FALSE)
  }
  
  # show function
  show2 <- function(){
    output$acell11 <- renderCell2(1,1)  
    output$acell12 <- renderCell2(1,2) 
    output$acell13 <- renderCell2(1,3)  
    output$acell14 <- renderCell2(1,4)  
    output$acell21 <- renderCell2(2,1) 
    output$acell22 <- renderCell2(2,2) 
    output$acell23 <- renderCell2(2,3)  
    output$acell24 <- renderCell2(2,4)  
    output$acell31 <- renderCell2(3,1) 
    output$acell32 <- renderCell2(3,2) 
    output$acell33 <- renderCell2(3,3)  
    output$acell34 <- renderCell2(3,4)
    output$acell41 <- renderCell2(4,1) 
    output$acell42 <- renderCell2(4,2) 
    output$acell43 <- renderCell2(4,3)  
    output$acell44 <- renderCell2(4,4)
  }
  show2()

  # click 
  processClickEvent2 <- function(gridrow,gridcol){
    if ((gamevals$Status[gridrow,gridcol]==1)&&(started())){
      gamevals$score <- gamevals$score+1
      gamevals$Status[gridrow,gridcol]==0
    }
  }
  # update score
  output$Score <- renderText(paste("Score",as.character(sum(gamevals$score))))
  
  click <- function(){
  observeEvent(input$click11,{processClickEvent2(1,1)})
  observeEvent(input$click12,{processClickEvent2(1,2)})#,once = TRUE)
  observeEvent(input$click13,{processClickEvent2(1,3)})#,once = TRUE)
  observeEvent(input$click14,{processClickEvent2(1,4)})#,once = TRUE)
  observeEvent(input$click21,{processClickEvent2(2,1)})#,once = TRUE)
  observeEvent(input$click22,{processClickEvent2(2,2)})#,once = TRUE)
  observeEvent(input$click23,{processClickEvent2(2,3)})#,once = TRUE)
  observeEvent(input$click24,{processClickEvent2(2,4)})#,once = TRUE)
  observeEvent(input$click31,{processClickEvent2(3,1)})#,once = TRUE)
  observeEvent(input$click32,{processClickEvent2(3,2)})#,once = TRUE)
  observeEvent(input$click33,{processClickEvent2(3,3)})#,once = TRUE)
  observeEvent(input$click34,{processClickEvent2(3,4)})#,once = TRUE)
  observeEvent(input$click41,{processClickEvent2(4,1)})#,once = TRUE)
  observeEvent(input$click42,{processClickEvent2(4,2)})#,once = TRUE)
  observeEvent(input$click43,{processClickEvent2(4,3)})#,once = TRUE)
  observeEvent(input$click44,{processClickEvent2(4,4)})#,once = TRUE)}
  
  click()
  }

shinyApp(ui, server)