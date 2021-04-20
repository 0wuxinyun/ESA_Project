# Library
library(shinydashboard)
library(shiny)

# Constant values
TICKERS <- 8
GRIDSIZE <- 4
TOTAL <- GRIDSIZE*GRIDSIZE

# Help Functions 
# Generate random Image 
randomI <- function(){
  a <- seq(1,8)
  randomTickers <- c(sample(a),sample(a))
  Images <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  count <- 1
  for(i in 1:GRIDSIZE){
    for(j in 1:GRIDSIZE){
      Images[i,j]=randomTickers[count]
      count <- count+1
    }
  }
  return(Images)
}
# UI 
ui <- dashboardPage(
  dashboardHeader(title ="Career Fair Game"),
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
              h2("Welcome! Here is the instruction for Career Fair game"),
              tags$h4("Instructions"),
              tags$p("Instruction text")
              
      ),
      
      # game
      tabItem(tabName = "Game",
              h2("Welcome to Career Fair game"),
              # start button to initialze timer
              actionButton("Start","Start"),
              fluidRow(
                box(
                  # timer
                  h2(textOutput("Time")),
                  # background imag width height need to change
                  img(src="Background.jpg",style="position:absolute;z-order:0;opacity: 0.2;",width="415px",height="415px"),
          
                  # holes 4x4 
                  imageOutput("cell11",height="100px",width="100px",click="click11",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell12",height="100px",width="100px",click="click12",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell13",height="100px",width="100px",click="click13",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell14",height="100px",width="100px",click="click14",inline=TRUE),  # height and width are for the containing div, not the image itself
                  tags$br(),
                  imageOutput("cell21",height="100px",width="100px",click="click21",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell22",height="100px",width="100px",click="click22",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell23",height="100px",width="100px",click="click23",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell24",height="100px",width="100px",click="click24",inline=TRUE),  # height and width are for the containing div, not the image itself
                  tags$br(),
                  imageOutput("cell31",height="100px",width="100px",click="click31",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell32",height="100px",width="100px",click="click32",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell33",height="100px",width="100px",click="click33",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell34",height="100px",width="100px",click="click34",inline=TRUE),  # height and width are for the containing div, not the image itself
                  tags$br(),
                  imageOutput("cell41",height="100px",width="100px",click="click41",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell42",height="100px",width="100px",click="click42",inline=TRUE),  # height and width are for the containing div, not the image itself
                  imageOutput("cell43",height="100px",width="100px",click="click43",inline=TRUE), # height and width are for the containing div, not the image itself
                  imageOutput("cell44",height="100px",width="100px",click="click44",inline=TRUE) # height and width are for the containing div, not the image itself
                  
                )
              )
      ))))

server <- function(input, output,session) {
  # Game reactive values:
  # Picture status : 0: blank 1:show the corresponding images 
  Status <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  # Picture location : Fixed -> Random
  Images <- randomI()
  # Uique identifier
  Identiy <- NULL
  # Track the last click 
  Previous <- c(-1,-1)
  PPrevious<- c(-1,-1)
  # Wrong 
  Wrong <- FALSE
  # End track
  Count <- 0
  # Frozen
  Frozen <- matrix(rep(FALSE,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  # All Reaction values
  gamevals <- reactiveValues(Status=Status,Identiy=Identiy,Previous=Previous,PPrevious=PPrevious,Wrong=Wrong,Count=Count,Frozen=Frozen)
  # Timer function:
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
    if(gamevals$Count!=8){
      paste("Time:",as.character(time_r()))}
    else{
      started(FALSE)
      paste("Game End:",as.character(time_r()))
    }
  })
  
  # Show the initil layout
  # display the hole and moles
  renderCell <- function(gridrow,gridcol){
    renderImage({
      # original back side of pictures & image
      Blank="www/blank.png"
      Flipped= paste0('www/',Images[gridrow,gridcol],'.png')
      # Check click events 
      imgsrc=switch(gamevals$Status[gridrow,gridcol]+1,Blank,Flipped)
      # display the pictures 
      list(src=imgsrc,style="position:relative;z-order:999") 
    },deleteFile=FALSE)
  }
  # show function
  show <- function(){
    output$cell11 <- renderCell(1,1)  
    output$cell12 <- renderCell(1,2) 
    output$cell13 <- renderCell(1,3)  
    output$cell14 <- renderCell(1,4)  
    output$cell21 <- renderCell(2,1) 
    output$cell22 <- renderCell(2,2) 
    output$cell23 <- renderCell(2,3)  
    output$cell24 <- renderCell(2,4)  
    output$cell31 <- renderCell(3,1) 
    output$cell32 <- renderCell(3,2) 
    output$cell33 <- renderCell(3,3)  
    output$cell34 <- renderCell(3,4)
    output$cell41 <- renderCell(4,1) 
    output$cell42 <- renderCell(4,2) 
    output$cell43 <- renderCell(4,3)  
    output$cell44 <- renderCell(4,4)
  }
  show()
  
  
  # click 
  processClickEvent <- function(gridrow,gridcol,Frozen){
    if (started()){
      if(Frozen[gridrow,gridcol]){
        gamevals$Status[gridrow,gridcol] <- 1
      }
      else{
      # Show image 
        # Check if click the same icon twice:
        if(!(gamevals$Previous[1] == gridrow & gamevals$Previous[2]==gridcol)){
      gamevals$Status[gridrow,gridcol] <- (gamevals$Status[gridrow,gridcol]+1)%%2
      # Get and Check the value 
      if(is.null(gamevals$Identiy)){
        gamevals$Identiy <- Images[gridrow,gridcol]
        if(gamevals$Wrong){
          gamevals$Status[gamevals$Previous[1],gamevals$Previous[2]] <- (gamevals$Status[gamevals$Previous[1],gamevals$Previous[2]]+1)%%2
          gamevals$Status[gamevals$PPrevious[1],gamevals$PPrevious[2]] <- (gamevals$Status[gamevals$PPrevious[1],gamevals$PPrevious[2]]+1)%%2
          gamevals$Wrong <- FALSE
        }
      }
      else{
        if(Images[gridrow,gridcol]==gamevals$Identiy){
          # Frozen the images
          gamevals$Frozen[gamevals$Previous[1],gamevals$Previous[2]] <- TRUE
          gamevals$Frozen[gridrow,gridcol] <- TRUE
          gamevals$Count <-gamevals$Count +1
        }
        else{
          # wrong click   
          gamevals$Wrong <- TRUE
          #gamevals$Status[gridrow,gridcol] <- (gamevals$Status[gridrow,gridcol]+1)%%2
        }
        # Free the history record
        gamevals$Identiy <- NULL
      }
      gamevals$PPrevious <- gamevals$Previous
      gamevals$Previous <- c(gridrow,gridcol)
    }}}
  }
  # Click event 
  click <- function(){
    observeEvent(input$click11,{processClickEvent(1,1,gamevals$Frozen)})
    observeEvent(input$click12,{processClickEvent(1,2,gamevals$Frozen)})
    observeEvent(input$click13,{processClickEvent(1,3,gamevals$Frozen)})
    observeEvent(input$click14,{processClickEvent(1,4,gamevals$Frozen)})
    observeEvent(input$click21,{processClickEvent(2,1,gamevals$Frozen)})
    observeEvent(input$click22,{processClickEvent(2,2,gamevals$Frozen)})
    observeEvent(input$click23,{processClickEvent(2,3,gamevals$Frozen)})
    observeEvent(input$click24,{processClickEvent(2,4,gamevals$Frozen)})
    observeEvent(input$click31,{processClickEvent(3,1,gamevals$Frozen)})
    observeEvent(input$click32,{processClickEvent(3,2,gamevals$Frozen)})
    observeEvent(input$click33,{processClickEvent(3,3,gamevals$Frozen)})
    observeEvent(input$click34,{processClickEvent(3,4,gamevals$Frozen)})
    observeEvent(input$click41,{processClickEvent(4,1,gamevals$Frozen)})
    observeEvent(input$click42,{processClickEvent(4,2,gamevals$Frozen)})
    observeEvent(input$click43,{processClickEvent(4,3,gamevals$Frozen)})
    observeEvent(input$click44,{processClickEvent(4,4,gamevals$Frozen)})
}
  
  click()
  
}

# Show app
shinyApp(ui, server)