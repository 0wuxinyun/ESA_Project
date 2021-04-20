
# Image from:
# https://www.wexlerevents.com/defining-dress-codes-what-wear-every-occassion/

# nbVQa839
# source("usePackages.R")
# pkgnames <- c("DT","tidyverse","shiny","DBI","jsonlite","shinydashboard")
# loadPkgs(pkgnames)
library(shiny)
library(DT)
library(tidyverse)
library(DBI)
library(jsonlite)
library(shinydashboard)
library(rsconnect)

#### Career Fair Helper ####
# Constant values
TICKERS <- 8
GRIDSIZE <- 4
TOTAL <- GRIDSIZE*GRIDSIZE

#### Apply Helper ####

"Detail : 4x4 whack a mole game 
N: number of trials in total
n: number of flashes each time
t: time interval unit 0.1second"
N <- 5
t <- 20
n <- 2

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

#### Skill Workshop Helper ####
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


#### Modals ####

nameFailedModal <- function(){
  modalDialog(
    title = "Please input a valid name"
  )
}

changeAvatarModal <- function(){
  modalDialog(
    title = "Change avatar",
    p("Proceed to ",tags$b("Welcome tab!"))
  )
}

avatarUpdateModal <- function(changed = 0,impressionscore){#whether avatar was selected previously before
    modalDialog(
      if (changed==1)
        p("Avatar changed."),
      if (changed==0)
      p("Avatar Selected"),
      p("New Impression Score", impressionscore),
      p("Please click on Home Page tab to continue."),
      
      footer = tagList(
        modalButton("OK")
      )
    )
}

# When exiting game, show improved stats and prompt player
exitGameModal <- function(game,
                          impression=0,
                          skills=0,
                          confidence=0,
                          social=0){
  modalDialog(
    title = paste("Exiting",game),
    h3("Score improvements"),
    p("Impression: +",impression),
    p("Skills: +",skills),
    p("Confidence: +",confidence),
    p("Social: +",social),
    h4("Click on Home Page tab to continue."),
    footer = tagList(
      modalButton("OK")
    )
  )
}

# entering game, prompt player
enterGameModal <- function(gamestring,energy_required=0){
  modalDialog(
    title = paste("Entering", gamestring),
    p("Energy used: ", energy_required),
    p("Click on Mini Game tab to continue."),
    footer = tagList(
      modalButton("OK")
    )
  )
}

enterGameModalFailed <- function(){
  modalDialog(
    title = "OOPS!",
    p("Not enough energy! Go to sleep or choose some other activity."),
    footer = tagList(
      modalButton("OK")
    )
  )
}

sleepModal <- function(){
  modalDialog(
    title = "You are rejuvenated!",
    p("Energy back to 100"),
    p("Day increased by 1")
  )
}

youWinModal <- function(days){
  modalDialog(
    title = "You got a job! :)",
    p("You got a job in ",tags$b(days)," days!"),
    p("Please return to Welcome tab"),
    footer=tagList(
      actionButton("winok","OK!")
    )
  )
}

youLoseModal <- function(){
  modalDialog(
    title = "You didn't manage to get a job! :(",
    p("Try to get your scores up first before you apply for the job."),
    footer = tagList(
      actionButton("loseok","OK")
    )
  )
}



#### dashboard MAIN UI ####

ui <- dashboardPage(
  dashboardHeader(title = "Job Finding Game"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      #https://fontawesome.com/icons?d=gallery
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("Home Page", tabName = "home", icon = icon("home")),
      menuItem("Mini Game", tabName = "game", icon = icon("chess-board")),
      htmlOutput("gamestate")
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "welcome",
              uiOutput("welcometab")
      ),
      
      # Second tab content
      tabItem(tabName = "home",
              tabBox( width = "100%",
                tabPanel("Landing Page", uiOutput("landing")),
                tabPanel("Instructions", uiOutput("landing_instructions"))
              )
              )
      ,
      
      # Third tab content
      tabItem(tabName = "game",
              h2("Mini Game Tab"), #Can try to deal for mini game 1 or 2
              uiOutput("minigame")
              )
      ), 
    )
  ) #dashboard body

#### DASHBOARD SERVER ####

server <- function(input, output, session) {
  #Set game parameters
  
  # reactiveValues objects for storing in-game items
  vals <- reactiveValues(playerid=NULL,playername=NULL,gamestate="welcome",
                         energy=90,days=0,
                         skills=10,impression=0,social=10,confidence=10,
                         sentence=NULL,status=TRUE)
  
  avatarc <- c(rep(0,8))
  avatarvals <- reactiveValues(avatar = avatarc)
  
  #### SIDEBAR OUTPUT ####
  output$gamestate <- renderUI({
    paste("game state:",vals$gamestate)
  })
  
  #### WELCOME PAGE OUTPUT ####
  
  output$welcometab <- renderUI(
    {if (vals$gamestate == "welcome"){
      if (is.null(vals$playername)){
        fluidPage(
          h2("Welcome!"),
          textInput("playernameinput","Please enter your name for your avatar: "),
          actionButton("submitnameok","Submit"),
          hr(),
          br(),
        )
      } else{
        fluidPage(
          tags$h4("Logged in as:"),
          htmlOutput("loggedInAs"),
          tags$br(),
          tags$h4("Step 1: Choose your avatar"),
          tags$p("Choose your avatar! Make sure to pick one that is presentable to interviewers."),
          htmlOutput("avatarselected"),
          img(src="avatarbackground.png",width="800px",height="415px",style="position:absolute;z-order:0"),
          imageOutput("cell1",height="200px",width="200px",click="click1",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("cell2",height="200px",width="200px",click="click2",inline=TRUE),  # height and width are for the containing div, not the image itself
          imageOutput("cell3",height="200px",width="200px",click="click3",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("cell4",height="200px",width="200px",click="click4",inline=TRUE),  # height and width are for the containing div, not the image itself
          tags$br(),
          imageOutput("cell5",height="200px",width="200px",click="click5",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("cell6",height="200px",width="200px",click="click6",inline=TRUE),  # height and width are for the containing div, not the image itself
          imageOutput("cell7",height="200px",width="200px",click="click7",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("cell8",height="200px",width="200px",click="click8",inline=TRUE),  # height and width are for the containing div, not the image itself
          tags$br()
        )
      }
    } else {
      if (vals$gamestate != "home"&vals$gamestate != "welcome")
        fluidPage(
          h2("Please go to Mini Game Tab")
        )
      else fluidPage(
        h2("Please go to ",vals$gamestate, " Tab")
      )
    }
    })
  
  # React to successful login
  output$loggedInAs <- renderUI({
    if (is.null(vals$playername))
      "Not logged in yet."
    else
      vals$playername
  })
  
  #### Welcome Page SERVER ####
  
  # player clicks submit name, game saves playername and sets playerid
  observeEvent(input$submitnameok, {
    if (input$playernameinput!="")
      vals$playername <- input$playernameinput
    else
      showModal(nameFailedModal())
    vals$playerid <- 1
  })
  
  
  #### AVATAR OUTPUT ####
  output$avatarselected <- renderUI({
    #p(paste("this is ", toString(avatarvals$avatar)))
    if (sum(avatarvals$avatar == 1))
      {p("Avatar selected: ", which.max(avatarvals$avatar))} 
    else p("Avatar is not selected yet.")
      
  })
  
  # output$avatarbackground <- renderImage(
  #   lisst(src='www/avatarbackground.PNG',style="position:absolute;z-order:0"),deleteFile=FALSE
  # )
  # 
  renderCell <- function(avatarnum){
    renderImage({
      #select the icon appropriate for this cell
      imageid <- avatarnum
      imgsrc=switch(imageid,"www/avatar1.png",
                    "www/avatar2.png",
                    "www/avatar3.png",
                    "www/avatar4.png",
                    "www/avatar5.png",
                    "www/avatar6.png",
                    "www/avatar7.png",
                    "www/avatar8.png")
      if (avatarvals$avatar[avatarnum]==0)
      list(src=imgsrc,style="position:relative;z-order:999;") 
      else
        list(src=imgsrc,style="border:2px solid green;position:relative;z-order:999") 
    },deleteFile=FALSE)
  }

  output$cell1 <- renderCell(1)  
  output$cell2 <- renderCell(2) 
  output$cell3 <- renderCell(3)  
  output$cell4 <- renderCell(4)  
  output$cell5 <- renderCell(5) 
  output$cell6 <- renderCell(6) 
  output$cell7 <- renderCell(7)  
  output$cell8 <- renderCell(8)  
  
  #### AVATAR EVENTS ####
  
  processClickEvent <- function(avatarnum){
    # If it is not this player's turn or if the cell is occupied, then ignore the click
    req(vals$playername)
    #print(avatarnum)
    #get initial status, whether it's new game or change avatar
    initial_status = max(avatarvals$avatar)
    # update avatar number
    avatarvals$avatar <- c(rep(0,8))
    avatarvals$avatar[avatarnum] <- 1
    print(avatarvals$avatar)
    # update gamestate
    changeGameState("home")
    # update scores
    vals$impression <- case_when(
      avatarnum == 1 | avatarnum == 5 ~ 10,
      avatarnum == 2 | avatarnum == 6 ~ 7,
      avatarnum == 3 | avatarnum == 7 ~ 3,
      avatarnum == 4 | avatarnum == 8 ~ 0
    )
    showModal(avatarUpdateModal(changed=initial_status,vals$impression))
  }
  
  observeEvent(input$click1,{processClickEvent(1)})
  observeEvent(input$click2,{processClickEvent(2)})
  observeEvent(input$click3,{processClickEvent(3)})
  observeEvent(input$click4,{processClickEvent(4)})
  observeEvent(input$click5,{processClickEvent(5)})
  observeEvent(input$click6,{processClickEvent(6)})
  observeEvent(input$click7,{processClickEvent(7)})
  observeEvent(input$click8,{processClickEvent(8)})
  observeEvent(input$click9,{print("clicked")})
  
  #### OVERALL SERVER ####
  
  #Functions to update game reactive vals
  
  addGameScore <- function(impression=0,skills=0,confidence=0,social=0){
    # add scores
    vals$impression = vals$impression + impression
    vals$skills = vals$skills + skills
    vals$confidence = vals$confidence + confidence
    vals$social = vals$social + social
  }
  
  changeGameState <- function(newgamestate){
    vals$gamestate <- newgamestate
  }
  
  checkPlayerStats <- function(game){ #game has to be in string
    ##### ENERGY REQUIREMENTS HERE #####
    #add in r list of energy required for each game here
    energy_list <- list(industryseminar = 40,
                        careerfair = 60,
                        skillsworkshop=50,
                        applyforjob=60)
    energy_required = energy_list[[game]]
    #if more than energy required,
    if (vals$energy >= energy_required){
      #change game state
      changeGameState(game)
      #minus player stats
      vals$energy = vals$energy - energy_required
      #show enter game modal
      #create list of game titles first
      gametitles = list(industryseminar = "Industry Seminar",
                        careerfair = "Career Fair",
                        skillsworkshop = "Skills Workshop",
                        applyforjob = "Interview")
      #obtain string
      gamestring = gametitles[[game]]
      showModal(enterGameModal(gamestring,energy_required))
    } else {
      #show failed
      showModal(enterGameModalFailed())
    }
  }
  
  #mini game ends
  gameHasEnded <- function(game,impression,skills,confidence,social){ #make sure game is in string format
    #add in list of score improvements here
    # show modal and add scores and change game state
    showModal(exitGameModal(game,impression,skills,confidence,social))
    addGameScore(impression,skills,confidence,social)
    changeGameState("home")
  }
  
  #sleep
  sleep <- function(){
    vals$days <- vals$days + 1
    vals$energy = 100
    showModal(sleepModal())
  }
  
  #### HOME/LANDING PAGE OUTPUTS ####
  output$landing <- renderUI({
    req(vals$playername) # if vals$playerid is NULL controls will not be available. 
    fluidPage(
      column(width=4,
             fluidRow(
               box(width = NULL,status= "primary",
                   p("Today is day ", tags$b(vals$days),", what will ",vals$playername," do today?"),
                   uiOutput("avatarchoices")
               )),
             fluidRow(
               uiOutput("energylanding"),
             )
      )
      ,
      column(width=4,
             fluidRow(box(width = NULL,height="405px",status="info",
                          p("Player Avatar"),
                          uiOutput("avatarlanding")
             )
             )
      ),
      column(width = 4,
             fluidRow(
               box(width=NULL, status = "warning",
                   p("Scores"),
                   (plotOutput("scoresPlot",width="100%",height="230px")),
                   uiOutput("dynamic")
               )
             ),
             fluidRow(uiOutput("dayslanding"))
      )
      
    )
  })
  
  output$avatarchoices <- renderUI({
    if (vals$gamestate == "home")
      fluidPage(
        column(12,align='center',
               actionButton("sleep","Sleep",style="width:100%;background-color:green;color: white;"),
               br(),
               actionButton("industryseminar","Industry Seminar",style = "width:100%;"),
               br(),
               actionButton("careerfair","Career Fair",style = "width:100%;"),
               br(),
               actionButton("skillsworkshop","Skills Workshop",style = "width:100%;"),
               br(),
               actionButton("changeavatar","Change Avatar",style = "width:100%;background-color:#FCF3CF;"),
               br(),
               actionButton("applyforjob","Apply for Job",style = "width:100%;background-color:#98FB98;")
        )
      ) else{
        if (vals$gamestate != "welcome")
          fluidPage(
            h2("Please go to Mini Game Tab")
          )
        else fluidPage(
          h2("Please go to ",vals$gamestate, " Tab")
        )
      }
  })
  
  output$energyPlot <- renderPlot({
    ggplot() +
      geom_col(aes("", 100)) +
      geom_col(aes("", vals$energy), fill = "forestgreen") +
      coord_flip() +
      theme_minimal() +
      theme(
        #axis.title = element_blank(),
        #axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #panel.background = element_blank()
      )+
      labs(y=paste("Energy:",vals$energy),x=NULL)
    
  })
  
  output$energylanding <- renderUI({
    box(width = NULL,title="Energy",background="blue",
        plotOutput("energyPlot",height="50px")
    )
  })
  
  
  output$dayslanding <- renderUI({
    box(width = NULL,title = "Days passed:",
        background = "light-blue",
        p(tags$b(vals$days,style = "font-size: 30px;"), " days have passed!",style="padding:0px;margin:0px;")
    )
  })
  
  output$avatarlanding <- renderUI({
    p("Your name: ",vals$playername)
    if (sum(avatarvals$avatar) != 0)
      imageOutput("avatarlandingimage")
    else
      p("Avatar not selected yet.")
  })
  
  output$avatarlandingimage <- renderImage({
    imageid <- which.max(avatarvals$avatar)
    imgsrc=switch(imageid,"www/avatar1.png",
                  "www/avatar2.png",
                  "www/avatar3.png",
                  "www/avatar4.png",
                  "www/avatar5.png",
                  "www/avatar6.png",
                  "www/avatar7.png",
                  "www/avatar8.png")
    list(src=imgsrc,
         contentType = 'image/png',height = 200,style="display: block; margin-left: auto; margin-right: auto;")
  },deleteFile=FALSE)
  
  output$scoresPlot <- renderPlot({
    scoresdata <- data.frame(scorescat = c("Skills","Confidence","Impression","Social"),
                             scoresval = c(vals$skills, vals$confidence, vals$impression, vals$social))
    ggplot(scoresdata,aes(x=scorescat,y=scoresval))+geom_col(aes(fill=scorescat))+geom_text(aes(label=scoresval),size=5,vjust=1)+
      labs(x="",y="")+theme_minimal()+theme(legend.position="none")
  })
  
  #### landing instructions output ####
  
  output$landing_instructions <- renderUI({
    fluidPage(
      h3("Instructions"),
      h4("Goal: To apply and get a job in the least possible number of days."),
      p("You have an energy level of 100 each day, and each choice you pick will spend some energy."),
      p("Once energy is spent, you'll have to ",tags$b("sleep")," to regain your energy. The day counter will increase by one."),
      p("Each choice will require you to play a minigame, and give you some score improvements in ", tags$b("impression, skills, confidence, and social")," scores."),
      # game info image
      imageOutput("instructionsimage",height="100%"),
      p("Once you reach a certain score, you can then ", tags$b("Apply for Job"), " to try winning the game!")
    )
  })
  
  output$instructionsimage <- renderImage({
    imgsrc = 'www/instructions.png'
    list(src=imgsrc,style="max-width:700px;width:100%;") 
  },deleteFile = FALSE)
  
  #### LANDING PAGE OBSERVE EVENTS ####
  #sleep
  observeEvent(input$sleep,{
    sleep()
  })
  
  observeEvent(input$industryseminar,{
    checkPlayerStats("industryseminar") #updates game state and minus player stats
    #put tabset mini game panel to instructions page
    updateTabsetPanel(session, "minigametabset",selected = "panel1")
    
  })
  
  observeEvent(input$careerfair,{
    checkPlayerStats("careerfair")
    updateTabsetPanel(session, "minigametabset",selected = "panel1")
  })
  
  observeEvent(input$skillsworkshop,{
    checkPlayerStats("skillsworkshop")
    updateTabsetPanel(session, "minigametabset",selected = "panel1")
  })
  
  observeEvent(input$applyforjob,{
    checkPlayerStats("applyforjob")
    updateTabsetPanel(session, "minigametabset",selected = "panel1")
  })
  
  observeEvent(input$changeavatar,{
    showModal(changeAvatarModal())
    changeGameState("welcome")
  })
  
  #### GAME PAGE OUTPUTS ####
    #### Main game UI (Do not edit here) ####
  output$minigame <- renderUI({
    #put tabs for game and instructions
    mainPanel(
      tabsetPanel(id="minigametabset",
                  tabPanel(title = "Instructions",value = "panel1",uiOutput("minigameinstructions")),
                  tabPanel(title = "Mini Game", value = "panel2",uiOutput("minigameoutput"))
      )
    )
  })
  
  # Mini game instructions tab
  
  output$minigameinstructions <- renderUI({
    if (vals$gamestate == "industryseminar"){
      uiOutput("industryseminarinstructions")
    } else if (vals$gamestate == "careerfair"){
      uiOutput("careerfairinstructions")
    } else if (vals$gamestate == "skillsworkshop"){
      uiOutput("skillsworkshopinstructions")
    } else if (vals$gamestate == "applyforjob"){
      uiOutput("applyforjobinstructions")}
    else {
      p("Mini game not started")
      if (vals$gamestate != "home"&vals$gamestate != "welcome")
        fluidPage(
          p("Please go to Mini Game Tab")
        )
      else fluidPage(
        p("Please go to ",vals$gamestate, " Tab")
      )
    }
  })
  
  # Mini Game output Tab
  output$minigameoutput <- renderUI({
    if (vals$gamestate == "industryseminar"){
      uiOutput("industryseminaroutput")
    } else if (vals$gamestate == "careerfair"){
      uiOutput("careerfairoutput")
    } else if (vals$gamestate == "skillsworkshop"){
      uiOutput("skillsworkshopoutput")
    } else if (vals$gamestate == "applyforjob"){
      uiOutput("applyforjoboutput")}
    else {
      p("Mini game not started")
      if (vals$gamestate != "home"&vals$gamestate != "welcome")
        fluidPage(
          p("Please go to Mini Game Tab")
        )
      else fluidPage(
        p("Please go to ",vals$gamestate, " Tab")
      )
    }
  })
  

  # !!!Add Game Server functions here!!!
  #### GAME PAGE SERVER ####
  
  
  #### industryseminar output####
  output$industryseminarinstructions <- renderUI({
    fluidPage(
      p("You are now in industry seminar."),
      actionButton("industryseminarstart", "Start")
    )
  })
  
  output$industryseminaroutput <- renderUI({
    fluidPage(
      p("You are now in industry seminar game."),
      actionButton("industryseminarend","End Game")
    )
  })
  
  #### industryseminar server ####
  observeEvent(input$industryseminarstart,{
    updateTabsetPanel(session, "minigametabset",selected = "panel2")
  })
  
  
  observeEvent(input$industryseminarend,{
    # Use end game function here
    gameHasEnded("industry seminar",0,2,0,2) #Add scores here
  })  
  
  #Insert below
  
  #### careerfair output####
  
  output$careerfairinstructions<- renderUI({
    fluidPage(
      p("You are now in career fair."),
      actionButton("careerfairstart", "Start")
    )
  })
  
  output$careerfairoutput<- renderUI({
    fluidPage(
      h2("Welcome to Career Fair game"),
      # start button to initialze timer
      uiOutput("careerfairstartbutton"),
      
      fluidRow(
        box(width="100%",
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
            imageOutput("cell44",height="100px",width="100px",click="click44",inline=TRUE), # height and width are for the containing div, not the image itself
            
            tags$br(),
            tags$br(),
            uiOutput("careerfairendbutton")
        )
      ),
      
    )
  })
  
  #### careerfair server ####
  observeEvent(input$careerfairstart,{
    updateTabsetPanel(session, "minigametabset",selected = "panel2")
  })
  
  observeEvent(input$careerfairend,{
    # Use end game function here
    gameHasEnded("career fair",0,0,floor(120/time_r()),floor(120/time_r())) #Add scores here
    gamevals$Status <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
    gamevals$Count <- 0
    time_r(0)
    finished(FALSE)
    gamevals$Images <- randomI()
  })  
    
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
  # ReactiveValues
  " Status reactive matrix : 1: Flash 0:normal"
  Status2 <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  mole <- random()
  score <- 0
  # All Reaction values
  gamevals <- reactiveValues(Status=Status,Identiy=Identiy,Previous=Previous,PPrevious=PPrevious,Wrong=Wrong,Count=Count,Frozen=Frozen,
                             mole=mole,Status2=Status2,score=score,Images=Images)
  # Timer function:
  # timer render by click start button
  # careerfairscore <- reactiveVal(value=0)
  time_r <- reactiveVal(value = 0)
  started <- reactiveVal(value = FALSE)
  finished <- reactiveVal(value=FALSE)
  observeEvent(input$Start, {
    time_r(0)
    finished(FALSE)
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
      paste("Time:",as.character(round(time_r(),digits = 2)))}
    else{
      started(FALSE)
      finished(TRUE)
      paste("Game End:",as.character(round(time_r(),digits = 2)))
    }
  })
  
  # Show the initil layout
  # display the hole and moles
  renderCell <- function(gridrow,gridcol){
    renderImage({
      # original back side of pictures & image
      Blank="www/blank.png"
      Flipped= paste0('www/',gamevals$Images[gridrow,gridcol],'.png')
      # Check click events 
      imgsrc=switch(gamevals$Status[gridrow,gridcol]+1,Blank,Flipped)
      # display the pictures 
      list(src=imgsrc,style="position:relative;z-order:999") 
    },deleteFile=FALSE)
  }
  # show function

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
  
  
  # click 
  processClickEvent1 <- function(gridrow,gridcol){
    if (started()){
      # Show image 
      gamevals$Status[gridrow,gridcol] <- (gamevals$Status[gridrow,gridcol]+1)%%2
      # Get and Check the value 
      if(is.null(gamevals$Identiy)){
        gamevals$Identiy <- gamevals$Images[gridrow,gridcol]
        if(gamevals$Wrong){
          gamevals$Status[gamevals$Previous[1],gamevals$Previous[2]] <- (gamevals$Status[gamevals$Previous[1],gamevals$Previous[2]]+1)%%2
          gamevals$Status[gamevals$PPrevious[1],gamevals$PPrevious[2]] <- (gamevals$Status[gamevals$PPrevious[1],gamevals$PPrevious[2]]+1)%%2
          gamevals$Wrong <- FALSE
        }
      }
      else{
        if(gamevals$Images[gridrow,gridcol]==gamevals$Identiy){
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
    }
  }
  # Click event 
  click <- function(){
    observeEvent(input$click11,{processClickEvent1(1,1)},once =gamevals$Frozen[1,1])
    observeEvent(input$click12,{processClickEvent1(1,2)},once =gamevals$Frozen[1,2])
    observeEvent(input$click13,{processClickEvent1(1,3)},once =gamevals$Frozen[1,3])
    observeEvent(input$click14,{processClickEvent1(1,4)},once =gamevals$Frozen[1,4])
    observeEvent(input$click21,{processClickEvent1(2,1)},once =gamevals$Frozen[2,1])
    observeEvent(input$click22,{processClickEvent1(2,2)},once =gamevals$Frozen[2,2])
    observeEvent(input$click23,{processClickEvent1(2,3)},once =gamevals$Frozen[2,3])
    observeEvent(input$click24,{processClickEvent1(2,4)},once =gamevals$Frozen[2,4])
    observeEvent(input$click31,{processClickEvent1(3,1)},once =gamevals$Frozen[3,1])
    observeEvent(input$click32,{processClickEvent1(3,2)},once =gamevals$Frozen[3,2])
    observeEvent(input$click33,{processClickEvent1(3,3)},once =gamevals$Frozen[3,3])
    observeEvent(input$click34,{processClickEvent1(3,4)},once =gamevals$Frozen[3,4])
    observeEvent(input$click41,{processClickEvent1(4,1)},once =gamevals$Frozen[4,1])
    observeEvent(input$click42,{processClickEvent1(4,2)},once =gamevals$Frozen[4,2])
    observeEvent(input$click43,{processClickEvent1(4,3)},once =gamevals$Frozen[4,3])
    observeEvent(input$click44,{processClickEvent1(4,4)},once =gamevals$Frozen[4,4])
  }
  
  click()
  
  output$careerfairendbutton <- renderUI({
    req(finished())
    actionButton("careerfairend","End Game")
  })
  
  output$careerfairstartbutton <- renderUI({
    if(finished()==FALSE)
    actionButton("Start","Start")
  })
  
  #### skillsworkshop output ####
  
  output$skillsworkshopinstructions<- renderUI({
    fluidPage(
      p("You are now in skills workshop."),
      actionButton("skillsworkshopstart", "Start")
    )
  })
  
  output$skillsworkshopoutput<- renderUI({
    fluidPage(
      uiOutput("skillsworkshopstartbutton"),
      fluidRow(
        box(width="100%",
          # timer 
          h2(textOutput("Time3")),
          # Display teh sentence
          h2(textOutput("Sentence")),
          #textfield for player input
          textInput("text", h3("Text input"), 
                    value = ""))  ,
      #actionButton("skillsworkshopend","End Game")
      tags$br(),
      tags$br(),
      uiOutput("skillsworkshopsendbutton")
    ))
  })
    
    output$skillsworkshopsendbutton <- renderUI({
      req(finished())
      actionButton("skillsworkshopsend","End Game")
    })
    
    output$skillsworkshopstartbutton <- renderUI({
      if(finished()==FALSE)
        actionButton("Start3","Start")
    })
  
    #### skillsworkshop server ####
    
  observeEvent(input$skillsworkshopstart,{
    updateTabsetPanel(session, "minigametabset",selected = "panel2")
  })
  
  observeEvent(input$skillsworkshopsend,{
    # Use end game function here
    gameHasEnded("skills workshop",0,floor(200/time_r()),0,0) #Add scores here
    time_r(0)
    finished(FALSE)
    vals$status <- TRUE
  })  
  
  #Insert below#
  output$Sentence <- renderText({
    # Game reactive values:
    vals$sentence 
  })
  # timer render by click start button
  observeEvent(input$Start3, {
    time_r(0)
    finished(FALSE)
    started(TRUE)
    conn <- getAWSConnection()
    vals$sentence <- getRandomSentence(conn)
    
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
  
  output$Time3 <- renderText({
    if(vals$status){
      paste("Time:",as.character(round(time_r(),digits = 2)))}
    else{
      started(FALSE)
      finished(TRUE)
      paste("Game End:",as.character(round(time_r(),digits = 2)))
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
  

  
  
  #### apply for job output ####
  output$applyforjobinstructions<- renderUI({
    fluidPage(
      p("You are now in interview."),
      actionButton("applyforjobstart", "Start")
    )
  })
  
  output$applyforjoboutput<- renderUI({
    fluidPage(
      p("You are now in interview game."),
      
      h2("Welcome to Apply game"),
      # start button to initialze timer
      uiOutput("applyforjobstartbutton"),
      fluidRow(
        box(width='100%',
          # timer
          h2(textOutput("Time2")),
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
          imageOutput("acell44",height="100px",width="100px",click="click44",inline=TRUE), # height and width are for the containing div, not the image itself
          #sliderInput("finalscore","Final Score:",1,10,1,value = 8),
          tags$br(),
          tags$br(),
          uiOutput("applyforjobendbutton")
      #actionButton("applyforjobend","End Game")))
        ))
    )
  })
  
  output$applyforjobendbutton <- renderUI({
    req(finished())
    actionButton("applyforjobend","End Game")
  })
  
  output$applyforjobstartbutton <- renderUI({
    if(finished()==FALSE)
      actionButton("Start2","Start")
  })
  
  #### apply for job server ####
  observeEvent(input$applyforjobstart,{
    updateTabsetPanel(session, "minigametabset",selected = "panel2")
  })
  #Insert below #
  

  #gamevals <- reactiveValues()
  # timer render by click start button
  observeEvent(input$Start2, {
    time_r(0)
    finished(FALSE)
    started(TRUE)
    gamevals$score=0
  }, ignoreInit = TRUE)
  
  output$Time2 <- renderText({
    if(N*t*0.1-time_r()<0){
      started(FALSE)
      finished(TRUE)
      "Time Runout"
    }
    else{if(as.integer(as.double(time_r())*10) %/% t<=N){
      paste("Time:",as.character(round(N*t*0.1-time_r(),digits=2)))}}
  })
  
  
  # random position update
  observe({if((as.integer(as.double(time_r())*10)%%t==0) && (as.integer((as.double(time_r()))*10) %/% t<N) && (started()) ){ 
    gamevals$mole <- random()
    gamevals$Status2 <- Status2
    show2()
  }})
  
  # display the hole and moles
  renderCell2 <- function(gridrow,gridcol){
    renderImage({
      imgsrc=("www/cross.png")
      for(i in 1:n){
        if (gridrow==gamevals$mole[i,1]&gridcol==gamevals$mole[i,2]){
          imgsrc=("www/mole.png")
          gamevals$Status2[gamevals$mole[i,1],gamevals$mole[i,2]]=1
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
    if ((gamevals$Status2[gridrow,gridcol]==1)&&(started())){
      gamevals$score <- gamevals$score+1
      gamevals$Status2[gridrow,gridcol]==0
    }
  }
  # update score
  output$Score <- renderText(paste("Score",as.character(sum(gamevals$score))))
  
  click2 <- function(){
    observeEvent(input$click11,{processClickEvent2(1,1)},once = TRUE)
    observeEvent(input$click12,{processClickEvent2(1,2)},once = TRUE)
    observeEvent(input$click13,{processClickEvent2(1,3)},once = TRUE)
    observeEvent(input$click14,{processClickEvent2(1,4)},once = TRUE)
    observeEvent(input$click21,{processClickEvent2(2,1)},once = TRUE)
    observeEvent(input$click22,{processClickEvent2(2,2)},once = TRUE)
    observeEvent(input$click23,{processClickEvent2(2,3)},once = TRUE)
    observeEvent(input$click24,{processClickEvent2(2,4)},once = TRUE)
    observeEvent(input$click31,{processClickEvent2(3,1)},once = TRUE)
    observeEvent(input$click32,{processClickEvent2(3,2)},once = TRUE)
    observeEvent(input$click33,{processClickEvent2(3,3)},once = TRUE)
    observeEvent(input$click34,{processClickEvent2(3,4)},once = TRUE)
    observeEvent(input$click41,{processClickEvent2(4,1)},once = TRUE)
    observeEvent(input$click42,{processClickEvent2(4,2)},once = TRUE)
    observeEvent(input$click43,{processClickEvent2(4,3)},once = TRUE)
    observeEvent(input$click44,{processClickEvent2(4,4)},once = TRUE)}
  
  click2()
  
  # End-of-Job-Interview Code #
  observeEvent(input$applyforjobend,{
    #check if final interview score above threshold
    time_r(0)
    finished(FALSE)
    winorlose <- checkFinalScore(gamevals$score)
    if (winorlose == "win"){
      showModal(youWinModal(vals$days))
      #remove player name once game has been won
      vals$playername <- NULL
      vals$playerid <- NULL
      vals$days <- 0
      vals$energy <- 90
      avatarvals$avatar <- c(rep(0,8))
      vals$gamestate <- "welcome"
      vals$skills=0
      vals$impression=0
      vals$social=0
      vals$confidence=0
      gamevals$score <- 0
    }else{
      showModal(makeModal("You Lose!:("))
    }
  })
  
  makeModal <- function(text){
    modalDialog(
                p(text),
                p("Get your scores up and Try again next time."),
                footer=actionButton("loseok","OK")
                )
  }
  
  observeEvent(input$winok,{
    removeModal()
  })
  
  observeEvent(input$loseok,{
    removeModal()
    gameHasEnded("Interview",0,0,gamevals$score,0)
  })
  
  checkFinalScore <- function(score){
    # set threshold score here! #
    sumscore <- vals$social + vals$confidence + vals$impression + vals$skills
    thresholdscore <- max(45 - sumscore,2)
    if (score >= thresholdscore){
      return("win")
    } else {
      return("lose")
    }
  }
  # End-of-Job-Interview Code End #


}
  
  
  
  
  
  shinyApp(ui, server)

