library(shiny)
playerdata <- read.csv(file="fpprojections.csv", colClasses=c("character","character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
colnames(playerdata) = c("Name","Player","Position","Team","Points","VOR","Passing Yards","Passing TDs","Interceptions","Rushing Yards","Rushing TDs","Receptions","Recieving Yards","Recieving TDs","2 Points","Fumbles","posrank","drafted","rank","yahoorank","yposrank","voy")
playerdata$Position <- paste(playerdata$Position, "(", playerdata$posrank,")", sep="")
source("projections.R")
#draftdata <- projpts(50,5,-2,20,6,.5,20,6,2,-1,17,39,48,13)
draftdata <- projections
shinyUI(fluidPage(
  titlePanel("J.A.R.V.I.S."),
  tabsetPanel(
    tabPanel("Main",
             sidebarPanel(
               style="max-width:225px", 
               sliderInput("num", "Number to display:", min=1, max=30, value=12),
               selectInput("pos", "Position:", c("ALL","QB","RB","WR","TE")),
               selectInput("player", "Player", c("All",draftdata$Player), multiple=FALSE, selectize=TRUE, selected="All"),
               # actionButton("myteam", label="My Team"),
               actionButton("draft", label="Drafted"),
               br(),br(),
               selectInput("numOfTeams", "Number of Teams", c(8, 10, 12, 14), selected = 12),
               selectInput("whichPick", "Which Pick", c(1:14), selected = 4),
               numericInput("passyds","Passing Yards", 50),
               numericInput("passtds", "Passing TDs", 5),
               numericInput("ints", "Interceptions", -2),
               numericInput("rushyds", "Rushing Yards", 20),
               numericInput("rushtds", "Rushing TDs", 6),
               numericInput("recs", "Receptions", .5),
               numericInput("recyds", "Receiving Yards", 20),
               numericInput("rectds", "Receiving TDs", 6),
               numericInput("twopts", "Two points", 2),
               numericInput("fumbles", "Fumbles", -2),
               numericInput("numofqb", "Number of QBs taken in first 10 rounds", 17),
               numericInput("numofrb", "Number of RBs taken in first 10 rounds", 39),
               numericInput("numofwr", "Number of WRs taken in first 10 rounds", 48),
               numericInput("numofte", "Number of TEs taken in first 10 rounds", 13)
             ),
             mainPanel(
               h4(paste0("Current Pick: "), textOutput("currentPick")),
               fluidRow(
                 column(12,
                        h2(textOutput("name"))
                 )
               ),
               fluidRow(
                 column(3,
                        uiOutput("pic")
                        # plotOutput("pic", width = "500")
                 )
               ),
               fluidRow(
                 column(5,
                        h3(textOutput("position"))
                 )
               ),
               uiOutput("myTeamHeader"),
               uiOutput("myteam"),
               DT::dataTableOutput("view")
               #     plotOutput("hist")
             )
    ),
    tabPanel("Team View",
             sidebarPanel(
               style="max-width:225px", 
               selectInput("teamToView", "View Team:", c(1:12))
             ),
             mainPanel(
               tableOutput("teamViewer")
             )
    ),
    tabPanel("Position Matrix",
             mainPanel(
               tableOutput("matrixViewer")
             )
    )
  )
)
)