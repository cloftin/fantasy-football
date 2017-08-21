library(shiny)
library(shinydashboard)
playerdata <- read.csv(file="fpprojections.csv", colClasses=c("character","character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
colnames(playerdata) = c("Name","Player","Position","Team","Points","VOR","Passing Yards","Passing TDs","Interceptions","Rushing Yards","Rushing TDs","Receptions","Recieving Yards","Recieving TDs","2 Points","Fumbles","posrank","drafted","rank","yahoorank","yposrank","voy")
playerdata$Position <- paste(playerdata$Position, "(", playerdata$posrank,")", sep="")
source("projections.R")
#draftdata <- projpts(50,5,-2,20,6,.5,20,6,2,-1,17,39,48,13)
draftdata <- projections
teamNumbers <- c(1:12)
names(teamNumbers) = c("Brian", "Bryan", "John", "Me", "Steven", "Ben",
                       "Dan", "Camen", "Kyle", "Raul", "Chip", "Ross")
# shinyUI(fluidPage(
# titlePanel("J.A.R.V.I.S."),
# tabsetPanel(
# tabPanel("Main",
dashboardPage(header = dashboardHeader(title = "JARVIS"), skin = "green",
              dashboardSidebar(disable = TRUE),
              body = dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                ),
                tabBox(width = 12,
                       tabPanel("Main",
                                br(),
                                fluidRow(
                                  column(3,
                                         fluidRow(
                                           box(title = "Draft Controls", solidHeader = T, status = "success",
                                               collapsible = T, width = 12,
                                               selectInput("pos", "Position:", c("ALL","QB","RB","WR","TE")),
                                               selectInput("player", "Player", c("All",draftdata$Player), multiple=FALSE, selectize=TRUE, selected="All"),
                                               # actionButton("myteam", label="My Team"),
                                               actionButton("draft", label="Drafted"),
                                               br(),br(),
                                               selectInput("numOfTeams", "Number of Teams", c(8, 10, 12, 14), selected = 12),
                                               selectInput("whichPick", "Which Pick", c(1:14), selected = 4)
                                           )),
                                         fluidRow(
                                           box(title = "Scoring", solidHeader = T, status = "success",
                                               collapsible = TRUE, collapsed = T, width = 12,
                                               numericInput("passyds","Passing Yards", 25),
                                               numericInput("passtds", "Passing TDs", 5),
                                               numericInput("ints", "Interceptions", -2),
                                               numericInput("rushyds", "Rushing Yards", 10),
                                               numericInput("rushtds", "Rushing TDs", 6),
                                               numericInput("recs", "Receptions", .5),
                                               numericInput("recyds", "Receiving Yards", 10),
                                               numericInput("rectds", "Receiving TDs", 6),
                                               numericInput("twopts", "Two points", 2),
                                               numericInput("fumbles", "Fumbles", -2),
                                               numericInput("numofqb", "Number of QBs taken in first 10 rounds", 12),
                                               numericInput("numofrb", "Number of RBs taken in first 10 rounds", 24),
                                               numericInput("numofwr", "Number of WRs taken in first 10 rounds", 36),
                                               numericInput("numofte", "Number of TEs taken in first 10 rounds", 12)
                                           )
                                         )
                                  ), column(6,
                                            fluidRow(
                                              valueBoxOutput("currentPick"),
                                              valueBoxOutput("myNextPick"),
                                              h2(textOutput("name")),
                                              h3(textOutput("position")),
                                              uiOutput("rankingsChart"),
                                              br(),
                                              uiOutput("myteam"),
                                              DT::dataTableOutput("playerList"),
                                              uiOutput("consistency")
                                            )
                                  )
                                )
                       ),
                       tabPanel("Team View",
                                br(),
                                fluidRow(
                                  column(3,
                                         box(title = "Team View", solidHeader = T, status = "success", width = 12,
                                             selectInput("teamToView", "", teamNumbers)
                                         )
                                  ),
                                  column(9,
                                         box(title = "Team", solidHeader = T, status = "success", width = 6,
                                             tableOutput("teamViewer")
                                         )
                                  )
                                )
                       ),
                       tabPanel("Position Matrix",
                                br(),
                                fluidRow(
                                  column(12,
                                         box(title = "Positions by Team", solidHeader = T, status = "success", width = 10,
                                             tableOutput("matrixViewer")
                                         )
                                  )
                                )
                       ),
                       tabPanel("Points By Position",
                                br(),
                                fluidRow(
                                  column(6,
                                         plotOutput("qbPointsChart")
                                  ),
                                  column(6,
                                         plotOutput("rbPointsChart")
                                  )
                                ),
                                fluidRow(
                                  column(6,
                                         plotOutput("wrPointsChart")
                                  ),
                                  column(6,
                                         plotOutput("tePointsChart")
                                  )
                                )
                                
                       )
                )
              )
)