library(shiny)
library(tidyr)
library(ggplot2)
library(DT)

source("projections.R")
source("yahooRanksChart.R")
source("getRound.R")
source("points_by_position_chart.R")
source("player_game_stats.R")
source("get_team_players.R")

drafted <- data.frame()
logos <- read.csv(file = "logos.csv", header = T, stringsAsFactors = F)
consistency <- get_consistency()
# useAltURL <- F

shinyServer(function(input, output, clientData, session) {
  
  drafted <- data.frame()
  
  my_team <- data.frame()
  draftdata <- projections
  observe({
    draftdata <- projpts(projections,input$passyds, input$passtds, input$ints, input$rushyds, input$rushtds, input$recs, input$recyds, input$rectds, input$twopts, input$fumbles, (input$numOfTeams * input$numofqb), (input$numOfTeams * input$numofrb), (input$numOfTeams * input$numofwr), (input$numOfTeams * input$numofte))
    draftdata$Pos <- paste(draftdata$Pos,"(",draftdata$PosRank,")", sep="")
    # draftdata$PosRank <- NULL
    datasetInput <- reactive({
      temp <- playerSet()
      if(input$player != "All") {
        subset(temp, temp$Player==input$player)
      }
      else {
        # temp[c(1:input$num),]
        temp
      }
    })
    playerSet <- reactive({
      if(input$pos != "ALL") {
        temp <- subset(draftdata, substr(draftdata$Pos, 1,2)==input$pos)
        temp[!(temp$Player %in% drafted$Player),]
      }
      else {
        draftdata[!(draftdata$Player %in% drafted$Player),]
      }
    })
    
    output$name <- renderText({
      if(input$player !="All") {
        input$player
      }
    })
    
    output$position <- renderText({
      temp <- subset(draftdata , draftdata$Player==input$player[1])
      paste(temp[,2], temp[,3], sep=' - ')
    })
    
    tableOutput <- reactive({
      a <- datasetInput() %>% data.frame() %>%
        mutate(YRound = as.integer(getRound(.$YRank, as.numeric(input$numOfTeams))))
      a <- merge(a, logos %>% select(Team, Logo), by = "Team")
      if(nrow(a) > 0) {
        a$Team <- paste0("<img src=\"", a$Logo, "\" height = 40></img>")
        a$Logo <- NULL
        a <- a[order(-a$VOR),]
        rownames(a) <- c(1:nrow(a))
      }
      a
    })
    
    output$playerList <- DT::renderDataTable(
      tableOutput(), 
      options = list(pageLength = 25),
      rownames = F,
      escape = F
    )
    
    
    consistencyOutput <- reactive({
      t <- consistency
      colnames(t) <- c("Player", "Pos", "GP", "Starts", "Top", "Pts/G", "StDev", "Start%", "Stud%", "Cons.", "Metric")
      t$GP <- as.integer(t$GP)
      t$Starts <- as.integer(t$Starts)
      t$Top <- as.integer(t$Top)
      for(i in 6:ncol(t)) {
        t[,i] <- round(t[,i], 2)
      }
      t[,8] <- t[,8] * 100
      t[,9] <- t[,9] * 100
      t[order(-t$Metric),]
    })
    
    consistencyTable <- reactive({
      t <- consistencyOutput()
      if(input$consPlayer != "All") {
        t <- t %>% filter(Player == input$consPlayer)
      }
      if(input$consPos != "All") {
        t <- t %>% filter(Pos == input$consPos)
      }
      t
    })
    
    output$allConsistency <- DT::renderDataTable(
      consistencyTable(),
      options = list(pageLength = 25),
      rownames = F,
      escape = F
    )
    
    output$consistency <- renderUI({
      # print("")
      # print("asdf")
      # t <- consistencyOutput()
      # if(nrow(t) > 0 && input$player != "All") {
      #   renderTable(
      #     t
      #   )
      # }
      if(input$player != "All") {
        t <- consistencyOutput() %>% filter(Player == input$player)
        box(title = "", width = 9,
            renderTable(t))
      }
    })
    
    # output$consistency <- DT::renderDataTable(
    #   consistencyOutput(),
    #   options = list(pageLength = 25),
    #   rownames = F,
    #   escape = F
    # )
    
    # output$currentPick <- renderText({
    #   nrow(drafted) + 1
    # })
    
    output$aa <- renderTable({
      myTeamFormatting()
    })
    
    output$myteam <- renderUI({
      if(nrow(myTeamFormatting()) > 0 && input$player == "All") {
        box(title = "My Team", width = 9,
            renderTable(myTeamFormatting()))
      }
    })
    
    output$currentPick <- renderValueBox({
      if((nrow(drafted) + 1) %in% picks()) {
        boxColor = "red"
      } else if ((nrow(drafted) + 1) %in% (picks() - 1)) {
        boxColor = "yellow"
      } else {
        boxColor = "green"
      }
      valueBox(
        (nrow(drafted) + 1), "Current Pick", icon = icon("ok", lib = "glyphicon"),
        color = boxColor
      )
    })
    
    output$myNextPick <- renderValueBox({
      valueBox(
        picks()[min(which(picks() > (nrow(drafted) + 1)))], "My Next Pick", icon = icon("ok", lib = "glyphicon"),
        color = "green"
      )
    })
    
    picks <- reactive({
      sort(c(seq(as.numeric(input$whichPick), as.numeric(input$numOfTeams) * 16, by = as.numeric(input$numOfTeams) * 2), 
             seq((as.numeric(input$numOfTeams)*2 + 1) - as.numeric(input$whichPick), as.numeric(input$numOfTeams) * 16, by = as.numeric(input$numOfTeams) * 2)))
    })
    
    myTeamFormatting <- reactive({
      
      get_team_players(drafted, input$whichPick, input$numOfTeams)
    
    })
    
    output$teamViewer <- renderTable({
      
      get_team_players(drafted, input$teamToView, input$numOfTeams)
      
    }, include.rownames=FALSE)
    
    output$matrixViewer <- renderTable({
      
      temp <- drafted
      temp$Pos <- substr(temp$Pos, 1, 2)
      matrix <- data.frame(matrix(ncol = 4, nrow = as.numeric(input$numOfTeams)))
      colnames(matrix) <- c("QB", "RB", "WR", "TE")
      # matrix$Team <- c(1:input$numOfTeams)
      for(i in 1:input$numOfTeams) {
        round <- (nrow(temp) %/% as.numeric(input$numOfTeams)) + 1
        picks <- sort(c(seq(i, as.numeric(input$numOfTeams) * 16, by = as.numeric(input$numOfTeams) * 2), 
                        seq((as.numeric(input$numOfTeams)*2 + 1) - i, as.numeric(input$numOfTeams) * 16, by = as.numeric(input$numOfTeams) * 2)))
        t <- temp[picks[which(picks <= nrow(temp))],]
        matrix$QB[i] <- t %>% filter(Pos == "QB") %>% nrow()
        matrix$RB[i] <- t %>% filter(Pos == "RB") %>% nrow()
        matrix$WR[i] <- t %>% filter(Pos == "WR") %>% nrow()
        matrix$TE[i] <- t %>% filter(Pos == "TE") %>% nrow()
      }
      rownames(matrix) <- c("Brian", "Bryan", "John", "Me", "Steven", "Ben",
                            "Dan", "Camen", "Kyle", "Raul", "Chip", "Ross")
      t(matrix)
      
    }, include.colnames = T, include.rownames = T)
    
    output$rankingsChart <- renderUI({
      if(input$player != "All") {
        graph <- rankingsChart(input$player)
        output$p <- renderPlot({graph})
        plotOutput("p", width = "500")
      }
    })
    
    
    output$qbPointsChart <- renderPlot({
      points_by_position_chart("QB", draftdata %>% filter(YRank != 0), input$numOfTeams, input$numofqb)
    })
    
    output$rbPointsChart <- renderPlot({
      points_by_position_chart("RB", draftdata %>% filter(YRank != 0), input$numOfTeams, input$numofrb)
    })
    
    output$wrPointsChart <- renderPlot({
      points_by_position_chart("WR", draftdata %>% filter(YRank != 0), input$numOfTeams, input$numofwr)
    })
    
    output$tePointsChart <- renderPlot({
      points_by_position_chart("TE", draftdata %>% filter(YRank != 0), input$numOfTeams, input$numofte)
    })
    
    
    
    input$draft
    isolate({
      drafted <<- rbind(drafted, subset(draftdata, draftdata$Player == input$player))
    })
    
    updateSelectInput(session, "player", choices = c("All", playerSet()$Player), selected="All")
    updateSelectInput(session, "consPlayer", choices = c("All", playerSet()$Player), selected="All")
    
  })
})