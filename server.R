library(shiny)
#playerdata <- read.csv(file="fpprojections.csv", colClasses=c("character","character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
#colnames(playerdata) = c("Name","Player","Position","Team","Points","VOR","Passing Yards","Passing TDs","Interceptions","Rushing Yards","Rushing TDs","Receptions","Recieving Yards","Recieving TDs","2 Points","Fumbles","posrank","drafted","Rank","Yahoo Rank","yposrank","VOY")
#playerdata$Position <- paste(playerdata$Position, "(", playerdata$posrank,")", sep="")
drafted <- data.frame()

# myTeam <- data.frame(Pos = c("QB", "WR", "WR", "WR", "RB", "RB", "TE", "", "K", "DEF", "", "", "", "", ""),
# Player = c(rep("", 15)), Team = c(rep("", 15)))
source("projections.R")
source("yahooRanksChart.R")
# draftdata <- draftdata

shinyServer(function(input, output, clientData, session) {
  #playerdata <- read.csv(file="fpprojections.csv", colClasses=c("character","character","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
  #colnames(playerdata) = c("Name","Player","Position","Team","Points","VOR","Passing Yards","Passing TDs","Interceptions","Rushing Yards","Rushing TDs","Receptions","Recieving Yards","Recieving TDs","2 Points","Fumbles","posrank","drafted","Rank","YahooRank","yposrank","VOY")
  #playerdata$Position <- paste(playerdata$Position, "(", playerdata$posrank,")", sep="")
  drafted <- data.frame()
  
  my_team <- data.frame()
  draftdata <- projections
  observe({
    draftdata <- projpts(projections,input$passyds, input$passtds, input$ints, input$rushyds, input$rushtds, input$recs, input$recyds, input$rectds, input$twopts, input$fumbles, input$numofqb, input$numofrb, input$numofwr, input$numofte)
    draftdata$Pos <- paste(draftdata$Pos,"(",draftdata$PosRank,")", sep="")
    draftdata$PosRank <- NULL
    #draftdata$VOY <- draftdata$Rank - draftdata$YRanking
    datasetInput <- reactive({
      temp <- playerSet()
      if(input$player != "All") {
        subset(temp, temp$Player==input$player)
      }
      else {
        temp[c(1:input$num),]
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
    
    output$view <- renderTable({
      a <- as.data.frame(datasetInput())
      a
    }, include.rownames=FALSE)
    
    #     temp55 <- subset(draftdata, draftdata$Player == input$player)
    #     print(subset(playerSet(), playerSet()$Player == input$player)$Pos[1])
    #     if(substr(temp$Pos[1], 1, 2) == "QB") {
    #       if(myTeam$Player[1] == "") {
    #         w <- 1
    #       } else {
    #         w <- which(myTeam[,1] == "")
    #         w <- w[w > 8][1]
    #       }
    #       #         w <- 1
    #       myTeam$Pos[w] <- temp$Pos
    #       myTeam$Player[1] <<- temp$Player
    #       myTeam$Team[w] <- temp$Team
    #     }
    
    output$currentPick <- renderText({
      nrow(drafted) + 1
    })
    
    output$myTeamHeader <- renderUI({
      t <- myTeamFormatting()
      if(nrow(t) > 0 & input$player == "All") {
        h3("My Team: ")
      }
    })
    
    output$myteam <- renderUI({
      t <- myTeamFormatting()
      if(nrow(t) > 0 & input$player == "All") {
        output$t <- renderTable({t}, include.rownames = F)
        tableOutput("t")
      }
    })
    
    myTeam <- reactive({
      temp <- drafted
      round <- (nrow(temp) %/% as.numeric(input$numOfTeams)) + 1
      picks <- sort(c(seq(as.numeric(input$whichPick), as.numeric(input$numOfTeams) * 16, by = as.numeric(input$numOfTeams) * 2), 
                      seq((as.numeric(input$numOfTeams)*2 + 1) - as.numeric(input$whichPick), as.numeric(input$numOfTeams) * 16, by = as.numeric(input$numOfTeams) * 2)))
      temp <- temp[picks[which(picks <= nrow(temp))],]
      temp$Pos <- substr(temp$Pos, 1, 2)
      temp
    })
    
    myTeamFormatting <- reactive({
      # print(my_team)
      # print(drafted)
      temp <- myTeam()
      qbs <- subset(temp, Pos=="QB")
      rbs <- subset(temp, Pos=="RB")
      wrs <- subset(temp, Pos=="WR")
      tes <- subset(temp, Pos=="TE")
      starters <- data.frame()
      bench <- data.frame()
      
      if(nrow(qbs) != 0) {
        starters <- qbs[1,]
        if(nrow(qbs) > 1) {
          bench <- qbs[c(2:nrow(qbs)),]
        }
      }
      
      if(nrow(rbs) != 0) {
        if(nrow(rbs) == 1) {
          starters <- rbind(starters, rbs[1,])
        } else if(nrow(rbs) == 2) {
          starters <- rbind(starters, rbs[c(1:2),])
        } else {
          starters <- rbind(starters, rbs[c(1:2),])
          bench <- rbind(bench, rbs[c(3:nrow(rbs)),])
        }
      }
      
      if(nrow(wrs) != 0) {
        if(nrow(wrs) == 1) {
          starters <- rbind(starters, wrs[1,])
        } else if(nrow(wrs) == 2) {
          starters <- rbind(starters, wrs[c(1:2),])
        } else if(nrow(wrs) == 3) {
          starters <- rbind(starters, wrs[c(1:3),])
        } else {
          starters <- rbind(starters, wrs[c(1:3),])
          bench <- rbind(bench, wrs[c(4:nrow(wrs)),])
        }
      }
      
      if(nrow(tes) != 0) {
        starters <- rbind(starters, tes[1,])
        if(nrow(tes) > 1) {
          bench <- rbind(bench, tes[c(2:nrow(tes)),])
        }
      }
      #       starters <- starters[, c(3,1,4)]
      #       bench <- bench[, c(3,1,4)]
      if(nrow(bench) > 0) {
        flex <- subset(bench, bench$Pos != "QB")
        flex <- flex[1,]
        flex$Pos <- "W/R/T"
        bench <- bench[-1,]
        starters <- rbind(starters, flex)
      }
      
      if(nrow(starters) > 0) {
        output <- rbind(starters, c("--", "---------", "---"), bench)
        output <- output[,c(2,1,3)]
      } else {
        output <- data.frame()
      }
      output
    })
    
    output$teamViewer <- renderTable({
      temp <- drafted
      round <- (nrow(temp) %/% as.numeric(input$numOfTeams)) + 1
      picks <- sort(c(seq(as.numeric(input$teamToView), as.numeric(input$numOfTeams) * 16, by = as.numeric(input$numOfTeams) * 2), 
                      seq((as.numeric(input$numOfTeams)*2 + 1) - as.numeric(input$teamToView), as.numeric(input$numOfTeams) * 16, by = as.numeric(input$numOfTeams) * 2)))
      temp <- temp[picks[which(picks <= nrow(temp))],]
      temp$Pos <- substr(temp$Pos, 1, 2)
      qbs <- subset(temp, Pos=="QB")
      rbs <- subset(temp, Pos=="RB")
      wrs <- subset(temp, Pos=="WR")
      tes <- subset(temp, Pos=="TE")
      starters <- data.frame()
      bench <- data.frame()
      
      if(nrow(qbs) != 0) {
        starters <- qbs[1,]
        if(nrow(qbs) > 1) {
          bench <- qbs[c(2:nrow(qbs)),]
        }
      }
      
      if(nrow(rbs) != 0) {
        if(nrow(rbs) == 1) {
          starters <- rbind(starters, rbs[1,])
        } else if(nrow(rbs) == 2) {
          starters <- rbind(starters, rbs[c(1:2),])
        } else {
          starters <- rbind(starters, rbs[c(1:2),])
          bench <- rbind(bench, rbs[c(3:nrow(rbs)),])
        }
      }
      
      if(nrow(wrs) != 0) {
        if(nrow(wrs) == 1) {
          starters <- rbind(starters, wrs[1,])
        } else if(nrow(wrs) == 2) {
          starters <- rbind(starters, wrs[c(1:2),])
        } else if(nrow(wrs) == 3) {
          starters <- rbind(starters, wrs[c(1:3),])
        } else {
          starters <- rbind(starters, wrs[c(1:3),])
          bench <- rbind(bench, wrs[c(4:nrow(wrs)),])
        }
      }
      
      if(nrow(tes) != 0) {
        starters <- rbind(starters, tes[1,])
        if(nrow(tes) > 1) {
          bench <- rbind(bench, tes[c(2:nrow(tes)),])
        }
      }
      #       starters <- starters[, c(3,1,4)]
      #       bench <- bench[, c(3,1,4)]
      if(nrow(bench) > 0) {
        flex <- subset(bench, bench$Pos != "QB")
        flex <- flex[1,]
        flex$Pos <- "W/R/T"
        bench <- bench[-1,]
        starters <- rbind(starters, flex)
      }
      
      if(nrow(starters) > 0) {
        output <- rbind(starters, c("--", "---------", "---"), bench)
        output <- output[,c(2,1,3)]
      } else {
        output <- data.frame()
      }
    }, include.rownames=FALSE)
    
    output$matrixViewer <- renderTable({
      
      temp <- drafted
      temp$Pos <- substr(temp$Pos, 1, 2)
      matrix <- data.frame(matrix(ncol = 5, nrow = as.numeric(input$numOfTeams)))
      colnames(matrix) <- c("Team", "QB", "RB", "WR", "TE")
      matrix$Team <- c(1:input$numOfTeams)
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
      t(matrix)
    }, include.colnames = F)
    #     output$pic <- renderText({
    #       temp <- tolower(gsub(" ", "-", input$player))
    #       w <- which(directory$player==input$player)
    #       paste("http://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/",directory$id[w],".png",sep="")
    #     })   
    
    output$pic <- renderUI({
      if(input$player != "All") {
        graph <- rankingsChart(input$player)
        output$p <- renderPlot({graph})
        plotOutput("p", width = "500")
      }
    })

    # output$pic <- renderPlot({
    #   if(input$player != "All") {
    #     rankingsChart(input$player)
    #   }
    # })
    #     output$hist <- renderPlot({
    #       try(barplot(table(substr(drafted$Position, 1,2))))
    #     })
    
    # drafted <<- {
    #   input$draft
    #   isolate(rbind(drafted, subset(draftdata, draftdata$Player==input$player)))
    # }
    # input$draft
    # isolate({
    #   drafted <<- rbind(drafted, subset(draftdata, draftdata$Player==input$player))
    # })
    # drafted <<- reactiveValues(Player = NULL)
    # myteam <<- reactiveValues(Player = NULL)
    # 
    # observeEvent(input$draft, {
    #   drafted <<- rbind(drafted, input$player)
    # })
    # observeEvent(input$myteam, {
    #   myteam <<- rbind(my_team, input$player)
    # })
    input$draft
    isolate({
      drafted <<- rbind(drafted, subset(draftdata, draftdata$Player == input$player))
    })
    updateSelectInput(session, "player", choices = c("All", playerSet()$Player), selected="All")
  })
})