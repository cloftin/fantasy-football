library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(FantasyFootballData)

# source("get_projections.R")
source("yahooRanksChart.R")
source("getRound.R")
source("points_by_position_chart.R")
# source("player_game_stats.R")
# source("weekly_fantasy_points.R")

drafted <- data.frame()
projections <- FantasyFootballData::get_projections()
logos <- read.csv(file = "logos.csv", header = T, stringsAsFactors = F)
consistency <- FantasyFootballData::get_consistency()
gamelogs <- read.csv(file = "gamelogs.csv", header = T, stringsAsFactors = F)
gamelogs$Player[gamelogs$Player == "Odell Beckham"] <- "Odell Beckham Jr."
gamelogs <- plyr::ldply(list.files("data/gamelogs/"), function(x) {
  return(read.csv(file = paste0("data/gamelogs/", x), header = T, stringsAsFactors = F))
})
gamelogs$player[gamelogs$player == "Odell Beckham"] <- "Odell Beckham Jr."
# useAltURL <- F

shinyServer(function(input, output, clientData, session) {
  
  drafted <- data.frame()
  
  my_team <- data.frame()
  draftdata <- projections
  observe({
    draftdata <- FantasyFootballData::projected_points(projections,input$passyds, input$passtds, input$ints, input$rushyds, input$rushtds, input$recs, input$recyds, input$rectds, input$twopts, input$fumbles, (input$numOfTeams * input$numofqb), (input$numOfTeams * input$numofrb), (input$numOfTeams * input$numofwr), (input$numOfTeams * input$numofte))
    draftdata$Pos <- paste(draftdata$Pos,"(",draftdata$PosRank,")", sep="")
    draftdata$YRankVOR <- draftdata$VOR * draftdata$YRank
    draftdata$YRankVOR[draftdata$YRank == -999] <- 0
    
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
        t <- t %>% filter(player == input$consPlayer)
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
      
      if(input$player != "All") {
        t <- consistencyOutput() %>% filter(Player == input$player)
        box(title = "", width = 9,
            renderTable(t))
      }
      
    })
    
    output$playerPicture <- renderUI({
      if(input$player != "All") {
        if(input$player == "Odell Beckham Jr.") {
          img(src="Odell Beckham.jpg", height = 100)
        } else {
          file = paste0(input$player, ".jpg")
          img(src=file, height = 100)
        }
      }
    })
    
    
    # output$aa <- renderTable({
    #   myTeamFormatting()
    # })
    # 
    
    playerGamelog <- reactive({
      t <- gamelogs %>% filter(Player == input$gamelogPlayer & !is.na(game_num) & game_num <= 16)
      t <- gamelogs %>% filter(player == input$gamelogPlayer & year == input$gamelogYear & !is.na(game_num) & game_num <= 16)
      t <- t[order(t$game_num),]
      t[is.na(t)] <- 0
      t$pts <- weekly_fantasy_points(t)
      if(t$Pos[1] == "QB") {
        t <- t %>% select(Player, game_num, pass_att, pass_cmp, pass_yds, pass_td, pass_int, rush_att, rush_yds, rush_td, pts)
      if(t$position[1] == "QB") {
        t <- t %>% select(player, game_num, pass_att, pass_cmp, pass_yds, pass_td, pass_int, rush_att, rush_yds, rush_td, pts)
        colnames(t) <- c("Player", "Game", "Attempts", "Comps", "PassYds", "PassTDs", "INTs", "Rushes", "RushYds", "RushTDs", "FPts")
      } else if(t$Pos[1] == "RB") {
        t <- t %>% select(Player, game_num, rush_att, rush_yds, rush_td, targets, rec, rec_yds, rec_td, kick_ret_yds, kick_ret_td, punt_ret_yds, punt_ret_td, pts)
      } else if(t$position[1] == "RB") {
        t <- t %>% select(player, game_num, rush_att, rush_yds, rush_td, targets, rec, rec_yds, rec_td, kick_ret_yds, kick_ret_td, punt_ret_yds, punt_ret_td, pts)
        colnames(t) <- c("Player", "Game", "Rushes", "RushYds", "RushTDs", "Targets", "Recs", "RecYds", "RecTDs",
                         "KRetYds", "KRetTDs", "PRetYds", "PRetTDs", "FPts")
      } else if(t$Pos[1] == "WR") {
        t <- t %>% select(Player, game_num, targets, rec, rec_yds, rec_td, kick_ret_yds, kick_ret_td, punt_ret_yds, punt_ret_td, pts)
      } else if(t$position[1] == "WR") {
        t <- t %>% select(player, game_num, targets, rec, rec_yds, rec_td, kick_ret_yds, kick_ret_td, punt_ret_yds, punt_ret_td, pts)
        colnames(t) <- c("Player", "Game", "Targets", "Recs", "RecYds", "RecTDs",
                         "KRetYds", "KRetTDs", "PRetYds", "PRetTDs", "FPts")
      } else if(t$Pos[1] == "TE") {
        t <- t %>% select(Player, game_num, targets, rec, rec_yds, rec_td, pts)
      } else if(t$position[1] == "TE") {
        t <- t %>% select(player, game_num, targets, rec, rec_yds, rec_td, pts)
        colnames(t) <- c("Player", "Game", "Targets", "Recs", "RecYds", "RecTDs", "FPts")
      }
      w <- which(colnames(t) %in% c("KRetYds", "KRetTDs", "PRetYds", "PRetTDs"))
      ww <- which(cbind(colSums(t[,c(3:ncol(t))])) == 0) + 2
      w <- ww[ww %in% w]
      if(length(w) > 0) {
        t <- t[,-w]
      }
      totals <- as.data.frame(matrix(ncol = ncol(t), nrow = 1))
      colnames(totals) <- colnames(t)
      totals$Player <- t$Player[1]
      totals$Game <- "Season"
      for(i in 3:ncol(t)) {
        totals[,i] <- sum(t[,i])
      }
      t <- rbind(t, totals)
      t
    })
    
    output$gamelog <- renderTable({
      playerGamelog()
    })
    
    yearlyPlayer <- reactive({
      t <- gamelogs %>% filter(player == input$yearlyPlayer & !is.na(game_num) & game_num <= 16)
      t <- t[order(t$game_num),]
      t[is.na(t)] <- 0
      t$pts <- weekly_fantasy_points(t)
      if(t$position[1] == "QB") {
        t <- t %>% select(player, year, pass_att, pass_cmp, pass_yds, pass_td,
                          pass_int, rush_att, rush_yds, rush_td, pts) %>%
          group_by(player, year) %>%
          dplyr::summarize(pass_att = sum(pass_att), pass_cmp = sum(pass_cmp), cmp_pct = sum(pass_cmp)/sum(pass_att),
                           pass_yds = sum(pass_yds), pass_td = sum(pass_td), pass_int = sum(pass_int),
                           rush_att = sum(rush_att), rush_yds = sum(rush_yds), rush_td = sum(rush_td),
                           pts = sum(pts)) %>%
          data.frame()
        
        colnames(t) <- c("Player", "Year", "Attempts", "Comps", "Comp %", "PassYds", "PassTDs", "INTs", "Rushes", "RushYds", "RushTDs", "FPts")
      } else if(t$position[1] == "RB") {
        t <- t %>% select(player, year, rush_att, rush_yds, rush_td, targets, rec, rec_yds,
                          rec_td, kick_ret_yds, kick_ret_td, punt_ret_yds, punt_ret_td, pts) %>%
          group_by(player, year) %>%
          dplyr::summarize(rush_att = sum(rush_att), rush_yds = sum(rush_yds), rush_td = sum(rush_td),
                           targets = sum(targets), rec = sum(rec), rec_yds = sum(rec_yds),
                           rec_td = sum(rec_td), pts = sum(pts)) %>%
          data.frame()
        
        colnames(t) <- c("Player", "Year", "Rushes", "RushYds", "RushTDs", "Targets", 
                         "Recs", "RecYds", "RecTDs", "FPts")
      } else if(t$position[1] == "WR") {
        t <- t %>% select(player, year, rush_att, rush_yds, rush_td, targets, rec, rec_yds,
                          rec_td, kick_ret_yds, kick_ret_td, punt_ret_yds, punt_ret_td, pts) %>%
          group_by(player, year) %>%
          dplyr::summarize(targets = sum(targets), rec = sum(rec), rec_yds = sum(rec_yds), rec_td = sum(rec_td),
                           kick_ret_yds = sum(kick_ret_yds), kick_ret_td = sum(kick_ret_td),
                           punt_ret_yds = sum(punt_ret_yds), punt_ret_td = sum(punt_ret_td),
                           pts = sum(pts)) %>%
          data.frame()
        
        colnames(t) <- c("Player", "Year", "Targets", "Recs", "RecYds", "RecTDs",
                         "KRetYds", "KRetTDs", "PRetYds", "PRetTDs", "FPts")
        
      } else if(t$position[1] == "TE") {
        t <- t %>% select(player, year, rush_att, rush_yds, rush_td, targets, rec, rec_yds,
                          rec_td, pts) %>%
          group_by(player, year) %>%
          dplyr::summarize(targets = sum(targets), rec = sum(rec), rec_yds = sum(rec_yds),
                           rec_td = sum(rec_td), pts = sum(pts)) %>%
          data.frame()
        
        colnames(t) <- c("Player", "Year", "Targets", "Recs", "RecYds", "RecTDs", "FPts")
      }
      
      for(i in 3:(ncol(t) - 1)) {
        t[,i] <- as.integer(as.character(t[,i]))
      }
      t
    })
    
    output$yearlystats <- renderTable({
      yearlyPlayer()
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
    
    output$allPointsChart <- renderPlot({
      points_by_position_chart("All", draftdata %>% filter(YRank != 0), input$numOfTeams)
    })
    
    
    input$draft
    isolate({
      drafted <<- rbind(drafted, subset(draftdata, draftdata$Player == input$player))
    })
    
    updateSelectInput(session, "player", choices = c("All", playerSet()$Player), selected="All")
    updateSelectInput(session, "consPlayer", choices = c("All", playerSet()$Player), selected="All")
    updateSelectInput(session, "gamelogPlayer", choices = playerSet()$Player)
    updateSelectInput(session, "yearlyPlayer", choices = playerSet()$Player)
    
  })
})