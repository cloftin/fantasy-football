library(shiny)
library(tidyr)
library(ggplot2)
library(DT)

source("projections.R")
source("yahooRanksChart.R")
source("getRound.R")

drafted <- data.frame()
logos <- read.csv(file = "logos.csv", header = T, stringsAsFactors = F)
# useAltURL <- F

shinyServer(function(input, output, clientData, session) {
  
  drafted <- data.frame()
  
  my_team <- data.frame()
  draftdata <- projections
  observe({
    draftdata <- projpts(projections,input$passyds, input$passtds, input$ints, input$rushyds, input$rushtds, input$recs, input$recyds, input$rectds, input$twopts, input$fumbles, input$numofqb, input$numofrb, input$numofwr, input$numofte)
    draftdata$Pos <- paste(draftdata$Pos,"(",draftdata$PosRank,")", sep="")
    draftdata$PosRank <- NULL
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
    
    input$draft
    isolate({
      drafted <<- rbind(drafted, subset(draftdata, draftdata$Player == input$player))
    })
    
    updateSelectInput(session, "player", choices = c("All", playerSet()$Player), selected="All")
    
  })
})