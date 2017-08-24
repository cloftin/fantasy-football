get_team_players <- function(dat, teamToView, numofTeams) {
  
  picks <- sort(c(seq(as.numeric(teamToView), as.numeric(numofTeams) * 16, by = as.numeric(numofTeams) * 2), 
                  seq((as.numeric(numofTeams)*2 + 1) - as.numeric(teamToView), as.numeric(numofTeams) * 16, by = as.numeric(numofTeams) * 2)))
  dat <- dat[picks[which(picks <= nrow(dat))],]
  dat$Pos <- substr(dat$Pos, 1, 2)
  qbs <- subset(dat, Pos=="QB")
  rbs <- subset(dat, Pos=="RB")
  wrs <- subset(dat, Pos=="WR")
  tes <- subset(dat, Pos=="TE")
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
  
  output <- data.frame()
  if(nrow(starters) > 0) {
    output <- rbind(starters, c("---", "---", "---", "---", "---"), bench)
    output <- output %>% select(Pos, Player, Team, Points, VOR)
  }
  
  return(output)
  
}