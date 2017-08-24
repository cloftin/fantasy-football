get_team_players <- function(dat, teamToView, numOfTeams, numofqb, numofrb, numofwr, numofte, numoffl) {
  
  picks <- sort(c(seq(as.numeric(teamToView), as.numeric(numOfTeams) * 16, by = as.numeric(numOfTeams) * 2), 
                  seq((as.numeric(numOfTeams)*2 + 1) - as.numeric(teamToView), as.numeric(numOfTeams) * 16, by = as.numeric(numOfTeams) * 2)))
  dat <- dat[picks[which(picks <= nrow(dat))],]
  dat$Pos <- substr(dat$Pos, 1, 2)
  qbs <- subset(dat, Pos=="QB")
  rbs <- subset(dat, Pos=="RB")
  wrs <- subset(dat, Pos=="WR")
  tes <- subset(dat, Pos=="TE")
  starters <- data.frame()
  bench <- data.frame()
  
  if(nrow(qbs) != 0) {
    starters <- rbind(starters, qbs[c(1:numofqb),])
    if(nrow(qbs) > numofqb) {
      bench <- rbind(bench, qbs[c((numofqb + 1):nrow(qbs)),])
    }
  }
  
  if(nrow(rbs) != 0) {
    starters <- rbind(starters, rbs[c(1:numofrb),])
    if(nrow(rbs) > numofrb) {
      bench <- rbind(bench, rbs[c((numofrb + 1):nrow(rbs)),])
    }
  }
  
  if(nrow(wrs) != 0) {
    starters <- rbind(starters, wrs[c(1:numofwr),])
    if(nrow(wrs) > numofwr) {
      bench <- rbind(bench, wrs[c((numofwr + 1):nrow(wrs)),])
    }
  }
  
  if(nrow(tes) != 0) {
    starters <- rbind(starters, tes[c(1:numofte),])
    if(nrow(tes) > numofte) {
      bench <- rbind(bench, tes[c((numofte + 1):nrow(tes)),])
    }
  }
  
  if(nrow(bench) > 0) {
    flex <- subset(bench, bench$Pos != "QB")
    flex <- flex[c(1:numoffl),]
    flex$Pos <- "W/R/T"
    bench <- bench[-(bench$Player %in% flex$Player),]
    starters <- rbind(starters, flex)
  }
  
  output <- data.frame()
  if(nrow(starters) > 0) {
    output <- rbind(starters, c("---", "---", "---", "---", "---"), bench)
    output <- output %>% select(Pos, Player, Team, Points, VOR)
  }
  
  if(nrow(output) > 0) {
    output <- output[complete.cases(output$Player),]
  }
  
  startervor <- sum(starters$VOR, na.rm = T)
  
  return(list(output, startervor))
  
}