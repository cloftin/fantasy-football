library(dplyr)

source("projpts.R")

espn_projections <- function() {
  base <- "http://games.espn.com/ffl/tools/projections?&startIndex="
  dat <- data.frame()
  for(i in 0:10) {
    url <- paste0(base, 40*i)
    t <- XML::readHTMLTable(url)$playertable_0
    colnames(t) <- t[1,]
    t <- t[-1,]
    t$Player <- unlist(lapply(strsplit(t$`PLAYER, TEAM POS`, ", "), function(x) {head(x,1)}))
    t$`PLAYER, TEAM POS` <- gsub("[^[:alnum:] ]", ",", unlist(lapply(strsplit(t$`PLAYER, TEAM POS`, ", "), function(x) {tail(x,1)})))
    t$Team <- toupper(unlist(lapply(strsplit(t$`PLAYER, TEAM POS`, ","), function(x) {head(x,1)})))
    t$Pos <- toupper(unlist(lapply(strsplit(t$`PLAYER, TEAM POS`, ","), function(x) {x[2]})))
    t$RNK <- NULL
    t$`PLAYER, TEAM POS` <- NULL
    t$`C/A` <- NULL
    colnames(t) <- c("PassYds", "PassTDs", "PassInts", "Rushes", "RushYds", "RushTDs", "Receptions", "RecYds", "RecTDs", "PTS", "Player", "Team", "Pos")
    t <- t %>% select(Player, Team, Pos, PassYds, PassTDs, PassInts, RushYds, RushTDs, Receptions, RecYds, RecTDs)
    dat <- rbind(dat, t)
  }
  
  for(i in 4:ncol(dat)) {
    dat[,i] <- as.numeric(dat[,i])
  }
  dat$TwoPts <- 0
  dat$Fumbles <- 0
  
  projpts(dat, 50, 5, -2, 20, 6, .5, 20, 6, 2, -1, 17, 39, 48, 13)
  return(dat)
  
}
