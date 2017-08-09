library(XML)
library(plyr)
library(dplyr)
source("scraping.R")

projpts <- function(x,passyds, passtds, ints, rushyds, rushtds, recs, recyds, rectds, twopts, fumbles, numofqb, numofrb, numofwr, numofte) {
  x <- as.data.frame(x)
  toreturn <- as.data.frame(matrix(ncol=4,nrow=nrow(x)))
  toreturn[,1] <- x[,1]
  toreturn[,2] <- x$Pos
  toreturn[,3] <- x$Team
  toreturn[,4] <- (x$PassYds/passyds) + (x$PassTDs*passtds) + (x$PassInts*ints) + (x$RushYds/rushyds) + (x$RushTDs*rushtds) + (x$Receptions*recs) + (x$RecYds/recyds) + (x$RecTDs*rectds) + (x$TwoPts*2) + (x$Fumbles*fumbles)
  qbs <- subset(toreturn, toreturn[,2]=="QB")
  qbs <- qbs[order(-qbs[,4]),]
  rbs <- subset(toreturn, toreturn[,2]=="RB")
  rbs <- rbs[order(-rbs[,4]),]
  wrs <- subset(toreturn, toreturn[,2]=="WR")
  wrs <- wrs[order(-wrs[,4]),]
  tes <- subset(toreturn, toreturn[,2]=="TE")
  tes <- tes[order(-tes[,4]),]
  qbreplace = (qbs[numofqb+1,4] + qbs[numofqb-1,4])/2
  rbreplace = (rbs[numofrb+1,4] + rbs[numofrb-1,4])/2
  wrreplace = (wrs[numofwr+1,4] + wrs[numofwr-1,4])/2
  tereplace = (tes[numofte+1,4] + tes[numofte-1,4])/2
  qbs[,5] <- qbs[,4]-qbreplace
  qbs <- qbs[order(-qbs[,4]),]
  qbs[,6] <- c(1:nrow(qbs))
  rbs[,5] <- rbs[,4]-rbreplace
  rbs <- rbs[order(-rbs[,4]),]
  rbs[,6] <- c(1:nrow(rbs))
  wrs[,5] <- wrs[,4]-wrreplace
  wrs <- wrs[order(-wrs[,4]),]
  wrs[,6] <- c(1:nrow(wrs))
  tes[,5] <- tes[,4]-tereplace
  tes <- tes[order(-tes[,4]),]
  tes[,6] <- c(1:nrow(tes))
  toreturn <- NULL
  toreturn <- rbind(qbs,rbs,wrs,tes)
  toreturn <- toreturn[order(-toreturn[,5]),]
  toreturn[,7] <- toreturn[,6]
  toreturn[,6] <- c(1:nrow(toreturn))
  
  toreturn[,1] <- gsub(" $","", toreturn[,1], perl=T)
  colnames(toreturn) = c("Player","Pos","Team","Points","VOR","Rank","PosRank")
  temp <- toreturn
  toreturn <- NULL
  toreturn <- merge(temp,yahoorankings)
  toreturn <- toreturn[order(-toreturn[,5]),]
  toreturn$YRank[which(is.na(toreturn$YRank))] <- 0
  toreturn$VOY <- toreturn$YRank - toreturn$Rank
  toreturn$VOR <- round(toreturn$VOR, 2)
  # toreturn$Team <- NULL
  return(toreturn)
}


lines <- readLines("https://www.fantasypros.com/nfl/projections/qb.php?week=draft")
lines <- lines[c((grep(" <tbody>", lines)+1):(grep(" </tbody>", lines)-1))]

qb_fp <- list()
for(i in 1:(length(lines)/12)) {
  qb_fp[[i]] <- lines[c((12*(i-1)+1):(12*i))]
}

qb_fp <- ldply(qb_fp, function(x) {
  team <- strsplit(strsplit(strsplit(x[1], split = "player-name\\\">")[[1]][2], "</a> ")[[1]][2], " ")[[1]][1]
  x[1] <- strsplit(strsplit(x[1], split = "player-name\\\">")[[1]][2], "</a>")[[1]][1]
  x <- gsub("<td class=\\\"center\\\">|</td>", "", x[c(1:11)])
  x <- as.data.frame(rbind(x))
  x <- x %>% select(V1, V4, V5, V6, V8, V9, V10)
  for(i in 2:ncol(x)) {
    x[,i] <- as.numeric(gsub(",", "", x[,i]))
  }
  x$Team <- team
  return(x)
})
colnames(qb_fp) <- c("Player", "PassYds", "PassTDs", "PassInts", "RushYds", "RushTDs", "Fumbles", "Team")
qb_fp$Pos <- "QB"
qb_fp <- merge(qb_fp, yahoorankings, all.x = T)

lines <- readLines("https://www.fantasypros.com/nfl/projections/rb.php?week=draft")
lines <- lines[c((grep(" <tbody>", lines)+1):(grep(" </tbody>", lines)-1))]

rb_fp <- list()
for(i in 1:(length(lines)/10)) {
  rb_fp[[i]] <- lines[c((10*(i-1)+1):(10*i))]
}

rb_fp <- ldply(rb_fp, function(x) {
  team <- strsplit(strsplit(strsplit(x[1], split = "player-name\\\">")[[1]][2], "</a> ")[[1]][2], " ")[[1]][1]
  x[1] <- strsplit(strsplit(x[1], split = "player-name\\\">")[[1]][2], "</a>")[[1]][1]
  x <- gsub("<td class=\\\"center\\\">|</td>", "", x[c(1:9)])
  x <- as.data.frame(rbind(x))
  x <- x %>% select(V1, V3, V4, V5, V6, V7, V8)
  for(i in 2:ncol(x)) {
    x[,i] <- as.numeric(gsub(",", "", x[,i]))
  }
  x$Team <- team
  return(x)
})
colnames(rb_fp) <- c("Player", "RushYds", "RushTDs", "Receptions", "RecYds", "RecTDs", "Fumbles", "Team")
rb_fp$Pos <- "RB"
rb_fp <- merge(rb_fp, yahoorankings, all.x = T)

lines <- readLines("https://www.fantasypros.com/nfl/projections/wr.php?week=draft")
lines <- lines[c((grep(" <tbody>", lines)+1):(grep(" </tbody>", lines)-1))]

wr_fp <- list()
for(i in 1:(length(lines)/10)) {
  wr_fp[[i]] <- lines[c((10*(i-1)+1):(10*i))]
}

wr_fp <- ldply(wr_fp, function(x) {
  team <- strsplit(strsplit(strsplit(x[1], split = "player-name\\\">")[[1]][2], "</a> ")[[1]][2], " ")[[1]][1]
  x[1] <- strsplit(strsplit(x[1], split = "player-name\\\">")[[1]][2], "</a>")[[1]][1]
  x <- gsub("<td class=\\\"center\\\">|</td>", "", x[c(1:9)])
  x <- as.data.frame(rbind(x))
  x <- x %>% select(V1, V3, V4, V5, V6, V7, V8)
  for(i in 2:ncol(x)) {
    x[,i] <- as.numeric(gsub(",", "", x[,i]))
  }
  x$Team <- team
  return(x)
})
colnames(wr_fp) <- c("Player", "RushYds", "RushTDs", "Receptions", "RecYds", "RecTDs", "Fumbles", "Team")
wr_fp$Pos <- "WR"
wr_fp <- merge(wr_fp, yahoorankings, all.x = T)

lines <- readLines("https://www.fantasypros.com/nfl/projections/te.php?week=draft")
lines <- lines[c((grep(" <tbody>", lines)+1):(grep(" </tbody>", lines)-1))]

te_fp <- list()
for(i in 1:(length(lines)/7)) {
  te_fp[[i]] <- lines[c((7*(i-1)+1):(7*i))]
}

te_fp <- ldply(te_fp, function(x) {
  team <- strsplit(strsplit(strsplit(x[1], split = "player-name\\\">")[[1]][2], "</a> ")[[1]][2], " ")[[1]][1]
  x[1] <- strsplit(strsplit(x[1], split = "player-name\\\">")[[1]][2], "</a>")[[1]][1]
  x <- gsub("<td class=\\\"center\\\">|</td>", "", x[c(1:6)])
  x <- as.data.frame(rbind(x))
  x <- x %>% select(V1, V2, V3, V4, V5)
  for(i in 2:ncol(x)) {
    x[,i] <- as.numeric(gsub(",", "", x[,i]))
  }
  x$Team <- team
  return(x)
})
colnames(te_fp) <- c("Player", "Receptions", "RecYds", "RecTDs", "Fumbles", "Team")
te_fp$Pos <- "TE"
te_fp <- te_fp[-which(te_fp$Player == "David Johnson"),]
te_fp <- merge(te_fp, yahoorankings, all.x = T)

big <- as.data.frame(matrix(ncol=14, nrow=1))
colnames(big) = c("Player","Pos","PassYds","PassTDs","PassInts","RushYds","RushTDs","Receptions","RecYds","RecTDs","TwoPts","Fumbles","YahooRank","YahooPosRank")

projections <- merge(big,qb_fp,all.y=T)
projections <- rbind(projections, merge(big,rb_fp,all.y=T))
projections <- rbind(projections, merge(big,wr_fp,all.y=T))
projections <- rbind(projections, merge(big,te_fp,all.y=T))
projections[is.na(projections)] <- 0

passyds=50
passtds=5
ints=-2
rushyds=20
rushtds=6
recs=.5
recyds=20
rectds=6
twopts=2
fumbles=-1
numofqb=17
numofrb=39
numofwr=48
numofte=13
draftdata <- projpts(projections,50,5,-2,20,6,.5,20,6,2,-1,17,39,48,13)
draftdata$Player <- gsub(" $","", draftdata$Player, perl=T)

draftdata <- draftdata[order(-draftdata$Rank),]

