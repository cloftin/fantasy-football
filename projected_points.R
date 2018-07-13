

projected_points <- function(x, passyds = 50, passtds = 5, ints = -2, rushyds = 20, rushtds = 6,
                    recs = .5, recyds = 20, rectds = 6, twopts = 2, fumbles = -1,
                    numofqb = 17, numofrb = 39, numofwr = 48, numofte = 13) {
  
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
  # qbreplace = (qbs[numofqb+1,4] + qbs[numofqb-1,4])/2
  # rbreplace = (rbs[numofrb+1,4] + rbs[numofrb-1,4])/2
  # wrreplace = (wrs[numofwr+1,4] + wrs[numofwr-1,4])/2
  # tereplace = (tes[numofte+1,4] + tes[numofte-1,4])/2
  qbreplace <- qbs[numofqb + 1, 4]
  rbreplace <- rbs[numofrb + 1, 4]
  wrreplace <- wrs[numofwr + 1, 4]
  tereplace <- tes[numofte + 1, 4]
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