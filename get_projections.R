
get_projections <- function() {
  
  lines <- readLines("https://www.fantasypros.com/nfl/projections/qb.php?week=draft")
  lines <- lines[c((grep(" <tbody>", lines)[1]+1):(grep(" </tbody>", lines)[1]-1))]
  
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
  lines <- lines[c((grep(" <tbody>", lines)[1]+1):(grep(" </tbody>", lines)[1]-1))]
  
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
  lines <- lines[c((grep(" <tbody>", lines)[1]+1):(grep(" </tbody>", lines)[1]-1))]
  
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
  lines <- lines[c((grep(" <tbody>", lines)[1]+1):(grep(" </tbody>", lines)[1]-1))]
  
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

  draftdata <- projected_points(projections)
  draftdata$Player <- gsub(" $","", draftdata$Player, perl=T)
  
  draftdata <- draftdata[order(-draftdata$Rank),]
  return(draftdata)
}