
optimal_draft <- function(myteam = NULL, available, currentPick, myPick = 3, numTeams = 12, removeKeepers = T) {
  
  myteam$Pos <- substr(myteam$Pos, 1, 2)
  
  if(removeKeepers) {
    available <- available %>% filter(!(Player %in% c("LeSean McCoy", "Cooper Kupp", "Sammy Watkins",
                                                      "DeVante Parker")))
  }
  available <- available[order(available$YRank),]
  available$Pos <- substr(available$Pos, 1, 2)
  
  picks <- sort(c(seq(myPick, as.numeric(numTeams) * 16, by = as.numeric(numTeams) * 2), 
                  seq((as.numeric(numTeams)*2 + 1) - as.numeric(myPick), as.numeric(numTeams) * 16, by = as.numeric(numTeams) * 2)))
  picks <- picks[picks >= currentPick]
  
  optteam <- data.frame(Player = NA, 
                        Position = c("QB", "RB", "RB", "WR", "WR", "WR", "TE", "FLEX"),
                        VOR = NA,
                        YRank = NA)
  
  if(!is.null(myteam)) {
    rbflex <- myteam[0,]
    wrflex <- myteam[0,]
    poscounts <- plyr::count(myteam$Pos)
    if("RB" %in% poscounts$x) {
      if(poscounts %>% filter(x == "RB") %>% .$freq > 2) {
        rbflex = myteam %>% filter(Pos == "RB") %>% .[3,]
      }
    }
    if("WR" %in% poscounts$x) {
      if(poscounts %>% filter(x == "WR") %>% .$freq > 3) {
        wrflex = myteam %>% filter(Pos == "WR") %>% .[4,]
      }
    }
    
    flex <- rbind(rbflex, wrflex) %>% .[order(-.$VOR),] %>% .[1,]
    myteam$Pos[which(myteam$Player == flex$Player[1])] <- "FLEX"
    
    for(i in 1:nrow(myteam)) {
      position <- myteam$Pos[i]
      w <- which(optteam$Position == position & is.na(optteam$Player))
      if(length(w) > 0) {
        optteam$Player[w[1]] <- myteam$Player[i]
        optteam$VOR[w[1]] <- myteam$VOR[i]
        optteam$YRank[w[1]] <- myteam$YRank[i]
      }
    }
  }
  
  remaining <- optteam %>% filter(is.na(Player))
  perms <- unique(combinat::permn(remaining$Position))
  dperms <- data.frame(matrix(unlist(perms), nrow=length(perms), byrow=T))
  columns <- c("one", "two", "three", "four", "five", "six", "seven", "eight")
  colnames(dperms) <- columns[c(((8 - nrow(remaining)) + 1):8)]
  if("one" %in% colnames(dperms)) {
    dperms <- dperms %>% filter(one %in% c("RB", "WR") & !(one == "FLEX"))
  }
  if("two" %in% colnames(dperms)) {
    dperms <- dperms %>% filter(two %in% c("RB", "WR") & !(two == "FLEX"))
  }
  
  
  # if(!is.null(myteam)) {
  #   if(nrow(myteam) > 0) {
  #     for(i in 1:nrow(myteam)) {
  #       dperms <- dperms %>% filter(.[,i] == myteam$Pos[i])
  #     }
  #   }
  # }
  
  availablePerPick <- list()
  for(i in 1:nrow(remaining)) {
    untilNextPick <- picks[i] - currentPick
    availablePerPick[[i]] <- if(untilNextPick > 0) {
      available[-c(1:untilNextPick),]
    } else {
      available
    }
    availablePerPick[[i]] <- availablePerPick[[i]][order(-availablePerPick[[i]]$VOR),]
  }
  
  cat(system.time(optimum <- apply(dperms, 1, function(x, availablePerPick) {
    
    t <- data.frame()
    for(i in 1:length(availablePerPick)) {
      pos <- x[i][1]
      a <- if(pos != "FLEX") {
        availablePerPick[[i]] %>% filter(Pos == pos) %>% .[1,] %>%
          select(Player, Pos, VOR, YRank)
      } else {
        availablePerPick[[i]] %>% filter(Pos %in% c("RB", "WR")) %>% .[1,] %>%
          select(Player, Pos, VOR, YRank)
      }
      t <- rbind(t, a)
      
      player <- a$Player
      availablePerPick <- lapply(availablePerPick, function(x, chosen) {
        x %>% filter(Player != chosen)
      }, chosen = player)
      
    }
    return(t)
    
  }, availablePerPick = availablePerPick)))
  
  totalvor <- lapply(optimum, function(x) {
    return(sum(x$VOR))
  })
  
  optimum <- optimum[[which.max(totalvor)]]
  
  return(optimum)
  
}