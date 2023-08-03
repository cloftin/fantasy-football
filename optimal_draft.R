
optimal_draft <- function(myteam = NULL, optimumPlayer = NULL, available, currentPick, myPick = 3, numTeams = 12) {
  
  myteam$Pos <- substr(myteam$Pos, 1, 2)

  ## Sort available players by Yahoo Rank
  available <- available[order(available$YRank),]
  available$Pos <- substr(available$Pos, 1, 2)
  
  ## Create vector of pick positions
  picks <- sort(c(seq(myPick, as.numeric(numTeams) * 16, by = as.numeric(numTeams) * 2), 
                  seq((as.numeric(numTeams)*2 + 1) - as.numeric(myPick), as.numeric(numTeams) * 16, by = as.numeric(numTeams) * 2)))
  picks <- picks[picks >= currentPick]
  
  ## Create empty team
  optteam <- data.frame(Player = NA, 
                        Position = c("QB", "RB", "RB", "WR", "WR", "WR", "TE", "FLEX"),
                        VOR = NA,
                        YRank = NA)
  
  
  ## Insert players already on my team
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
  
  ## Get positions left to fill
  remaining <- optteam %>% filter(is.na(Player))
  
  ## Create unique combinations by assigning positions left to fill to remaining picks
  perms <- unique(combinat::permn(remaining$Position))
  
  ## Transform to a data frame
  dperms <- data.frame(matrix(unlist(perms), nrow=length(perms), byrow=T))
  columns <- c("one", "two", "three", "four", "five", "six", "seven", "eight")
  
  ## Remove picks already made
  colnames(dperms) <- columns[c(((8 - nrow(remaining)) + 1):8)]
  
  ## Don't fill flex position in 1st 2 rounds
  if("one" %in% colnames(dperms)) {
    dperms <- dperms %>% filter(one != "FLEX")
  }
  if("two" %in% colnames(dperms)) {
    dperms <- dperms %>% filter(two != "FLEX")
  }
  
  
  # if(!is.null(myteam)) {
  #   if(nrow(myteam) > 0) {
  #     for(i in 1:nrow(myteam)) {
  #       dperms <- dperms %>% filter(.[,i] == myteam$Pos[i])
  #     }
  #   }
  # }
  
  ## Create list of data frames, each df contains players who will be available at given pick,
  ## based on Yahoo rank
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
  
  
  ## Loop through each position/pick combination, assign best player at specified position who will be
  ## available at the specified pick. ~2.5k combinations for 8 rounds, will take considerable time
  ## FUTURE: try optimizing further (either faster code or reducing # of combos)
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
  
  ## If optimum player is chosen, restricts possible combinations to those that include given player
  if(!is.null(optimumPlayer) & optimumPlayer != "All") {
    t <- unlist(lapply(optimum, function(x) {optimumPlayer %in% x$Player}))
    optimum <- optimum[t]
  }
  
  ## Calculate total VOR per team
  totalvor <- lapply(optimum, function(x) {
    return(sum(x$VOR))
  })
  
  ## Find highest total VOR team and return
  optimum <- optimum[[which.max(totalvor)]]
  
  return(optimum)
  
}