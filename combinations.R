# dk <- read.csv("/Users/colin/Desktop/DKSalaries.csv")
picks <- c(7, 18, 31, 42, 55, 66, 79, 90, 103, 114, 127)
combinations <- function(draftdata, currentRound, picks, qbsToChoose, rbsToChoose, wrsToChoose, tesToChoose, flexToChoose) {
  
  draftdata$YRank <- draftdata$Rank + draftdata$VOY
  dk <- subset(draftdata, VOR > 0)
  dk <- split(dk, dk$Pos)
#   dk <- dk[-1]                         # remove the other position
  dk <- dk[c("QB", "WR", "RB", "TE")]  # reordering, for no reason really
  dk$FLEX <- subset(draftdata, (Pos == "WR" | Pos == "RB" | Pos == "TE") & VOR > 0)
  dk[[5]]$PosRank <- c(1:nrow(dk[[5]]))
  dk$FLEX <- subset(dk[[5]], PosRank > 24)
  for(i in 1:5) {
    dk[[i]] <- dk[[i]][c(1:10),]
  }
  
  ## Expected number of combinations
  ## #QBs * choose(#WR, 3) * choose(#RB, 2) * #TE
  choose(nrow(dk[[1]]), 1)*choose(nrow(dk[[2]]),3)*choose(nrow(dk[[3]]), 2)*choose(nrow(dk[[4]]), 1)
                                                                                                                   
  # 384
  
  ## Get indices of combos within each group
  rows <- list(combn(nrow(dk[[1]]), 1), 
               combn(nrow(dk[[2]]), 3), 
               combn(nrow(dk[[3]]), 2), 
               combn(nrow(dk[[4]]), 1))  # these are possible combinations of each position
  
  dims <- sapply(rows, NCOL)
  inds <- expand.grid(mapply(`:`, 1, dims))             # indicies of combinations in 'rows'
  dim(inds)
  # [1] 384   4
  
  ## Function to extract a group
  extract <- function(ind) {
    g <- inds[ind,]
    do.call(rbind, lapply(1:4, function(i) dk[[i]][rows[[i]][,g[[i]]], ]))
  }
  
}