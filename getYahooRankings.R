library(XML) # HTML processing
library(dplyr)

getYahooRankings <- function() {
  options(stringsAsFactors = FALSE)
  # Base URL
  # base.url = 'http://fantasyfootballimpact.com/special-features/yahoo-default-list-movement/'
  # alt.url = "http://webcache.googleusercontent.com/search?q=cache:pZM5tJu8yQoJ:fantasyfootballimpact.com/special-features/yahoo-default-list-movement/+&cd=1&hl=en&ct=clnk&gl=us"
  base.url = "https://partners.fantasypros.com/external/widget/nfl-staff-rankings.php?source=2&id=7:8:9:285:699&year=2018&week=0&position=ALL&scoring=HALF&ajax=true&width=640&export=xls"
  
  # if(!useAltURL) {
  yahoorankings <- readLines(base.url)
  # }
  # if(nrow(yahoorankings) == 0 || !exists("yahoorankings")) {
  #   assign("useAltURL", T, envir = .GlobalEnv)
  #   yahoorankings <- as.data.frame(readHTMLTable(alt.url)[1])
  # }
  yahoorankings <- yahoorankings[c(5:length(yahoorankings))]
  yahoorankings <- plyr::ldply(yahoorankings, function(x) {
    unlist(strsplit(x, "\\t"))[c(1:4)]
  })
  colnames(yahoorankings) <- c("Rank", "Player", "PosRank", "Team")
  yahoorankings$PosRank <- gsub("RB",  "RB_",   yahoorankings$PosRank)
  yahoorankings$PosRank <- gsub("WR",  "WR_",   yahoorankings$PosRank)
  yahoorankings$PosRank <- gsub("QB",  "QB_",   yahoorankings$PosRank)
  yahoorankings$PosRank <- gsub("TE",  "TE_",   yahoorankings$PosRank)
  yahoorankings$PosRank <- gsub("K",   "K_",    yahoorankings$PosRank)
  yahoorankings$PosRank <- gsub("DST", "DST_", yahoorankings$PosRank)
  yahoorankings$Pos <- unlist(lapply(yahoorankings$PosRank, function(x) {
    strsplit(x, "_")[[1]][1]
  }))
  yahoorankings$PosRank <- as.numeric(unlist(lapply(yahoorankings$PosRank, function(x) {
    strsplit(x, "_")[[1]][2]
  })))

  yahoorankings <- yahoorankings %>% select(Rank, Player, Pos, PosRank, Team)
  yahoorankings <- yahoorankings %>% filter(Pos != "DST")
  
  return(yahoorankings)
}

yahoorankings <- getYahooRankings() %>% select(Player, Rank)
colnames(yahoorankings) <- c("Player", "YRank")
