library(XML) # HTML processing
library(dplyr)

getYahooRankings <- function() {
  options(stringsAsFactors = FALSE)
  # Base URL
  base.url = 'http://fantasyfootballimpact.com/special-features/yahoo-default-list-movement/'
  
  yahoorankings <- as.data.frame(readHTMLTable(base.url)[1])
  colnames(yahoorankings) <- gsub("tablepress.98.", "", colnames(yahoorankings))
  yahoorankings$Pos <- gsub("WRv", "WR", yahoorankings$Pos)
  yahoorankings$Pos <- gsub("TgE", "TE", yahoorankings$Pos)
  for(i in 4:ncol(yahoorankings)) {
    yahoorankings[,i] <- as.numeric(yahoorankings[,i])
  }
  yahoorankings <- yahoorankings %>% filter(Pos != "DST")
  
  yahoorankings$Player <- unlist(lapply(strsplit(yahoorankings$Player, ", "), function(a) {
    paste0(a[2], " ", a[1])
  }))
  
  for(i in 4:ncol(yahoorankings)) {
    if(is.na(yahoorankings[1,i])) {
      yahoorankings <- yahoorankings[,c(1:(i-1))]
      break
    }
  }
  return(yahoorankings)
}

yahoorankings <- getYahooRankings() %>% select(Player, ncol(.))
colnames(yahoorankings) <- c("Player", "YRank")


w <- which(yahoorankings$Player == "Odell Beckham")
yahoorankings$Player[w] <- "Odell Beckham Jr."
w <- which(yahoorankings$Player == "TY Hilton")
yahoorankings$Player[w] <- "T.Y. Hilton"
w <- which(yahoorankings$Player == "DeVante Parker")
yahoorankings$Player[w] <- "Devante Parker"
w <- which(yahoorankings$Player == "Drew Bres")
yahoorankings$Player[w] <- "Drew Brees"
w <- which(yahoorankings$Player == "Rayn Tannehill")
yahoorankings$Player[w] <- "Ryan Tannehill"
w <- which(yahoorankings$Player == "Tyler Taylor")
yahoorankings$Player[w] <- "Tyrod Taylor"
w <- which(yahoorankings$Player == "Buck Allen")
yahoorankings$Player[w] <- "Javorius Allen"
