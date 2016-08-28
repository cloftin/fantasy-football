source("scraping.R")
rankingsChart <- function(playerName) {
  
  yahooRanks <- getYahooRankings()
  weekDates <- data.frame(Week = c(1:(ncol(yahooRanks) - 3)), Date = as.Date(paste0(colnames(yahooRanks)[c(4:ncol(yahooRanks))], ".2016"), format = "%m.%d.%Y"))
  colnames(yahooRanks) <- c(colnames(yahooRanks)[c(1:3)], c(1:(ncol(yahooRanks)-3)))
  
  yahooRanks <- gather(yahooRanks, Week, YRank, 4:ncol(yahooRanks))
  yahooRanks <- merge(yahooRanks, weekDates)
  yahooRanks$Week <- NULL

  minRanking <- yahooRanks %>% filter(Player == playerName)
  maxRanking <- max(minRanking$YRank)
  minRanking <- min(minRanking$YRank)
  
  ggplot() + geom_line(data = yahooRanks %>% filter(Player != playerName), aes(x = Date, y = YRank, group = Player), colour = "grey") +
    geom_line(data = yahooRanks %>% filter(Player == playerName), aes(x = Date, y = YRank, colour = "red"), size = 1) + 
    scale_y_reverse(lim = c(200,1)) +
    ylab("Ranking\n") + theme_bw() + theme(legend.position = "none") 
}

