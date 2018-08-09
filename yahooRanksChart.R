# source("scraping.R")
rankingsChart <- function(playerName) {
  
  yahooRanks <- FantasyFootballData::get_yahoo_rankings(update = FALSE)
  
  weekDates <- data.frame(Week = c(1:(ncol(yahooRanks) - 2)), Date = as.Date(paste0(colnames(yahooRanks)[c(3:ncol(yahooRanks))], "/2018"), format = "%m/%d/%Y"))
  
  colnames(yahooRanks) <- c(colnames(yahooRanks)[c(1:2)], c(1:(ncol(yahooRanks)-2)))
  
  yahooRanks <- gather(yahooRanks, Week, YRank, 3:ncol(yahooRanks))
  yahooRanks <- merge(yahooRanks, weekDates)
  yahooRanks$Week <- NULL

  yahooRanks$Team <- gsub("ARZ", "ARI", yahooRanks$Team)
  minRanking <- yahooRanks %>% filter(Player == playerName)
  maxRanking <- max(minRanking$YRank)
  minRanking <- min(minRanking$YRank)
  
  maxLimit <- maxRanking + 5
  minLimit <- minRanking - 5
  
  if(maxLimit > 200) {
    maxLimit <- 200
  }
  
  if(minLimit < 1) {
    minLimit <- 1
  }
  
  logos <- read.csv(file = "logos.csv", header = T, stringsAsFactors = F)
  colors <- logos %>% select(Team, Primary, Secondary)
  breaks <- colors$Team
  values <- colors$Primary
  yahooRanks <- merge(yahooRanks, colors, by = "Team")
  
  eval(parse(text = paste0("g <- ggplot() + geom_line(data = yahooRanks %>% filter(Player != playerName), aes(x = Date, y = YRank, group = Player), colour = 'grey') +
    geom_line(data = yahooRanks %>% filter(Player == playerName), aes(x = Date, y = YRank), colour = '", unique(yahooRanks %>% filter(Player == playerName) %>% select(Primary))[1],
                           "', size = 1) + 
    geom_point(data = yahooRanks %>% filter(Player == playerName), aes(x = Date, y = YRank), colour = '", unique(yahooRanks %>% filter(Player == playerName) %>% select(Secondary))[1],
                           "', size = 3) +
    scale_y_reverse(lim = c(200,0)) +
    coord_cartesian(ylim = c(maxLimit, minLimit)) +
    ylab('Ranking\n') + theme_bw() + theme(legend.position = 'none')")))
  
  
  g
}

