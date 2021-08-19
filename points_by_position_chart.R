points_by_position_chart <- function(position, dat, numofteams, numofpos = 0) {
  
  if(position != "All") {
    dat <- dat %>% filter(substr(Pos, 1, 2) == position, PosRank <= 50)
    dat <- dat[order(dat$PosRank),]
  } else {
    dat <- dat %>% filter(PosRank <= 25)
  }
  
  if(position == "All") {
    g <- ggplot(data = dat, aes(x = PosRank, y = Points, colour = factor(substr(Pos, 1, 2)), label = Player)) + 
      geom_point() + theme_bw() +
      ggtitle(position) + scale_colour_discrete(name = "Position") + theme(legend.position = "top")
  } else {
    g <- ggplot(data = dat, aes(x = PosRank, y = Points, color = cluster, label = Player)) + geom_point() + theme_bw() +
      ggtitle(position) + theme(legend.position = "none")
    toeval <- "g <- g"
    for(i in 1:numofpos) {
      toeval <- paste0(toeval, " + geom_vline(aes(xintercept = ", (numofteams * i), "), color = '#01A65A')")
    }
    
    eval(parse(text = toeval))
  }
  # if(numofpos == 2) {
  #   g <- g + geom_vline(aes(xintercept = numofteams * numofpos), color = "#01A65A")
  # } else if(numofpos == 36) {
  #   g <- g + geom_vline(aes(xintercept = 24), color = "#01A65A") + geom_vline(aes(xintercept = 36), color = "#01A65A")
  # }
  
  plotly::ggplotly(g, tooltip = c("Player", "PosRank", "Points"))
  
}