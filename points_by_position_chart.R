points_by_position_chart <- function(position, dat, numofteams, numofpos) {
  
  dat <- dat %>% filter(substr(Pos, 1, 2) == position, PosRank <= 50)
  dat <- dat[order(dat$PosRank),]

  g <- ggplot(data = dat, aes(x = PosRank, y = Points)) + geom_path() + geom_point() + theme_bw() +
    ggtitle(position)
  
  toeval <- "g <- g"
  for(i in 1:numofpos) {
    toeval <- paste0(toeval, " + geom_vline(aes(xintercept = ", (numofteams * i), "), color = '#01A65A')")
  }
  
  eval(parse(text = toeval))
  # if(numofpos == 2) {
  #   g <- g + geom_vline(aes(xintercept = numofteams * numofpos), color = "#01A65A")
  # } else if(numofpos == 36) {
  #   g <- g + geom_vline(aes(xintercept = 24), color = "#01A65A") + geom_vline(aes(xintercept = 36), color = "#01A65A")
  # }
  
  g
  
}