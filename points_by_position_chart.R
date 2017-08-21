points_by_position_chart <- function(position, dat, numofpos) {
  
  dat <- dat %>% filter(substr(Pos, 1, 2) == position, PosRank <= 50)
  dat <- dat[order(dat$PosRank),]
  
  ints <- if(numofpos == 12) {
    12
  } else if(numofpos == 24) {
    c(12, 24)
  } else if(numofpos == 36) {
    c(12, 24, 36)
  }
  
  g <- ggplot(data = dat, aes(x = PosRank, y = Points)) + geom_path() + geom_point() + theme_bw() +
    ggtitle(position) + geom_vline(aes(xintercept = 12), color = "#01A65A")
  
  if(numofpos == 24) {
    g <- g + geom_vline(aes(xintercept = 24), color = "#01A65A")
  } else if(numofpos == 36) {
    g <- g + geom_vline(aes(xintercept = 24), color = "#01A65A") + geom_vline(aes(xintercept = 36), color = "#01A65A")
  }
  
  g
  
}