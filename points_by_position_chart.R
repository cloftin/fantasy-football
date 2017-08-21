points_by_position_chart <- function(position, dat) {
  print(head(dat))
  dat <- dat %>% filter(substr(Pos, 1, 2) == position, PosRank <= 50)
  print(head(dat))
  dat <- dat[order(dat$PosRank),]
  
  ggplot(data = dat, aes(x = PosRank, y = Points)) + geom_path() + geom_point() + theme_bw() +
    ggtitle(position)
  
}