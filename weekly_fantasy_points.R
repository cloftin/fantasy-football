weekly_fantasy_points <- function(week) {
  pass_yds <- .04
  pass_tds <- 5
  ints <- -2
  rush_yds <- .1
  rush_tds <- 6
  recs <- .5
  rec_yds <- .1
  rec_tds <- 6
  two_pts <- 2
  fumbles <- -2
  ret_yds <- .05
  ret_tds <- 6
  pass_bonus <- data.frame(condition = c(300, 350, 400), pts = c(1,2,3))
  rush_bonus <- data.frame(condition = c(100, 150, 200), pts = c(1,2,3))
  rec_bonus  <- data.frame(condition = c(100, 150, 200), pts = c(1,2,3))
  
  pts <- (week$rush_yds * rush_yds) + (week$rush_td * rush_tds) + (week$rec * recs) +
    (week$rec_yds * rec_yds) + (week$rec_td * rec_tds) + (week$two_pt_md * two_pts) +
    (week$pass_yds * pass_yds) + (week$pass_td * pass_tds) + (week$pass_int * ints) +
    (week$kick_ret_yds * ret_yds) + (week$kick_ret_td * ret_tds) + (week$punt_ret_yds * ret_yds) +
    (week$punt_ret_td * ret_tds)
  
  if(length(which(week$pass_yds >= pass_bonus$condition)) > 0) {
    pts <- pts + pass_bonus$pts[max(which(week$pass_yds >= pass_bonus$condition))]
  }
  if(length(which(week$rush_yds >= rush_bonus$condition)) > 0) {
    pts <- pts + rush_bonus$pts[max(which(week$rush_yds >= rush_bonus$condition))]
  }
  if(length(which(week$rec_yds >= rec_bonus$condition)) > 0) {
    pts <- pts + rec_bonus$pts[max(which(week$rec_yds >= rec_bonus$condition))]
  }
  return(pts)
  
}