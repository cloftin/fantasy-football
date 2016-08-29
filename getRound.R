getRound <- function(picks, numOfTeams) {
  
  return(((picks-1) %/% numOfTeams)+1)
  
}