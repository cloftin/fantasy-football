library(data.table)
library(XML)
library(httr)
library(dplyr)
library(stringi)

get_yahoo_adp <- function() {
  
  adp <- data.frame()
  base = "https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=SD&pos=ALL&sort=DA_AP&count="
  for(i in 0:3) {
    tabs <- httr::GET(paste0(base, (i*50)))
    dat <- data.frame(readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)[2]$'draftanalysistable')
    
    colnames(dat)[1] <- "Offense"
    
    players <- lapply(dat$Offense, function(x) {stri_trim(strsplit(x, "\\n")[[1]][2])}) %>%
      lapply(., function(x) {strsplit(x, " - ")[[1]][1]}) %>%
      lapply(., function(x) {strsplit(x, " ")})
    
    players <- unlist(lapply(players, function(x) {
      x <- x[[1]]
      paste(x[c(1:(length(x) - 1))], collapse = " ")
    }))
    
    dat$Offense <- players
    colnames(dat) <- c("Player", "ADP", "AvgRound", "PercentDrafted")
    dat[,2] <- as.numeric(dat[,2])
    dat[,3] <- as.numeric(dat[,3])
    dat$PercentDrafted <- as.integer(gsub("%", "", dat$PercentDrafted))
    
    adp <- rbind(adp, dat)
  }
  
  return(adp)
}