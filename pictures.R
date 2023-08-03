library(XML) # HTML processing
options(stringsAsFactors = FALSE)

base.url = 'http://a.espncdn.com/combiner/i?img=/i/headshots/nfl/players/full/'
download.folder = "www"
if(!(dir.exists(download.folder))) {dir.create(download.folder)}
directory <- read.csv('ids.csv')
directory$name <- toupper(gsub("[ ]","", directory$player))


pb <- txtProgressBar(0,nrow(directory),style=3)
for (i in 1:nrow(directory)) {
  url = paste(base.url,directory$id[i],".png",sep="")
  if(length(url) > 0) {
    jpg.name <- paste(download.folder,"/", directory$player[i], '.png', sep = '')
    try(
      download.file(url, jpg.name, method = 'auto', quiet = TRUE, mode = "w",
                    cacheOK = TRUE, extra = getOption("download.file.extra")))
    setTxtProgressBar(pb,i)
  }
}
close(pb)