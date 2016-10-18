library(httr)
library(ggplot2)
library(dplyr)

url <- "https://en.wikipedia.org/w/api.php"
rvc <- ''

getRevDates <- function(title, rvid) {
  pl <- list(
    action='query', 
    prop='revisions', 
    titles=title,
    format='json',
    rvlimit='500')
  if (rvid != '') {
    pl <- c(pl, rvcontinue=rvid)
    
  }
  r <- POST(url, body = pl)
  res <- content(r)
  rev <- res$query$pages[[1]]$revisions
  date <- unlist(lapply(rev, function(x) x$timestamp))
  date_posix <- as.POSIXct(strptime(date, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
  rvc <<- res$continue$rvcontinue
  print("----------------------next-------------")
  print(rvc)
  return(date)
}

getRevisionHistory <- function(title){
  results <- vector()
  while(!is.null(rvc)) {
    t <- getRevDates(title, rvc)
    results <- c(results, t)
  }
  
  date_posixct <-  as.POSIXct(strptime(results, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
  rt <- as.data.frame(date_posixct, date)
  rt <- rt %>% 
    mutate(date = as.Date(date_posixct)) %>%
    mutate(month = as.Date(cut(date, breaks = "month")))
  return(rt)
}

rvc <- ''
title <- 'GFriend'
rt <- getRevisionHistory('GFriend')
#rt_en <- getRevisionHistory('Sinking_of_MV_Sewol')
#rt_ko <- getRevisionHistory('세월호_침몰_사고')
rt_horizon <- rt %>% 
  group_by(date) %>%
  summarize(counts =n()) %>%
  mutate(title=title) %>%
  select(date, title, counts)

horizonscale = 10
horizon.panel.ggplot(rt_horizon, title)

ggplot(rt, aes(x=date)) + 
  geom_histogram(binwidth=10) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  #coord_cartesian(ylim=c(0, 100)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(rt,aes(x=month)) + 
  stat_density(aes(ymax=..density.., ymin=-..density..), 
               geom='ribbon', 
               fill='gray') +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
