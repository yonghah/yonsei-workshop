library(httr)
library(ggplot2)
library(dplyr)

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
  print(rvc)
  return(date)
}

getRevisionHistory <- function(title, lang){
  results <- vector()
  rvc <<-''
  url <<- paste("https://", lang, ".wikipedia.org/w/api.php", sep = "")
  # res$continue$rvcontinue가 없을 때까지 반복
  while(!is.null(rvc)) {
    t <- getRevDates(title, rvc)
    results <- c(results, t)
  }
  date_posixct <-  as.POSIXct(strptime(results, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
  rt <- as.data.frame(date_posixct, date)
  rt <- rt %>% 
    mutate(date = as.Date(date_posixct)) %>%
    # mutate(month = as.Date(cut(date, breaks = "month"))) %>%
    mutate(title = title)
  return(rt)
}

# rt1 <- getRevisionHistory('Sinking_of_MV_Sewol', "en")
# rt2 <- getRevisionHistory('세월호_침몰_사고', "ko")

# titles <- c('Seoul', 'Tokyo', 'Beijing', 'Shanghai', 'Hong_Kong', 'Singapore', 'Taipei')
titles <- c('Girls\'_Generation', 'Kara_(South_Korean_band)', 'Wonder_Girls', 
            '2NE1', 'F(x)_(band)', 'Secret_(South_Korean_band)',
            'Sistar', 'Miss_A', 'Girl\'s_Day',
            'AOA_(band)', 'EXID', 'Apink', 
            'GFriend', 'Red_Velvet_(band)', 'Mamamoo', 'Twice_(band)',
            'Lovelyz'
            )
datalist <- list()
i <- 1
for (title in titles) {
  print(title)
  datalist[[i]] <- getRevisionHistory(title, "en")
  i <- i + 1
}
rt <- bind_rows(datalist)

# horizontal chart
bandh <- 30
rtg <- rt %>%
  group_by(p=cut(date_posixct, "30 day"), title) %>%
  summarise(count=n()) %>%
  mutate(level=count %/% bandh, 
         rem = count %% bandh,
         date = as.Date(p))

alphaStep = 2 / max(rtg$level)

ggplot(rtg, aes(x=date)) +
  geom_bar(aes(y=bandh, alpha=alphaStep*level), fill='darkred', stat='identity') +
  geom_bar(aes(y=rem), alpha=alphaStep, fill='darkred', stat='identity') +
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  ylab(NULL) +
  facet_grid(title ~.) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.text.y = element_blank(),
        legend.position="none", 
        strip.text.y = element_text(angle=0),
        strip.background = element_rect(fill = "white")) 
  

# # histogram
# ggplot(rt, aes(x=date)) + 
#   geom_histogram(binwidth=10, alpha = 6/10, aes(y=..count..), fill="purple") +
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   # annotate("text", x = as.Date("2015-01-15"), y = 80, label = "Glass Bead") + # 유리구슬
#   # annotate("text", x = as.Date("2015-07-23"), y = 50, label = "Me gustas tu") + # 오늘부터 우리는
#   # annotate("text", x = as.Date("2016-01-25"), y = 180, label = "Rough") + # 시간을 달려서
#   # annotate("text", x = as.Date("2016-07-11"), y = 100, label = "NAVILLERA") + #너 그리고 나
#   labs(title = "Number of revisions in the Wikipedia page") +
#   ylab("Count in 10 days") +
#   facet_grid(title ~.) +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
#   

# # heatmap
# ggplot(rt, aes(x=date, y=0)) +
#   stat_density(aes(fill = ..density..), geom = "raster", position = "identity") +
#   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))

# density 
# ggplot(rt,aes(x=month)) +
#   stat_density(aes(ymax=..density.., ymin=-..density..),  alpha = 5/10,
#                # geom='ribbon',
#                fill='orange') +
#   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
