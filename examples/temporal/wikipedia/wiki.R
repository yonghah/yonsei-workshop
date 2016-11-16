library(httr)
library(ggplot2)
library(dplyr)

rvc <- ''
url <- ''

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
    mutate(title = title)
  return(rt)
}

# titles <- c('Seoul', 'Tokyo', 'Beijing', 'Shanghai', 'Hong_Kong', 'Singapore', 'Taipei')
# titles <- c('Girls\'_Generation', 'Kara_(South_Korean_band)', 'Wonder_Girls', 
#             '2NE1', 'F(x)_(band)', 'Secret_(South_Korean_band)',
#             'Sistar', 'Miss_A', 'Girl\'s_Day',
#             'AOA_(band)', 'EXID', 'Apink', 
#             'GFriend', 'Red_Velvet_(band)', 'Mamamoo', 'Twice_(band)',
#             'Lovelyz'
#             )

titles <- c('FC_Barcelona', 'Real_Madrid_C.F.', 'Atlético_Madrid', 'FC_Bayern Munich', 
            'Juventus_F.C.', 'Manchester_United_F.C.', 'Arsenal_F.C.', 'Manchester_City_F.C.', 'Chelsea F.C.', 'Liverpool_F.C.',
            'Borussia_Dortmund', 'A.C._Milan', 'Tottenham_Hotspur_F.C.')

datalist <- list()
i <- 1
for (title in titles) {
  print(title)
  datalist[[i]] <- getRevisionHistory(title, "en")
  i <- i + 1
}
rt <- bind_rows(datalist)

# horizontal chart
bandh <- 200
rtg <- rt %>%
  group_by(p=cut(date_posixct, "1 month"), title) %>%
  summarise(count=n()) %>%
  mutate(level=count %/% bandh, 
         rem = count %% bandh,
         date = as.Date(p))

max(rtg$count)
max(rtg$level)

alphaStep = 1 / (max(rtg$level) + 1)

ggplot(rtg, aes(x=date)) +
  geom_bar(aes(y=bandh, alpha = level), fill='darkred', stat='identity') +
  scale_alpha_continuous(range = c(alphaStep, 1)) +
  geom_bar(aes(y=rem), alpha = min(1, alphaStep), fill='darkred', stat='identity') +
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
#   geom_histogram(binwidth=20) +
#   facet_grid(title ~.) 
  
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


