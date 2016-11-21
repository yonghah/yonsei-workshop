source("wikiRevisions.R")

titles <- c('Girl\'s_Day', 'AOA_(band)')
df <- wikiRevisions(titles, "en")

library(ggplot2)
ggplot(df, aes(x=date)) + 
  geom_histogram(binwidth=20) + 
  facet_grid(title~.)