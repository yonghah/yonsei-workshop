library(rvest)

mf <- html("http://www.imdb.com/name/nm0000151/")

# title
mf_title <- mf %>% 
  html_nodes("#filmography .filmo-category-section div b a") %>%
  html_text() 

# year
mf_year <- mf %>%
  html_nodes("#filmography .filmo-category-section div span") %>%
  html_text() %>%
  trimws()

# link to movie
mf_link <- mf %>%
  html_nodes("#filmography .filmo-category-section div b a") %>%
  html_attr("href") 

actors <- paste("http://www.imdb.com", mf_link[2], sep='') %>%
  read_html() %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

getTitleActors <- function(link) {
  actors <- paste("http://www.imdb.com", link, sep='') %>%
    read_html() %>%
    html_nodes("#titleCast .itemprop span") %>%
    html_text()
  return(actors)
}

al <- lapply(mf_link, getTitleActors)

alee <- read_html("http://www.imdb.com/name/nm3880181/?ref_=nv_sr_4") %>%
  html_nodes("#filmography .filmo-category-section div b a") %>%
  html_attr("href") 

alee_costar <- lapply(alee, getFullCredits)


getFullCredits <- function(link) {
  id <- strsplit(link, "\\?")[[1]][1]
  link <- paste("http://www.imdb.com", id, "fullcredits?ref_=tt_cl_sm#cast", sep='')
  actors <- 
    read_html(link) %>%
    html_nodes("#fullcredits_content .itemprop span") %>%
    html_text()
  return(actors)
}