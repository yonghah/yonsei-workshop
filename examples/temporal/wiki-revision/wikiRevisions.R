#' create data frame showing amount of revisions  in Wikipedia for each keyword
#'
#' @param titls vector of Wikipedia page titles
#' @param lang language of wikipedia such as en
#' @return data frame with date_posix, date, title
#' @examples
#'  df <- wikiRevisions(c("seoul", "tokyo"),"en")

wikiRevisions <- function(titles, lang) {
  rvc <- ''
  url <- ''
  
  getRevDates <- function(title, rvid, url) {
    pl <- list(
      action='query', 
      prop='revisions', 
      titles=title,
      format='json',
      rvlimit='500')
    if (rvid != '') {
      pl <- c(pl, rvcontinue=rvid)
    }
    r <- httr::POST(url, body = pl)
    res <- httr::content(r)
    rev <- res$query$pages[[1]]$revisions
    date <- unlist(lapply(rev, function(x) x$timestamp))
    date_posix <- as.POSIXct(strptime(date, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
    rvc <<- res$continue$rvcontinue
    return(date)
  }
  
  getRevisionHistory <- function(title, lang){
    results <- vector()
    rvc <<-''
    url <- paste("https://", lang, ".wikipedia.org/w/api.php", sep = "")
    
    # loop until res$continue$rvcontinue is null
    while(!is.null(rvc)) {
      t <- getRevDates(title, rvc, url)
      results <- c(results, t)
    }
    date_posix <-  as.POSIXct(strptime(results, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
    rt <- as.data.frame(date_posix)
    rt <- rt %>% 
      dplyr::mutate(date = as.Date(date_posix), 
                    title=title) 
    return(rt)
  }
  
  df <- do.call(rbind, lapply(titles, getRevisionHistory, lang=lang))
  return(df)
}

