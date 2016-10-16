library(httr)
library(jsonlite)
library(dplyr)
library(leaflet)

# 공공보건포털 xhr
url <- "http://www.g-health.kr/portal/health/pubHealthSearch/get_list.do"

# 보건기관 코드에 따라 데이터 가져오기
getMedIns <- function(code) {
  # code 71, 72, 73 Bogeonso
  # code 81 Pharmacy
  pl <- list(cl_cd=code, rows=30000, cpage=1)
  r <- POST(url, 
            body = pl, 
            add_headers(
              'Content_Type' = 
                'application/x-www-form-urlencoded; charset=UTF-8' 
            )
  )
  res <- content(r,"text")
  df <- jsonlite::fromJSON(res)
  # 경위도 없는 데이터 제외
  df <- df %>%
    filter(x_pos != '') %>%
    filter(y_pos != '') %>%
    filter(cl_cd != '')
  return(df)
}

# 보건소, 보건지소, 보건진료소
bg71 <- getMedIns(71)
bg72 <- getMedIns(72) 
bg73 <- getMedIns(73) 
bg <- bind_rows(bg71, bg72, bg73)

# 약국
pharm <- getMedIns(81)

m <- leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>% 
  setView(lng = 127.5, lat = 36, zoom = 7)

pal <- colorFactor(c("red", "green", "yellow", "orange"), 
                   domain = c("71", "72", "73", "81"))

addCircleMarkers(m, lng = bg$x_pos, lat=bg$y_pos, 
                 radius = 2,
                 color = pal(bg$cl_cd),
                 stroke = FALSE, 
                 fillOpacity = 0.4)

addCircleMarkers(m, lng = pharm$x_pos, lat=pharm$y_pos, 
                 radius = 2,
                 color = pal(pharm$cl_cd),
                 stroke = FALSE, 
                 fillOpacity = 0.4)
