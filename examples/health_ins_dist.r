options(warn=0)

library(httr)
library(jsonlite)
library(dplyr)
library(leaflet)
library(sp)
library(deldir)
library(maps)
library(maptools)
library(mapdata)
library(rgeos)
library(ineq)
library(ggplot2)

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
  df$x_pos <- as.numeric(df$x_pos)
  df$y_pos <- as.numeric(df$y_pos)
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

# color by code
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

###################
# Create voronoi 
###################

korea <- map("world2Hires", "South Korea", fill=TRUE, plot=FALSE)
korea_gon <- map2SpatialPolygons(korea, sub(":.*$", "", korea$names))

# function getting voronoi polygon dataframe
df_to_voronoi_polygon <- function(df) {
  
  vor_data <- df %>% select(x_pos, y_pos)
  sp <- SpatialPointsDataFrame(cbind(vor_data$x_pos,
                                     vor_data$y_pos),
                               vor_data, match.ID=TRUE)
  vor_desc <- tile.list(deldir(sp@coords[,1], sp@coords[,2]))
  lapply(1:(length(vor_desc)), function(i) {
    tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
    tmp <- rbind(tmp, tmp[1,])
    Polygons(list(Polygon(tmp)), ID=i)
  }) -> vor_polygons
  
  raw_gons <- SpatialPolygons(vor_polygons)
  
  # clip voronoi polygons
  vor_gons <- gIntersection(korea_gon, raw_gons, byid=TRUE)
  
  pid <- sapply(slot(vor_gons, "polygons"), function(x) slot(x, "ID"))
  p.df <- data.frame( ID=1:length(vor_gons), row.names = pid)    
  
  vor.spdf<-SpatialPolygonsDataFrame(vor_gons, data=p.df)
  return(vor.spdf)
}


## Voronoi diagram for bogeonso
bg.spdf <- df_to_voronoi_polygon(bg)
addPolygons(m, data=bg.spdf,
            stroke=TRUE, color="#88FF33", weight=0.5,
            fill=TRUE, fillOpacity = 0.0,
            smoothFactor=0.5)

bg.area <- sapply(slot(bg.spdf, "polygons"), slot, "area")
bg.gini <- round(ineq(bg.area, type="Gini"),3)
bg.lc <- Lc(bg.area)
bg.lc_df <- data.frame(bg.lc[1], bg.lc[2])

ggplot(data=bg.lc_df) +
  geom_point(aes(x=p, y=L), color="purple") +
  geom_line(aes(x=p, y=L), color="purple") +
  scale_x_continuous(limits=c(0,1)) + 
  scale_y_continuous(limits=c(0,1)) +
  geom_abline() +
  labs(title = "보건소 서비스 면적의 로렌츠 곡선") +
  annotate("text", x = 0.1, y = 0.95, label = paste("Gini:", bg.gini))

## Voronoi diagram for pharm
pharm.spdf <- df_to_voronoi_polygon(pharm)
addPolygons(m, data=pharm.spdf,
            stroke=TRUE, color="orange", weight=0.5,
            fill=TRUE, fillOpacity = 0.0,
            smoothFactor=0.5)

pharm.area <- sapply(slot(pharm.spdf, "polygons"), slot, "area")
pharm.gini <- round(ineq(pharm.area, type="Gini"),3)
pharm.lc <- Lc(pharm.area)
pharm.lc_df <- data.frame(pharm.lc[1], pharm.lc[2])

ggplot(data=pharm.lc_df) +
  geom_point(aes(x=p, y=L), color="purple") +
  geom_line(aes(x=p, y=L), color="purple") +
  scale_x_continuous(limits=c(0,1)) + 
  scale_y_continuous(limits=c(0,1)) +
  geom_abline() +
  labs(title = "약국 서비스 면적의 로렌츠 곡선") +
  annotate("text", x = 0.1, y = 0.95, label = paste("Gini:", pharm.gini))