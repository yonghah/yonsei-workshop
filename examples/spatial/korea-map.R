library(sp)
library(maps)
library(maptools)
library(mapdata)
library(leaflet)


korea <- map("world2Hires", "South Korea", fill=TRUE, plot=FALSE)
korea_gon <- map2SpatialPolygons(korea, sub(":.*$", "", korea$names))
korea_spdf <- SpatialPolygonsDataFrame(korea_gon,
                                      data.frame(country=names(korea_gon), 
                                                 stringsAsFactors=FALSE), 
                                      FALSE)
leaflet(korea_spdf) %>% 
  addTiles() %>% 
  addPolygons(weight=1)