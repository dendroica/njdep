library(sf)
library(mapview)
mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldGrayCanvas", "Esri.WorldImagery"))

edna <- read.csv(file.path(Sys.getenv("FILEPATH"), "data/df_env_colleen.csv"))
edna <- edna[!is.na(edna$longavg),]
edna$lon <- as.numeric(unlist(lapply(strsplit(edna$longavg,"_"),"[[",1)))
edna$lat <- as.numeric(unlist(lapply(strsplit(edna$latavg,"_"),"[[",1)))
edna[edna$lon > 0,]$lon <- edna[edna$lon > 0,]$lon * -1
edna <- edna %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
mapview(edna) #Show in New Window from RStudio Viewer to get the map to render