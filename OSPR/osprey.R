library(sf)
library(mapview)
library(tidyverse)
osprey <- file.path(Sys.getenv("FILEPATH"), "data/ospr/INPUT/Osprey Project Master List 2022.kml")
layers_info <- sf::st_layers(osprey)
kml <- read_sf(osprey, layer="Master Nest List")
kml <- st_zm(kml)

ensp <- read_sf(file.path(Sys.getenv("FILEPATH"), "data/ospr/INPUT/ENSP_OSPR.gpkg"))
ensp_nest <- ensp[ensp$FEAT_DESC=="Nest",]

mapview(kml[kml$Name=="111-A-024",]) + mapview(ensp_nest, col.regions="yellow")
#111-A-024 can match to osprey watch nest ID

ospr <- read.csv(file.path(Sys.getenv("FILEPATH"), "data/ospr/INPUT/OspreyWatch.csv"))
ospr <- ospr %>% 
  separate(Coords, into = c("lon", "lat"), sep = ",", convert = TRUE) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
ospr <- st_transform(ospr, crs=st_crs(ensp_nest))
