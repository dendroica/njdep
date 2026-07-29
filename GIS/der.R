library(sf)
library(dplyr)
library(ggplot2)
sf::sf_use_s2(FALSE)

#de <- st_read("C:/Users/galax/Downloads/delriv", layer="delawareriver")
#de <- subset(de, TYPE=="water" & NAME=="Delaware River")
#de <- st_transform(de, crs=st_crs(pts))
#clipped_sf <- st_crop(de, st_bbox(pts))
pseg <- st_read("pseg_strata.gpkg")
pts <- read.delim("deleteMe.txt")
pts <- st_as_sf(pts, coords = c("Longitude", "Latitude"), crs = 4326)

dissolved_data <- st_union(st_crop(pseg, st_bbox(pts)))

ggplot() +
  geom_sf(data = dissolved_data, fill = "lightblue") +  # Bottom layer (e.g., boundaries)
  geom_sf(data = pts, color = "red") 
#st_write(dissolved_data, "~/DERsection_pseg.gpkg")
