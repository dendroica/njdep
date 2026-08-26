library(sf)
library(dplyr)
library(tidyverse)
library(mapview)
library(stringr)
ensp <- read_sf(file.path(Sys.getenv("FILEPATH"), "data/ospr/INPUT/ENSP_OSPR.gpkg"))
ensp_nest <- ensp[ensp$FEAT_DESC=="Nest",]

ospr <- read.csv(file.path(Sys.getenv("FILEPATH"), "data/ospr/INPUT/OspreyWatch.csv"))
ospr <- ospr %>% 
  separate(Coords, into = c("lon", "lat"), sep = ",", convert = TRUE) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
ospr <- st_transform(ospr, crs=st_crs(ensp_nest))

ensp_match <- ensp_nest
ensp_match$ID <- ensp_match$SF_ID 
ensp_match$Nickname <- as.character(ensp_match$LINK_ID) 
ensp_match$Substrate <- "ENSP Unrecorded"
ensp_match$Last.Watched <- str_extract(ensp_match$LAST_OBS, "\\d{4}")
ensp_match$Status <- "unknown"
ensp_match$City.County <- NA
ensp_match$State <- "New Jersey"
ensp_match$Postal.Code <- NA
ensp_match <- ensp_match[,names(ensp_match) %in% names(ospr)]
all_nests <- bind_rows(ospr, ensp_match)
#write_sf(all_nests, "ospr_nests.gpkg")

ospr_na <- ospr[ospr$Substrate %in% c("", "Other", "CM 'LB'", "Coal Dist Arm", "CM",
                              "Don't Know", "Nothing Here", "New Jersey Osprey Project",
                              "Inactive. No Nest",
                              "Other (Specify)") | is.na(ospr$Substrate),]
ospr_natural <- ospr[ospr$Substrate %in% c("Dead Tree", "Live Tree", "Marsh", "Island",
                                   "Snag In Water", "Branches/twigs",
                                   "Old Red Tail Hawk Nest",
                                   "Dead Tree W/ Platform Supports",
                                   "Ground Nest") & !is.na(ospr$Substrate),]
ospr_naturals <- rbind(ospr_na, ospr_natural)
#mapview(ensp_nest[13,]) + mapview(ospr[19,])
mapview(all_nests, color="red") + mapview(ospr_naturals)

ospr26 <- ospr[ospr$Last.Watched=="2026",]

#load habitat_focal from focal_areas.gpkg
all_nests <- st_transform(all_nests, crs=st_crs(habitat_focal))
ospr_naturals <- st_transform(ospr_naturals, crs=st_crs(habitat_focal))

habitat_focal$n_points_nat <- lengths(st_intersects(habitat_focal, ospr_naturals))
habitat_focal$density_nat <- habitat_focal$n_points_nat / habitat_focal$area_acres

habitat_focal$n_points_all <- lengths(st_intersects(habitat_focal, all_nests))
habitat_focal$density_all <- habitat_focal$n_points_all / habitat_focal$area_acres

habitat_focal$n_points26 <- lengths(st_intersects(habitat_focal, ospr26))
habitat_focal$density26 <- habitat_focal$n_points26 / habitat_focal$area_acres
