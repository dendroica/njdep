library(sf)
menhaden <- read.csv(file.path(Sys.getenv("FILEPATH"), "data/John JJ Egan bunker totals by tow (002)(Original data).csv"))
menhaden$Site.Name[menhaden$Site.Name %in% c("Berkeley Island Park", "Berkeley Island Park Profile")] <- "Berkeley Island Beach"
menhaden$Site.Name[menhaden$Site.Name %in% c("Cattus Island Park Beach")] <- "Cattus Isl Control"
menhaden$Site.Name[menhaden$Site.Name %in% c("Cattus Island Park Point")] <- "Cattus Isl Point"

ospr <- read.csv(file.path(Sys.getenv("FILEPATH"), "data/seining locations_CORRECTED.csv"))
ospr$Name[6] <- "Browns Woods Beach"
ospr$Name[15] <- "East Green Street Point"
ospr$Name[11] <- "Barnegat Public Beach"
ospr <- ospr %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
#ospr <- st_transform(ospr, crs=st_crs(ensp_nest))