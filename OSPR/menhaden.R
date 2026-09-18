library(sf)
menhaden <- read.csv(file.path(Sys.getenv("FILEPATH"), "data/John JJ Egan bunker totals by tow (002)(Original data).csv"))
menhaden$Site.Name[menhaden$Site.Name %in% c("Berkeley Island Park", "Berkeley Island Park Profile")] <- "Berkeley Island Beach"
menhaden$Site.Name[menhaden$Site.Name %in% c("Cattus Island Park Beach")] <- "Cattus Isl Control"
menhaden$Site.Name[menhaden$Site.Name %in% c("Cattus Island Park Point")] <- "Cattus Isl Point"
menhaden$Site.Name[menhaden$Site.Name %in% c("Iowa Point")] <- "Iowa Cove"
menhaden$Site.Name[menhaden$Site.Name %in% c("East Green Street Cove")] <- "East Green Street Point"
menhaden$Site.Name[menhaden$Site.Name %in% c("Ocean Gate")] <- "Angelsea Ave"
menhaden$Site.Name[menhaden$Site.Name %in% c("Lavallette")] <- "Brooklyn Ave"
menhaden$Site.Name[menhaden$Site.Name %in% c("Great Bay Blvd. (1st bridge)")] <- "First Bridge Beach"

seine <- read.csv(file.path(Sys.getenv("FILEPATH"), "data/seining locations_CORRECTED.csv"))
seine$Name[6] <- "Browns Woods Beach"
seine$Name[15] <- "East Green Street Point"
seine$Name[11] <- "Barnegat Public Beach"
seine <- seine %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)