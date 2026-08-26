library(ebirdst)
library(mapview)
library(sf)
library(tigris) # Or another source for state boundaries
library(terra)
abd_seasonal <- load_raster(
  species = "osprey",
  product = "occurrence",
  period = "seasonal",
  metric = "max",
  resolution = "3km",
  force = TRUE
)
#ebirdst_download_status(
#  species = "osprey", 
#  force = TRUE
#)
ospr <- abd_seasonal[["breeding"]]
nj_boundary <- states(cb = TRUE) |> 
  subset(NAME == "New Jersey")
my_data <- st_transform(nj_boundary, st_crs(ospr))
nj_clipped_data <- crop(ospr, my_data, snap="out", mask=TRUE)
#ospr_nj <- mask(nj_clipped_data, nj_clipped_data > 0, maskvalues = FALSE)

base_path <- Sys.getenv("GIS")
#Nesting densities higher near shallow-water environments suggesting a
#preference for such areas (fish can be caught in deep water only when they
#occur near the surface or are driven to the surface).

gdb_path <- "osprey.gdb"
#st_layers(file.path(base_path, gdb_path))
my_layer <- read_sf(dsn = file.path(base_path, gdb_path), layer = "soa_osprey")
nests <- sf::st_centroid(my_layer)

mapview(nj_clipped_data) + mapview(nests)
