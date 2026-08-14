library(sf)
library(dplyr)
library(mapview)
base_path <- Sys.getenv("GIS")
co <- read_sf(dsn=file.path(base_path, "NJ_Counties_3424_3190026769482268997.gpkg"))
co <- co[co$COUNTY=="MONMOUTH",]
co$NAME <- co$COUNTY
st_geometry(co) <- "geometry"
co <- co[,"NAME"]
towns <- read_sf(dsn=file.path(base_path, "NJ_Municipalities_3857_-482933278821945072.gpkg"))
towns <- towns[towns$MUN %in% c("WILDWOOD CREST BORO", "CAPE MAY POINT BORO", 
                                "WEST CAPE MAY BORO", "CAPE MAY CITY",
                                "WEST WILDWOOD BORO", "NORTH WILDWOOD CITY",
                                "STONE HARBOR BORO", "SEA ISLE CITY", "AVALON BORO", "OCEAN CITY"),]
towns$NAME <- ifelse(towns$MUN %in% c("WILDWOOD CREST BORO", "WEST WILDWOOD BORO",
                                             "NORTH WILDWOOD CITY",
                                      "WEST CAPE MAY BORO",
                                      "CAPE MAY POINT BORO",
                                      "CAPE MAY CITY"),
                                "2 Mi Island", towns$MUN)
towns$NAME <- ifelse(towns$NAME %in% c("STONE HARBOR BORO",
                                       "AVALON BORO"),
                     "7 Mi Island", towns$NAME)
towns <- st_transform(towns, crs=st_crs(co))
st_geometry(towns) <- "geometry"
towns <- towns[,"NAME"]
towns <- towns %>%
  group_by(NAME) %>%
  summarise()

river <- read_sf(file.path(base_path, "Watershed_Management_Areas_in_New_Jersey/Watershed_Management_Areas_in_New_Jersey.shp"))
rivers <- river[river$WMA_NAME %in% c("Hackensack, Hudson, and Pascack",
                                      "North and South Branch Raritan",
                                      "Lower Raritan, South River, and Lawrence",
                                      "Barnegat Bay","Great Egg Harbor"),] #"Cape May
rivers$dissolve_group <- ifelse(rivers$WMA_NAME %in% c("North and South Branch Raritan",
                                               "Lower Raritan, South River, and Lawrence"),
                            "Raritan", rivers$WMA_NAME)
rivers <- st_transform(rivers, crs=st_crs(co))
rivers$NAME <- rivers$dissolve_group
rivers <- rivers[,"NAME"]
nc_dissolved <- rivers %>%
  group_by(NAME) %>%
  summarise() # st_union happens automatically here

sedge <- read_sf(file.path(base_path, "sedge.geojson"))
sedge <- st_transform(sedge, crs=st_crs(co))
great <- st_as_sfc(st_bbox(c(xmin = -74.5, xmax = -74.2, ymax = 39.6, ymin = 39.3), crs= st_crs(4326))) #Great Bay
great <- st_as_sf(st_transform(great, crs=st_crs(co)))
st_geometry(great) <- "geometry"
great$NAME <- "Great Bay"

#areas <- st_union(great, sedge)
#areas <- st_union(areas, rivers)
#areas <- st_union(areas, towns)
#areas <- st_union(areas, co)
#polygon_sf <- st_sf(
#  id = 1:24,
#  geometry = areas
#)
layer_list <- list(great, sedge, nc_dissolved, towns, co)
combined_layer <- do.call(rbind, layer_list)
combined_layer$NAME <- ifelse(combined_layer$NAME %in% c("OCEAN CITY",
                                                     "Great Egg Harbor"),
                              "GEH", combined_layer$NAME)
combined_layer <- combined_layer %>%
  group_by(NAME) %>%
  summarise()
#st_write(combined_layer, file.path(Sys.getenv("FILEPATH"), "data/ospr/focal.gpkg"))
ospr <- read_sf(file.path(Sys.getenv("FILEPATH"), "data/ospr/ospr_nests.gpkg"))
ospr <- st_transform(ospr, st_crs(areas))
ospr_areas <- st_intersection(ospr, combined_layer)
#st_write(ospr_areas, file.path(Sys.getenv("FILEPATH"), "data/ospr/nests_clipped.gpkg"))
#mcp <- st_convex_hull(st_union(ospr_areas))
#mapview(mcp) + mapview(co) + mapview(rivers) + mapview(ospr_areas)

hull_grouped <- ospr_areas %>%
  group_by(NAME) %>%
  summarize(geometry = st_convex_hull(st_union(geom)))
#st_write(hull_grouped, "hulls.gpkg")
mapview(hull_grouped) + mapview(combined_layer) + mapview(ospr_areas)
