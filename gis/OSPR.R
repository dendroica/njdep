library(sf)
library(terra)
library(dplyr)
library(mapview)

base_path <- Sys.getenv("GIS")
co <- read_sf(dsn=file.path(base_path, "NJ_Counties_3424_3190026769482268997.gpkg"))
co <- co[co$COUNTY=="MONMOUTH",]
co$NAME <- co$COUNTY
st_geometry(co) <- "geometry"
co <- co[,"NAME"]
towns <- read_sf(dsn=file.path(base_path, "NJ_Municipalities_3857_-482933278821945072.gpkg"))
towny <- towns[towns$MUN %in% c("WILDWOOD CREST BORO", "CAPE MAY POINT BORO", 
                                "WEST CAPE MAY BORO", "CAPE MAY CITY",
                                "WEST WILDWOOD BORO", "NORTH WILDWOOD CITY",
                                "WILDWOOD CITY", "LOWER TWP",
                                "STONE HARBOR BORO", "SEA ISLE CITY", "AVALON BORO", "OCEAN CITY"),]
towny$NAME <- ifelse(towny$MUN %in% c("WILDWOOD CREST BORO", "WEST WILDWOOD BORO",
                                             "NORTH WILDWOOD CITY",
                                      "WEST CAPE MAY BORO",
                                      "CAPE MAY POINT BORO",
                                      "CAPE MAY CITY", "WILDWOOD CITY", "LOWER TWP"),
                                "Lower Cape May Co", towny$MUN)
towny$NAME <- ifelse(towny$NAME %in% c("STONE HARBOR BORO",
                                       "AVALON BORO"),
                     "7 Mi Is", towny$NAME)
towny <- st_transform(towny, crs=st_crs(co))
st_geometry(towny) <- "geometry"
towny <- towny[,"NAME"]
towny <- towny %>%
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
great <- st_as_sfc(st_bbox(c(xmin = -74.53, xmax = -74.2, ymax = 39.6, ymin = 39.3), crs= st_crs(4326))) #Great Bay

#39.34046,-74.46917
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
layer_list <- list(great, sedge, nc_dissolved, towny, co)
combined_layer <- do.call(rbind, layer_list)
combined_layer$NAME <- ifelse(combined_layer$NAME %in% c("OCEAN CITY",
                                                     "Great Egg Harbor"),
                              "GEH", combined_layer$NAME)
combined_layer <- combined_layer %>%
  group_by(NAME) %>%
  summarise()

#st_write(combined_layer, file.path(Sys.getenv("FILEPATH"), "data/ospr/focal.gpkg"))
ospr <- read_sf(file.path(Sys.getenv("FILEPATH"), "data/ospr/ospr_nests.gpkg"))
ospr <- st_transform(ospr, st_crs(combined_layer))
ospr_areas <- st_intersection(ospr, combined_layer)
#st_write(ospr_areas, file.path(Sys.getenv("FILEPATH"), "data/ospr/nests_clipped.gpkg"))
#mcp <- st_convex_hull(st_union(ospr_areas))
#mapview(mcp) + mapview(co) + mapview(rivers) + mapview(ospr_areas)

hull_grouped <- ospr_areas %>%
  group_by(NAME) %>%
  summarize(geometry = st_convex_hull(st_union(geom)))
#st_write(hull_grouped, "hulls.gpkg")
mapview(hull_grouped) + mapview(combined_layer) + mapview(ospr_areas)

combined_layer[8,] <- st_difference(combined_layer[8, ], st_union(combined_layer[7,]))
combined_layer[2,] <- st_difference(combined_layer[2, ], st_union(combined_layer[7,]))
combined_layer[2,] <- st_difference(combined_layer[2, ], st_union(combined_layer[10,]))
combined_layer[2,] <- st_difference(combined_layer[2, ], st_union(combined_layer[4,]))
combined_layer[3,] <- st_difference(combined_layer[3, ], st_union(combined_layer[4,]))

focal_areas <- st_intersection(hull_grouped, st_union(combined_layer))
focal_areas[2,] <- st_difference(focal_areas[2, ], st_union(combined_layer[7,]))
focal_areas[2,] <- st_difference(focal_areas[2, ], st_union(focal_areas[10,]))
focal_areas[2,] <- st_difference(focal_areas[2, ], st_union(combined_layer[4,]))
focal_areas[3,] <- st_difference(focal_areas[3, ], st_union(combined_layer[4,]))
focal_areas[2,] <- st_difference(focal_areas[2, ], st_union(focal_areas[4,]))

habitat <- read_sf(file.path(Sys.getenv("FILEPATH"), "data/ospr/ospr_habitat.gpkg"))
habitat <- st_as_sf(st_transform(habitat, crs=st_crs(co)))
habitat[,2:48] <- NULL
habitat_focal <- st_intersection(habitat, focal_areas)
#habitat_focal$ACRES <- NULL
#habitat_focal$SHAPE_Length <- NULL
#habitat_focal$SHAPE_Area <- NULL
#habitat_focal$AREASQKM <- NULL
#habitat_focal$ELEVATION <- NULL
#habitat_focal$LEVELELEV <- NULL
#habitat_focal$LAST_EDITED_DATE <- NULL
#habitat_focal$LAST_EDITED_USER <- NULL
#habitat_focal$CREATED_DATE <- NULL
#habitat_focal$CREATED_USER <- NULL
#habitat_focal$FEATURE_NAME <- NULL
#habitat_focal$FEATURE_CLASS <- NULL
#habitat_focal$FEATURE_ID <- NULL
#habitat_focal$WATERBODY_NAME <- NULL
#habitat_focal$REACHCODE <- NULL
#habitat_focal$GNIS_NAME <- NULL
#habitat_focal$GNIS_ID <- NULL
#habitat_focal$RESOLUTION <- NULL
#habitat_focal$FDATE <- NULL
#habitat_focal$PERMANENT_IDENTIFIER <- NULL
#habitat_focal$COMID <- NULL

habitat_focal$area_acres <- st_area(habitat_focal) / 43560
habitat_focal$n_points <- lengths(st_intersects(habitat_focal, ospr_areas))
habitat_focal$density <- habitat_focal$n_points / habitat_focal$area_acres

nj  <- read_sf(file.path(Sys.getenv("FILEPATH"), "data/NJ_State_Boundary_-5862472423523821984.gpkg"))
nj <- st_transform(nj, crs=st_crs("EPSG:3857"))
r <- rast(file.path(Sys.getenv("FILEPATH"), "data/ospr/Annual_NLCD_CONUSV1_Ref_Data_Release/lcnext-1.0-stratum-map-Clipped.tif"))
pts <- read_sf(file.path(Sys.getenv("FILEPATH"), "data/ospr/nests_recent.gpkg"))
pts <- st_transform(pts, crs(r))
extracted_values <- terra::extract(r, pts)

#get rid of 42, 43, 52, 71
