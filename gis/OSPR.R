library(sf)
library(dplyr)
library(mapview)
base_path <- Sys.getenv("GIS")
co <- read_sf(dsn=file.path(base_path, "NJ_Counties_3424_3190026769482268997.gpkg"))
co <- co[co$COUNTY=="MONMOUTH",]
towns <- read_sf(dsn=file.path(base_path, "NJ_Municipalities_3857_-482933278821945072.gpkg"))
towns <- towns[towns$MUN %in% c("WILDWOOD CREST BORO", "STONE HARBOR BORO", "SEA ISLE CITY", "AVALON BORO"),]
towns <- st_transform(towns, crs=st_crs(co))
river <- read_sf(file.path(base_path, "Watershed_Management_Areas_in_New_Jersey/Watershed_Management_Areas_in_New_Jersey.shp"))
rivers <- river[river$WMA_NAME %in% c("Hackensack, Hudson, and Pascack",
                                      "North and South Branch Raritan",
                                      "Lower Raritan, South River, and Lawrence",
                                      "Barnegat Bay","Great Egg Harbor","Cape May"),]
rivers <- st_transform(rivers, crs=st_crs(co))
sedge <- read_sf(file.path(base_path, "sedge.geojson"))
sedge <- st_transform(sedge, crs=st_crs(co))
great <- st_as_sfc(st_bbox(c(xmin = -74.5, xmax = -74.2, ymax = 39.6, ymin = 39.3), crs= st_crs(4326))) #Great Bay
great <- st_transform(great, crs=st_crs(co))
areas <- st_union(great, sedge)
areas <- st_union(areas, rivers)
areas <- st_union(areas, towns)
areas <- st_union(areas, co)

ospr <- read_sf(file.path(Sys.getenv("FILEPATH"), "data/ospr/ospr_nests.gpkg"))
ospr <- st_transform(ospr, st_crs(areas))
ospr_areas <- st_intersection(ospr, areas)
#st_write(ospr_areas, file.path(Sys.getenv("FILEPATH"), "data/ospr/nests_clipped.gpkg"))
mcp <- st_convex_hull(st_union(ospr_areas))

mapview(mcp) + mapview(co) + mapview(rivers) + mapview(ospr_areas)

mcp_grouped <- areas %>%
  st_intersection(ospr_areas, .) %>%
  st_convex_hull()
