library(sf)
# 2. Define the path to your .gdb folder
gdb_path <- "~/osprey.gdb"
# 3. List all available layers inside the geodatabase
st_layers(gdb_path)
# 4. Import a specific layer by providing its name
my_layer <- st_read(dsn = gdb_path, layer = "soa_osprey")
pts <- sf::st_centroid(my_layer)
#mapview(pts)
