# scan for sturgeon (Atlantic and short nose) and turtle (all species) records and join only those records
# source("2-pseg_datafix.R")

root <- Sys.getenv("FILEPATH")
path <- file.path(root, "data/PSEG/PSEG_Data_compiled.RData")
load(path)
library(sf)
sf::sf_use_s2(FALSE)
library(dplyr)

fullnames <- gsub("(Level [0-9]_[A-Za-z ]* [0-9]{4}).*", "\\1", names(dups))
gears <- unique(sapply(fullnames, function(y) gsub("Level [0-9]_([A-Za-z ]*) [0-9]{4}.*", "\\1", y)))
samples <- sapply(gears, function(x) {
  sum(sapply(dups[which(grepl(paste0("Level 1_", x), fullnames))], nrow))
})

sppofinterest <- "BLACK DRUM"

getspp <- function(x) {
  if ("COMMON_NAME" %in% names(x)) {
    x <- x[grep(sppofinterest, x$COMMON_NAME), ] # "STRIPED BASS STURGEON|TURTLE
  } else {
    x <- data.frame()
  }
  return(x)
}
turtle <- lapply(dups, getspp)
turtle <- turtle[sapply(turtle, nrow) > 0]

load(file.path(root, "data/PSEG/pseg_dict.RData"))
beachlocs <- read.csv(file.path(root, "data/PSEG/PSEG 2025 Beach Seine Locations.csv"))
coords <- apply(beachlocs[, 2:3], 2, strsplit, " ")
deg <- apply(as.data.frame(lapply(coords, function(x) unlist(lapply(x, "[[", 1)))), 2, as.numeric)
min <- apply(as.data.frame(lapply(coords, function(x) unlist(lapply(x, "[[", 2)))), 2, as.numeric) / 60
beachlocs <- as.data.frame(deg + min)
beachlocs$sta <- 1:nrow(beachlocs)
beachlocs$lng <- beachlocs$lng * -1

trawllocs <- read.csv(file.path(root, "data/PSEG/PSEG Bottom Trawl Locations.csv"))[, 1:3]
df_sf <- st_as_sf(
  x = trawllocs,
  coords = c("Easting", "Northing"),
  crs = "+proj=utm +zone=18 +datum=NAD83"
)
sfc <- st_transform(df_sf, crs = "+proj=longlat +datum=WGS84")
df <- sfc %>%
  dplyr::mutate(
    lng = sf::st_coordinates(.)[, 1],
    lat = sf::st_coordinates(.)[, 2]
  )

strata <- read_sf(file.path(root, "data/PSEG/pseg_strata.gpkg"))
joined_points <- st_join(df, strata, join = st_intersects)

lvl2 <- Map(function(x, y) {
  gear <- gsub("Level [0-9]_([A-Za-z ]*) [0-9]{4}.*", "\\1", y)
  yr <- as.integer(gsub("Level [0-9]_[A-Za-z ]* ([0-9]{4}).*", "\\1", y))
  fish <- x$COMMON_NAME
  print(y)
  if (gsub("(Level [0-9])_[A-Za-z ]* [0-9]{4}.*", "\\1", y) == "Level 2") {
    lvl1 <- fullnames[which(grepl(paste0("Level 1_", gear), fullnames))]
    if (any(as.integer(gsub("Level [0-9]_[A-Za-z ]* ([0-9]{4}).*", "\\1", lvl1)) == yr)) {
      index <- which(as.integer(gsub("Level [0-9]_[A-Za-z ]* ([0-9]{4}).*", "\\1", lvl1)) == yr)
    } else {
      index <- which(as.integer(gsub("Level [0-9]_[A-Za-z ]* ([0-9]{4}).*", "\\1", lvl1)) > yr)
    }
    level1 <- lvl1[index]
    print(nrow(x))
    ref <- dups[[which(fullnames == level1)]]
    names(ref) <- toupper(names(ref))
    
    out <- merge(x, ref, by = names(x)[names(x) %in% c("SAMPLE_ID", "SAMPLNO", "DBO_ES_ST_SAMPLE_SAMPLE_ID")], all=T)
    #if ("SAMPLNO" %in% names(x)) { #this is the longer identifier, use it where you have it
    #  out <- merge(x, ref, by = "SAMPLNO", all = T)
    #} else if (toupper("dbo_es_st_sample_sample_id") %in% names(ref)) {
    #  out <- merge(x, ref, by.x = toupper("dbo_es_st_sample_sample_id"), by.y = toupper("dbo_es_st_sample_sample_id"), all = T)
      #out$SAMPLNO <- out$DBO_ES_ST_SAMPLE_SAMPLE_ID
    #}
    out$GEAR_TYPE <- gear
    
    out[is.na(out$FILENAME.x), ]$FILENAME.x <- out[is.na(out$FILENAME.x), ]$FILENAME.y
    out$filename <- out$FILENAME.x
    out$FILENAME.x <- NULL
    out$FILENAME.y <- NULL
    zerofill <- out[is.na(out$COMMON_NAME), ]
    zerofill$TOTAL_COUNTED <- 0
    zerofill$COMMON_NAME <- sppofinterest
    obs <- out[!is.na(out$COMMON_NAME), ]
    #obs$COMMON_NAME <- fish #why do we need this?
    out <- rbind(obs, zerofill)
    if("GEAR" %in% names(out)) {
      out$GEARTYPE <- datatabs[[1]]$Gear_Code_lookup$Definition[match(as.character(out$GEAR), datatabs[[1]]$Gear_Code_lookup$Value)]
    }
    
    if ("GRID" %in% names(out)) { #Bottom Trawl, Ichthyoplankton, Pelagic Trawl
      out <- merge(out, joined_points, by.x = "GRID", by.y = "GRID_ID", all.x = T) #I'm still not 100% these are the same "grids"
      out$GRID <- NULL
      out$strata <- NULL #this is the one from joined_points, might need where strata is missing? but this is mostly to get area
      #out <- st_drop_geometry(out) do you need to do this?
    } else if (gear == "Beach Seine") {
        out$LOC_CODE <- as.numeric(gsub("[BWSbws ]*([0-9]+)", "\\1", out$SITE_ID))
        out <- merge(out, beachlocs, by.x = "LOC_CODE", by.y = "sta", all.x = T)
        out$LOC_CODE <- NULL
    }
    
    if ("START_LAT_DD" %in% names(out) & !"lat" %in% names(out)) {
      out[is.na(out$START_LAT_SS), ]$START_LAT_SS <- 0
      out[is.na(out$START_LON_SS), ]$START_LON_SS <- 0
      out$lat <- out$START_LAT_DD + (out$START_LAT_MM + as.numeric(out$START_LAT_SS)) / 60
      out$lng <- (out$START_LON_DD + (out$START_LON_MM + as.numeric(out$START_LON_SS)) / 60) * -1
    }
    
    if(gear=="Bottom Trawl" & !any(names(out)=="STRATA")) {
      matchup <- out[!is.na(out$lat),] #notice this is all that gets kept, should records w/o lat get folded back in?
      matchup <- st_as_sf(matchup, coords=c("lng", "lat"), crs = "+proj=longlat +datum=WGS84")
      joinedpoints <- st_join(matchup, strata, join = st_intersects)
      names(joinedpoints)[names(joinedpoints)=="strata"] <- "STRATA"
      out <- st_drop_geometry(joinedpoints)
    }
    
    environ <- out[!duplicated(out$SAMPLE_ID),]
    environ$TOTAL_COUNTED <- NULL
    out$TOTAL_COUNTED <- as.integer(out$TOTAL_COUNTED)
    catch <- out %>% select(SAMPLE_ID, COMMON_NAME, TOTAL_COUNTED) %>% group_by(SAMPLE_ID, COMMON_NAME) %>% #I broke this, it must be changing this here...check type is char?
      summarise(TOTAL_COUNTED = sum(TOTAL_COUNTED)) #this sums up over fish groups
    out <- merge(environ, catch)
  } else {
    out <- data.frame()
  }
  return(out)
}, turtle, names(turtle))
lvl2 <- lvl2[sapply(lvl2, nrow) > 0]

lvl3 <- Map(function(x, y) {
  print(y)
  gear <- gsub("Level [0-9]_([A-Za-z ]*) [0-9]{4}.*", "\\1", y)
  yr <- as.integer(gsub("Level [0-9]_[A-Za-z ]* ([0-9]{4}).*", "\\1", y))
  names(x) <- toupper(names(x))

  if (gsub("(Level [0-9])_[A-Za-z ]* [0-9]{4}.*", "\\1", y) == "Level 3") {
    meldin <- grepl(paste0("Level 2_", gear, " ", yr), names(lvl2)) # take a value missing from level 2 data and see if it's anywhere in any level 3 data
    if (any(meldin)) {
      # print(meldin)
      lvl3 <- lvl2[[which(meldin)]]
      #names(lvl3) <- toupper(names(lvl3))
      commonfields <- names(x)[names(x) %in% names(lvl3)]
      commonfields <- commonfields[!commonfields %in% c("FILENAME", "NODC")]
      # print(nrow(x))
      out <- merge(x, lvl3, by = commonfields, all.x = T)
    }
    
    #if (!"SAMPLNO" %in% names(out)) {
    #  out$SAMPLNO <- out$DBO_ES_ST_SAMPLE_SAMPLE_ID
    #  out$DBO_ES_ST_SAMPLE_SAMPLE_ID <- NULL
    #}
    
    if (!"CNT_LVL3" %in% names(out)) {
      out$CNT_LVL3 <- 1
    }

    summary_data <- out %>%
      # filter(LENGTH_MEASURE_TECHNIQUE_CODE != "TL") %>%
      group_by(SAMPLE_ID, TOTAL_COUNTED) %>%
      summarise(ct = sum(CNT_LVL3), .groups = "drop")
    summary_data$expand <- summary_data$TOTAL_COUNTED / summary_data$ct
    out$expand <- summary_data$expand[match(out$SAMPLE_ID, summary_data$SAMPLE_ID)]
    out$expandct <- out$CNT_LVL3 * out$expand
    if ("UNIT_MEASURE_WEIGHT_CODE" %in% names(out)) {
      out$wt <- datatabs[[1]]$Unit_Measure_Weight_lookup$Definition[match(out$UNIT_MEASURE_WEIGHT_CODE, datatabs[[1]]$Unit_Measure_Weight_lookup$Value)]
    }
    out$filename <- out$FILENAME.x
    out$GEAR_TYPE <- gear
  } else {
    out <- data.frame()
  }
  return(out)
}, turtle, names(turtle))
lvl3 <- lvl3[sapply(lvl3, nrow) > 0]
# 95081009100026420
# [1] "Level 2_Beach Seine 1995_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Beach Seine/Beach Seine 1995.accdb" GPS_GROUP_ID (do same extract as next)
# [2] "Level 2_Beach Seine 2004_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Beach Seine/Beach Seine 2004.accdb" GPS_GROUP_ID
# [3] "Level 2_Bottom Trawl 1995_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Bottom Trawl/Bottom Trawl 1995.accdb"  GRID
# [4] "Level 2_Bottom Trawl 2004_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Bottom Trawl/Bottom Trawl 2004.accdb"  GPS_GROUP_ID (but lat/lng in dataset)
# [5] "Level 2_Entrainment 1995_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Entrainment/Entrainment 1995.accdb"  GPS_GROUP_ID
# [6] "Level 2_Entrainment 2005_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Entrainment/Entrainment 2005.accdb"  SITE_ID
# [7] "Level 2_Ichthyoplankton 2002_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Ichthyoplankton/Ichthyoplankton 2002.accdb" GRID
# [8] "Level 2_Impingement 1995_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Impingement/Impingement 1995.accdb"  GPS_GROUP_ID
# [9] "Level 2_Impingement 2004_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Impingement/Impingement 2004.accdb"  SITE_ID
# [10] "Level 2_Pelagic Trawl 2002_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Pelagic Trawl/Pelagic Trawl 2002.accdb"  GRID
# [11] "Level 3_Beach Seine 1995_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Beach Seine/Beach Seine 1995.accdb"
# [12] "Level 3_Beach Seine 2004_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Beach Seine/Beach Seine 2004.accdb"
# [13] "Level 3_Bottom Trawl 1995_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Bottom Trawl/Bottom Trawl 1995.accdb"
# [14] "Level 3_Bottom Trawl 2004_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Bottom Trawl/Bottom Trawl 2004.accdb"
# [15] "Level 3_Entrainment 1995_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Entrainment/Entrainment 1995.accdb"
# [16] "Level 3_Entrainment 2005_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Entrainment/Entrainment 2005.accdb"
# [17] "Level 3_Ichthyoplankton 2002_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Ichthyoplankton/Ichthyoplankton 2002.accdb"
# [18] "Level 3_Impingement 1995_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Impingement/Impingement 1995.accdb"
# [19] "Level 3_Impingement 2004_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Impingement/Impingement 2004.accdb"
# [20] "Level 3_Pelagic Trawl 2002_V:/Marine Fish/Jeff B/PSEG data/PSEG/rawdata/PSEG/Pelagic Trawl/Pelagic Trawl 2002.accdb"

# "GPS_Group_Indicator_ID_lookup"
# "GPS_Point_ID_lookup" potentially needs some spacing cleanup
# "Survey_Method_Code_lookup"
# "GPS_Group_ID_lookup" can be used with even coding for alternate sites
# ...as with the other tables in the data dictionary, Value is the lookup key...but I'm not sure what to do with duplicated Values...
# $Length_Measurement_Code_lookup
# Variable Value Definition
# 1 Length Measurement Code     1 millimeter
# 2 Length Measurement Code     2 centimeter
# 3 Length Measurement Code     3      meter
# 4 Length Measurement Code     8     inches
# 5 Length Measurement Code     9       feet
# $Length_Measurement_Technique_Code_lookup
datatabs[[1]]$GPS_Group_ID_lookup <- datatabs[[1]]$GPS_Group_ID_lookup[-which(datatabs[[1]]$GPS_Group_ID_lookup$Value %in% c("BZ- 1", "CP", "CRL", "EAST INT", "LOW OFFMAL", "UPPERDITCH", "UPPERDUCK", "WEST INT", "SNC1A", "glp5", "TEST", "crl5", "LOW  LEFT")), ]

# who knows if there are actually lookup values hiding in site_id?
findcode <- function(x) {
  x[which(apply(x, 1, function(y) any(grepl("SNC1A", y)))), ]
}
# checkcode <- lapply(dups, findcode)
# checkcode <- checkcode[sapply(checkcode, nrow)>0]
datatabs[[1]]$GPS_Group_ID_lookup[datatabs[[1]]$GPS_Group_ID_lookup$Value == "SNGS", ]$Value <- datatabs[[1]]$GPS_Group_ID_lookup[datatabs[[1]]$GPS_Group_ID_lookup$Value == "SNGS", ]$site_id
datatabs[[1]]$GPS_Group_ID_lookup$Value <- gsub(" ", "", toupper(datatabs[[1]]$GPS_Group_ID_lookup$Value))
datatabs[[1]]$Unit_Measure_Weight_lookup$Value <- as.integer(datatabs[[1]]$Unit_Measure_Weight_lookup$Value)

# pull relevant info (e.g. gear, date, location, species, size)
# This way, we only need to know the names for the important fields, and it should be easy enough to standardize the names before joining them.
#lvl2 <- lapply(lvl2, function(x) {
#  x <- x[, names(x)[names(x) %in%
#                      c(toupper(c(
#                        "common_name", "SAMPLNO", "gear", "gear_type", "geartype",
#                        "collection_date", "DATE", "COL_DATE",
#                        "total_counted", "CNT_LVL3", "ind_fish_id", "subsample_yn",
#                        "total_weight", "maximum_weight", "minimum_weight",
#                        "unit_measure_weight_code", "wt",
#                        "individual_length", "length_measure_technique_code", "LEN_METH",
#                        "STRATA", "REGION", "STATE", "LOC_CODE",
#                        "gps_group_id", "gps_point_id", "GRID", "location_id", "site_id",
#                        "siteid",
#                        "START_lat_dd", "START_lat_mm", "START_lat_ss", "START_lon_dd", "START_lon_mm",
#                        "START_lon_ss", "start_lat_dd", "start_lat_mm", "start_lat_ss",
#                      "start_lon_dd", "start_lon_mm", "start_lon_ss",
#                        "filename", "DUR", "STRATA"
#                      )), "FILENAME.x", "FILENAME.y", "filename", "siteid", "lat", "lng")]]
#  return(x)
#})

# fields I made: gear_type, geartype, site, filename
if (any(grepl("Level 2_Impingement 1995", names(lvl2)))) {
  impinge <- which(grepl("Level 2_Impingement 1995", names(lvl2)))
  lvl2[[impinge]]$site <- datatabs[[1]]$Impingement_Location_Code_lookup$Definition[match(lvl2[[impinge]]$SITE_ID, datatabs[[1]]$Impingement_Location_Code_lookup$Value)]
  lvl2[[impinge]]$notes <- ""
  lvl2[[impinge]]$notes[is.na(lvl2[[impinge]]$site)] <- "assuming convention elsewhere in data, alternative site indicated with A"
  lvl2[[impinge]]$site[is.na(lvl2[[impinge]]$site)] <- "SNGS Circulating Water 1, North"
  # your custom fields for aligning the data
  # [[1]]$Unit_Measure_Weight_lookup
}

if (any(grepl("Level 2_Impingement 2004", names(lvl2)))) {
  impinge <- which(grepl("Level 2_Impingement 2004", names(lvl2)))
  lvl2[[impinge]]$site <- datatabs[[1]]$Impingement_Location_Code_lookup$Definition[match(lvl2[[impinge]]$SITE_ID, datatabs[[1]]$Impingement_Location_Code_lookup$Value)]
  lvl2[[impinge]]$notes <- ""
  lvl2[[impinge]]$notes[is.na(lvl2[[impinge]]$site)] <- "assuming convention elsewhere in data, alternative site indicated with A"
  lvl2[[impinge]]$site[is.na(lvl2[[impinge]]$site)] <- "SNGS Circulating Water 1, North"
  # your custom fields for aligning the data
  # [[1]]$Unit_Measure_Weight_lookup
}

lvl2[[4]]$site <- datatabs[[1]]$Site_ID_lookup$Definition[match(lvl2[[4]]$SITE_ID, datatabs[[1]]$Site_ID_lookup$Value)]
# For the beach seine, station locations are shown in the map below. At the very least we can approximate lat/long from the map,
# or if we want to be more specific, we may be able to get the exact locations from PSEG.
# Do you know if the old LOC_CODEs match up with the more recent gps_group_id?
# Hopefully yes, but if not, we’ll need to figure out how those relate.
lvl2[[1]]$gps_group_indicator_id <- datatabs[[1]]$GPS_Group_ID_lookup$gps_group_indicator_id[match(lvl2[[1]]$SITE_ID, datatabs[[1]]$GPS_Group_ID_lookup$Value)]
lvl2[[2]]$gps_group_indicator_id <- datatabs[[1]]$GPS_Group_ID_lookup$gps_group_indicator_id[match(lvl2[[2]]$SITE_ID, datatabs[[1]]$GPS_Group_ID_lookup$Value)]

# lvl2[[3]][is.na(lvl2[[3]]$CNT_LVL3), ]$CNT_LVL3 <- lvl2[[3]][is.na(lvl2[[3]]$CNT_LVL3), ]$TOTAL_COUNTED
# lvl2[[3]]$count <- lvl2[[3]]$CNT_LVL3
# Maybe also total number of stations conducted for the year (whether sturgeon/turtle were caught or not), to get an estimate of overall interaction frequency
# [[1]]$Weight_Measurement_Code_lookup

# "MAXIMUM_WEIGHT", "MINIMUM_WEIGHT",
lvl2s <- lapply(lvl2, function(x) {
  x <- x[, names(x)[!names(x) %in% c( "END_LAT_DD", "END_LAT_MM", "END_LON_DD",
                                      "END_LON_MM"
  )]]
  return(x)
})

outdata <- lapply(1:length(lvl2s), function(a) {
  if((a+1) < length(lvl2s) && lvl2s[[a+1]]$GEAR_TYPE[1] == lvl2s[[a]]$GEAR_TYPE[1]) {
    b <- bind_rows(lvl2s[[a]], lvl2s[[a+1]])
    b <- b[,!grepl("y",names(b))]
    #b$COLLECTION_DATE <- format(b$COLLECTION_DATE, "%Y-%m-%d %H:%M:%S")
    lvl2data <- b[order(b$DATE), ]
  } else if (a-1 >0 && lvl2s[[a-1]]$GEAR_TYPE[1] == lvl2s[[a]]$GEAR_TYPE[1]) {
    lvl2data <- data.frame()
  } else {
    b <- lvl2s[[a]]
    b <- b[,!grepl("y",names(b))]
    #b$COLLECTION_DATE <- format(b$COLLECTION_DATE, "%Y-%m-%d %H:%M:%S")
    lvl2data <- b[order(b$DATE), ]
  }
return(lvl2data)})
outdata <- outdata[sapply(outdata, nrow) > 0]
lapply(outdata, function(d) {
  d <- d[, colSums(!is.na(d)) > 0]
  write.csv(d, file = file.path(root, paste0(sppofinterest, "-catch_", d$GEAR_TYPE[1], ".csv")), row.names = F)
})

#outdata <- bind_rows(lvl2s)
#outdata <- outdata[, c(
#  "GEAR_TYPE", "GEARTYPE", "COLLECTION_DATE", "SAMPLNO", "DUR", "STRATA",
#  "siteid", "lat", "lng", "COMMON_NAME", "TOTAL_COUNTED",
#  # "INDIVIDUAL_LENGTH", "LENGTH_MEASURE_TECHNIQUE_CODE",
#  # "TOTAL_WEIGHT", "wt",
#  "filename"
#)]
#lvl2data <- outdata[order(outdata$GEAR_TYPE, outdata$COLLECTION_DATE), ]
#lvl2data$COLLECTION_DATE <- format(lvl2data$COLLECTION_DATE, "%Y-%m-%d %H:%M:%S")
# can you match ichthyoplankton with grid? pelagic trawl?
# write.csv(lvl2data, "~/output/stripedbass_catch-ALL.csv", row.names = F)

outdata3 <- lapply(lvl3, function(x) {
  x <- x[, names(x)[names(x) %in% c(
    "SAMPLE_ID", "CNT_LVL3", 
    "COMMON_NAME", "expand", "filename",
    "expandct", "GEAR_TYPE",
    "TOTAL_WEIGHT", "LENGTH", "wt", "TOTAL_COUNTED",
    "LENGTH_MEASURE_TECHNIQUE_CODE"
  )]]
  return(x)
})
outdata3 <- bind_rows(outdata3)
#outdata[is.na(outdata$LENGTH), ]$LENGTH <- outdata[is.na(outdata$LENGTH), ]$INDIVIDUAL_LENGTH
#outdata$INDIVIDUAL_LENGTH <- NULL

mismatch <- outdata3[outdata3$expand < 1 & !is.na(outdata3$expand), ]
good <- outdata3[outdata3$expand >= 1 & !is.na(outdata3$expand), ]
write.csv(outdata3, file.path(root, paste0(sppofinterest,"_length.csv")), row.names = F)

# EDA
summary_data <- mismatch %>%
  filter(LENGTH_MEASURE_TECHNIQUE_CODE != "TL") %>%
  group_by(SAMPLE_ID, TOTAL_COUNTED) %>%
  summarise(ct = sum(CNT_LVL3), .groups = "drop")
summary_data$expand <- summary_data$TOTAL_COUNTED / summary_data$ct
# out$expand <- summary_data$expand[match(out$SAMPLNO, summary_data$SAMPLNO)]
# out$expandct <- out$CNT_LVL3 * out$expand
#"000035144" "000049376" "000056791" "000066069" "000095240" "000095251" "000095260" "000096096"

#to do: make sure no NA sample ids, check to see if sample IDs are unique ACROSS survey types? (remember fixed or created ids too)
