# dups[[11:31]] has species name and variably: nodc code, species, SPINDEX
# com_name, common_name, COM_NAME

# scan for sturgeon (Atlantic and short nose) and turtle (all species) records and join only those records
# source("2-pseg_datafix.R")
load("~/data/PSEG_Data_compiled.RData")
library(sf)
library(dplyr)

fullnames <- gsub("(Level [0-9]_[A-Za-z ]* [0-9]{4}).*", "\\1", names(dups))
gears <- unique(sapply(fullnames, function(y) gsub("Level [0-9]_([A-Za-z ]*) [0-9]{4}.*", "\\1", y)))
samples <- sapply(gears, function(x) {
  sum(sapply(dups[which(grepl(paste0("Level 1_", x), fullnames))], nrow))
})

getspp <- function(x) {
  if (any(names(x) %in% c("COMMON_NAME", "COM_NAME", "common_name", "com_name"))) {
    x <- x[which(grepl("STRIPED BASS", x[, which(names(x) %in% c("COMMON_NAME", "COM_NAME", "common_name", "com_name"))])), ] # "STRIPED BASS STURGEON|TURTLE
  } else {
    x <- data.frame()
  }
  return(x)
}
turtle <- lapply(dups, getspp)
turtle <- turtle[sapply(turtle, nrow) > 0]

load("~/data/pseg_dict.RData")
beachlocs <- read.csv("~/data/PSEG 2025 Beach Seine Locations.csv")
coords <- apply(beachlocs[, 2:3], 2, strsplit, " ")
deg <- apply(as.data.frame(lapply(coords, function(x) unlist(lapply(x, "[[", 1)))), 2, as.numeric)
min <- apply(as.data.frame(lapply(coords, function(x) unlist(lapply(x, "[[", 2)))), 2, as.numeric) / 60
beachlocs <- as.data.frame(deg + min)
beachlocs$sta <- 1:nrow(beachlocs)
beachlocs$lng <- beachlocs$lng * -1

trawllocs <- read.csv("~/data/PSEG Bottom Trawl Locations.csv")[, 1:3]
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
lvl2 <- Map(function(x, y) {
  gear <- gsub("Level [0-9]_([A-Za-z ]*) [0-9]{4}.*", "\\1", y)
  yr <- as.integer(gsub("Level [0-9]_[A-Za-z ]* ([0-9]{4}).*", "\\1", y))
  fish <- x$COMMON_NAME[1]
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

    if ("DBO_ES_ST_SAMPLE_SAMPLE_ID" %in% names(ref) && "SAMPLE_ID" %in% names(ref)) {
      ref[is.na(ref$SAMPLE_ID), ]$SAMPLE_ID <- ref[is.na(ref$SAMPLE_ID), ]$DBO_ES_ST_SAMPLE_SAMPLE_ID
    }

    if ("SAMPLNO" %in% names(x)) { # change to logic in meldin test where you merge by common fields
      out <- merge(x, ref, by = "SAMPLNO", all = T)
      out$SAMPLE_ID <- NULL
    } else if ("SAMPLE_ID" %in% names(ref)) {
      x$COLLECTION_DATE <- NULL
      out <- merge(x, ref, by = "SAMPLE_ID", all = T) # ...test instead for this case of needing to match on different ids
      out$SAMPLNO <- out$SAMPLE_ID
      out$SAMPLE_ID <- NULL
    } else if (toupper("dbo_es_st_sample_sample_id") %in% names(ref)) {
      out <- merge(x, ref, by.x = "SAMPLE_ID", by.y = toupper("dbo_es_st_sample_sample_id"), all = T)
      out$SAMPLNO <- out$SAMPLE_ID
      out$SAMPLE_ID <- NULL
    }

    out$GEAR_TYPE <- gear

    if ("GEAR_CODE" %in% names(out)) {
      out$GEAR <- out$GEAR_CODE
      out$GEAR_CODE <- NULL
    } else if ("GEARCHAR" %in% names(out)) {
      out$GEAR <- out$GEARCHAR
      out$GEARCHAR <- NULL
    }

    out[is.na(out$FILENAME.x), ]$FILENAME.x <- out[is.na(out$FILENAME.x), ]$FILENAME.y
    out$filename <- out$FILENAME.x
    out[is.na(out$COMMON_NAME), ]$TOTAL_COUNTED <- 0
    out$COMMON_NAME <- fish
    if ("GEAR" %in% names(out)) {
      out$GEAR <- as.integer(out$GEAR)
      out$GEARTYPE <- datatabs[[1]]$Gear_Code_lookup$Definition[match(as.character(out$GEAR), datatabs[[1]]$Gear_Code_lookup$Value)]
    }

    if (!"COLLECTION_DATE" %in% names(out)) {
      if ("COL_DATE" %in% names(out)) {
        out$COLLECTION_DATE <- out$COL_DATE
      } else {
        out$COLLECTION_DATE <- out$DATE
      }
    }
    out$COLLECTION_DATE <- as.POSIXct(out$COLLECTION_DATE)

    if ("GRID" %in% names(out)) {
      out <- merge(out, df, by.x = "GRID", by.y = "GRID_ID", all.x = T)
      out$siteid <- out$GRID
      # out$GRID <- NULL
    } else if ("GPS_GROUP_ID" %in% names(out)) {
      if (gear == "Beach Seine") {
        out$LOC_CODE <- as.numeric(gsub("[BWSbws ]*([0-9]+)", "\\1", out$GPS_GROUP_ID))
        out <- merge(out, beachlocs, by.x = "LOC_CODE", by.y = "sta", all.x = T)
        out$LOC_CODE <- NULL
      }
      out$siteid <- out$GPS_GROUP_ID
      # out$GPS_GROUP_ID <- NULL
    } else {
      out$siteid <- out$SITE_ID
      # out$SITE_ID <- NULL
    }
    out$siteid <- as.character(out$siteid)

    if ("START_LAT_DD" %in% names(out) & !"lat" %in% names(out)) {
      out[is.na(out$START_LAT_SS), ]$START_LAT_SS <- 0
      out[is.na(out$START_LON_SS), ]$START_LON_SS <- 0
      out$lat <- out$START_LAT_DD + (out$START_LAT_MM + as.numeric(out$START_LAT_SS)) / 60
      out$lng <- (out$START_LON_DD + (out$START_LON_MM + as.numeric(out$START_LON_SS)) / 60) * -1
    }
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
  if (gsub("(Level [0-9])_[A-Za-z ]* [0-9]{4}.*", "\\1", y) == "Level 3") {
    meldin <- grepl(paste0("Level 2_", gear, " ", yr), names(turtle)) # take a value missing from level 2 data and see if it's anywhere in any level 3 data
    if (any(meldin)) {
      # print(meldin)
      lvl3 <- turtle[[which(meldin)]]
      names(lvl3) <- toupper(names(lvl3))
      commonfields <- names(x)[names(x) %in% names(lvl3)]
      commonfields <- commonfields[!commonfields %in% c("FILENAME", "NODC_CODE")]
      # print(nrow(x))
      out <- merge(x, lvl3, by = commonfields, all.x = T)
    }
    if (!"SAMPLNO" %in% names(out) & "SAMPLE_ID" %in% names(out)) {
      out$SAMPLNO <- out$SAMPLE_ID
      out$SAMPLE_ID <- NULL
    }
    if (!"CNT_LVL3" %in% names(out)) {
      out$CNT_LVL3 <- 1
    }
    if ("CNT_LVL2" %in% names(out)) {
      out$TOTAL_COUNTED <- out$CNT_LVL2
      out$CNT_LVL2 <- NULL
    }
    if (!"LENGTH_MEASURE_TECHNIQUE_CODE" %in% names(out)) {
      out$LENGTH_MEASURE_TECHNIQUE_CODE <- "U"
    }
    summary_data <- out %>%
      # filter(LENGTH_MEASURE_TECHNIQUE_CODE != "TL") %>%
      group_by(SAMPLNO, TOTAL_COUNTED) %>%
      summarise(ct = sum(CNT_LVL3), .groups = "drop")
    summary_data$expand <- summary_data$TOTAL_COUNTED / summary_data$ct
    out$expand <- summary_data$expand[match(out$SAMPLNO, summary_data$SAMPLNO)]
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
# [1] "Level 2_Beach Seine 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Beach Seine/Beach Seine 1995.accdb" GPS_GROUP_ID (do same extract as next)
# [2] "Level 2_Beach Seine 2004_V:/Marine Fish/Jeff B/PSEG data/rawdata/Beach Seine/Beach Seine 2004.accdb" GPS_GROUP_ID
# [3] "Level 2_Bottom Trawl 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Bottom Trawl/Bottom Trawl 1995.accdb"  GRID
# [4] "Level 2_Bottom Trawl 2004_V:/Marine Fish/Jeff B/PSEG data/rawdata/Bottom Trawl/Bottom Trawl 2004.accdb"  GPS_GROUP_ID (but lat/lng in dataset)
# [5] "Level 2_Entrainment 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Entrainment/Entrainment 1995.accdb"  GPS_GROUP_ID
# [6] "Level 2_Entrainment 2005_V:/Marine Fish/Jeff B/PSEG data/rawdata/Entrainment/Entrainment 2005.accdb"  SITE_ID
# [7] "Level 2_Ichthyoplankton 2002_V:/Marine Fish/Jeff B/PSEG data/rawdata/Ichthyoplankton/Ichthyoplankton 2002.accdb" GRID
# [8] "Level 2_Impingement 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Impingement/Impingement 1995.accdb"  GPS_GROUP_ID
# [9] "Level 2_Impingement 2004_V:/Marine Fish/Jeff B/PSEG data/rawdata/Impingement/Impingement 2004.accdb"  SITE_ID
# [10] "Level 2_Pelagic Trawl 2002_V:/Marine Fish/Jeff B/PSEG data/rawdata/Pelagic Trawl/Pelagic Trawl 2002.accdb"  GRID
# [11] "Level 3_Beach Seine 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Beach Seine/Beach Seine 1995.accdb"
# [12] "Level 3_Beach Seine 2004_V:/Marine Fish/Jeff B/PSEG data/rawdata/Beach Seine/Beach Seine 2004.accdb"
# [13] "Level 3_Bottom Trawl 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Bottom Trawl/Bottom Trawl 1995.accdb"
# [14] "Level 3_Bottom Trawl 2004_V:/Marine Fish/Jeff B/PSEG data/rawdata/Bottom Trawl/Bottom Trawl 2004.accdb"
# [15] "Level 3_Entrainment 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Entrainment/Entrainment 1995.accdb"
# [16] "Level 3_Entrainment 2005_V:/Marine Fish/Jeff B/PSEG data/rawdata/Entrainment/Entrainment 2005.accdb"
# [17] "Level 3_Ichthyoplankton 2002_V:/Marine Fish/Jeff B/PSEG data/rawdata/Ichthyoplankton/Ichthyoplankton 2002.accdb"
# [18] "Level 3_Impingement 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Impingement/Impingement 1995.accdb"
# [19] "Level 3_Impingement 2004_V:/Marine Fish/Jeff B/PSEG data/rawdata/Impingement/Impingement 2004.accdb"
# [20] "Level 3_Pelagic Trawl 2002_V:/Marine Fish/Jeff B/PSEG data/rawdata/Pelagic Trawl/Pelagic Trawl 2002.accdb"

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
sturgeonturtle <- lapply(lvl2, function(x) {
  x <- x[, names(x)[names(x) %in%
    c(toupper(c(
      "common_name", "SAMPLNO", "gear", "gear_type", "geartype",
      "collection_date", "DATE", "COL_DATE",
      "total_counted", "CNT_LVL3", "ind_fish_id", "subsample_yn",
      "total_weight", "maximum_weight", "minimum_weight",
      "unit_measure_weight_code", "wt",
      "individual_length", "length_measure_technique_code", "LEN_METH",
      "STRATA", "REGION", "STATE", "LOC_CODE",
      "gps_group_id", "gps_point_id", "GRID", "location_id", "site_id",
      "siteid",
      "START_lat_dd", "START_lat_mm", "START_lat_ss", "START_lon_dd", "START_lon_mm",
      "START_lon_ss", "start_lat_dd", "start_lat_mm", "start_lat_ss",
      "start_lon_dd", "start_lon_mm", "start_lon_ss",
      "filename", "DUR", "STRATA"
    )), "FILENAME.x", "FILENAME.y", "filename", "siteid", "lat", "lng")]]
  return(x)
})

# fields I made: gear_type, geartype, site, filename
if (any(grepl("Level 2_Impingement 1995", names(sturgeonturtle)))) {
  impinge <- which(grepl("Level 2_Impingement 1995", names(sturgeonturtle)))
  sturgeonturtle[[impinge]]$site <- datatabs[[1]]$Impingement_Location_Code_lookup$Definition[match(sturgeonturtle[[impinge]]$LOC_CODE, datatabs[[1]]$Impingement_Location_Code_lookup$Value)]
  sturgeonturtle[[impinge]]$notes <- ""
  sturgeonturtle[[impinge]]$notes[is.na(sturgeonturtle[[impinge]]$site)] <- "assuming convention elsewhere in data, alternative site indicated with A"
  sturgeonturtle[[impinge]]$site[is.na(sturgeonturtle[[impinge]]$site)] <- "SNGS Circulating Water 1, North"
  # your custom fields for aligning the data
  # [[1]]$Unit_Measure_Weight_lookup
}

if (any(grepl("Level 2_Impingement 2004", names(sturgeonturtle)))) {
  impinge <- which(grepl("Level 2_Impingement 2004", names(sturgeonturtle)))
  sturgeonturtle[[impinge]]$site <- datatabs[[1]]$Impingement_Location_Code_lookup$Definition[match(sturgeonturtle[[impinge]]$SITE_ID, datatabs[[1]]$Impingement_Location_Code_lookup$Value)]
  sturgeonturtle[[impinge]]$notes <- ""
  sturgeonturtle[[impinge]]$notes[is.na(sturgeonturtle[[impinge]]$site)] <- "assuming convention elsewhere in data, alternative site indicated with A"
  sturgeonturtle[[impinge]]$site[is.na(sturgeonturtle[[impinge]]$site)] <- "SNGS Circulating Water 1, North"
  # your custom fields for aligning the data
  # [[1]]$Unit_Measure_Weight_lookup
}

sturgeonturtle[[4]]$site <- datatabs[[1]]$Site_ID_lookup$Definition[match(sturgeonturtle[[4]]$SITE_ID, datatabs[[1]]$Site_ID_lookup$Value)]
# For the beach seine, station locations are shown in the map below. At the very least we can approximate lat/long from the map,
# or if we want to be more specific, we may be able to get the exact locations from PSEG.
# Do you know if the old LOC_CODEs match up with the more recent gps_group_id?
# Hopefully yes, but if not, we’ll need to figure out how those relate.
sturgeonturtle[[1]]$gps_group_indicator_id <- datatabs[[1]]$GPS_Group_ID_lookup$gps_group_indicator_id[match(sturgeonturtle[[1]]$GPS_GROUP_ID, datatabs[[1]]$GPS_Group_ID_lookup$Value)]
sturgeonturtle[[2]]$gps_group_indicator_id <- datatabs[[1]]$GPS_Group_ID_lookup$gps_group_indicator_id[match(sturgeonturtle[[2]]$GPS_GROUP_ID, datatabs[[1]]$GPS_Group_ID_lookup$Value)]

# sturgeonturtle[[3]][is.na(sturgeonturtle[[3]]$CNT_LVL3), ]$CNT_LVL3 <- sturgeonturtle[[3]][is.na(sturgeonturtle[[3]]$CNT_LVL3), ]$TOTAL_COUNTED
# sturgeonturtle[[3]]$count <- sturgeonturtle[[3]]$CNT_LVL3
# Maybe also total number of stations conducted for the year (whether sturgeon/turtle were caught or not), to get an estimate of overall interaction frequency
# [[1]]$Weight_Measurement_Code_lookup

# "MAXIMUM_WEIGHT", "MINIMUM_WEIGHT",
sturgeonturtles <- lapply(sturgeonturtle, function(x) {
  x <- x[, names(x)[names(x) %in% c(
    "COLLECTION_DATE", "lat", "lng", "siteid", "GEARTYPE", "GEAR_TYPE", "COMMON_NAME", "TOTAL_COUNTED",
    "TOTAL_WEIGHT", "INDIVIDUAL_LENGTH", "wt", "DUR", "STRATA",
    "LENGTH_MEASURE_TECHNIQUE_CODE", "SAMPLNO", "filename"
  )]]
  return(x)
})
outdata <- bind_rows(sturgeonturtles)
outdata <- outdata[, c(
  "GEAR_TYPE", "GEARTYPE", "COLLECTION_DATE", "SAMPLNO", "DUR", "STRATA",
  "siteid", "lat", "lng", "COMMON_NAME", "TOTAL_COUNTED",
  # "INDIVIDUAL_LENGTH", "LENGTH_MEASURE_TECHNIQUE_CODE",
  # "TOTAL_WEIGHT", "wt",
  "filename"
)]
lvl2data <- outdata[order(outdata$GEAR_TYPE, outdata$COLLECTION_DATE), ]
lvl2data$COLLECTION_DATE <- format(lvl2data$COLLECTION_DATE, "%Y-%m-%d %H:%M:%S")
# can you match ichthyoplankton with grid? pelagic trawl?
# write.csv(lvl2data, "~/output/stripedbass_catch.csv", row.names = F)

outdata <- lapply(lvl3, function(x) {
  x <- x[, names(x)[names(x) %in% c(
    "SAMPLNO", "CNT_LVL3", "LENGTH",
    "COMMON_NAME", "expand", "filename",
    "expandct", "GEAR_TYPE",
    "TOTAL_WEIGHT", "INDIVIDUAL_LENGTH", "wt", "TOTAL_COUNTED",
    "LENGTH_MEASURE_TECHNIQUE_CODE"
  )]]
  return(x)
})
outdata <- bind_rows(outdata)
outdata[is.na(outdata$LENGTH), ]$LENGTH <- outdata[is.na(outdata$LENGTH), ]$INDIVIDUAL_LENGTH
outdata$INDIVIDUAL_LENGTH <- NULL

mismatch <- outdata[outdata$expand < 1 & !is.na(outdata$expand), ]
good <- outdata[outdata$expand >= 1 & !is.na(outdata$expand), ]
# write.csv(outdata, "~/output/stripedbass_length.csv", row.names = F)

# EDA
summary_data <- mismatch %>%
  filter(LENGTH_MEASURE_TECHNIQUE_CODE != "TL") %>%
  group_by(SAMPLNO, TOTAL_COUNTED) %>%
  summarise(ct = sum(CNT_LVL3), .groups = "drop")
summary_data$expand <- summary_data$TOTAL_COUNTED / summary_data$ct
# out$expand <- summary_data$expand[match(out$SAMPLNO, summary_data$SAMPLNO)]
# out$expandct <- out$CNT_LVL3 * out$expand
