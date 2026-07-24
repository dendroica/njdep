myroot <- Sys.getenv("FILEPATH")
mypath <- file.path("./pseg/1-pseg_data.R")
#"~/code/pseg/1-pseg_data.R"
source(mypath)
library(dplyr)

dups[[1]]$col_date <- NULL
# these are the changes instructed in Alissa's excel sheet: Data tracking for PSEG Baywide Seine Survey
dups[[1]][dups[[1]]$SAMPLNO == "9608291135006A420", ]$LOC_CODE <- "0006"
dups[[1]][dups[[1]]$SAMPLNO == "9609110805006A420", ]$LOC_CODE <- "0006"
dups[[1]][dups[[1]]$SAMPLNO == "9609260845006A420", ]$LOC_CODE <- "0006"
dups[[1]][dups[[1]]$SAMPLNO == "9610130945006A420", ]$LOC_CODE <- "0006"
dups[[1]][dups[[1]]$SAMPLNO == "9610290946006A420", ]$LOC_CODE <- "0006"
dups[[1]][dups[[1]]$SAMPLNO == "96101311130005420", ]$LOC_CODE <- "0034"
dups[[1]][dups[[1]]$SAMPLNO == "97080511380014420", ]$LOC_CODE <- "0013"
dups[[1]][dups[[1]]$SAMPLNO == "9708081143006A420", ]$LOC_CODE <- "0006"
dups[[1]][dups[[1]]$SAMPLNO == "9708261545006A420", ]$LOC_CODE <- "0006"
dups[[1]][dups[[1]]$SAMPLNO == "97100512430008420", ]$LOC_CODE <- "0038"
dups[[1]][dups[[1]]$SAMPLNO == "97102314110023420", ]$LOC_CODE <- "0024"
dups[[1]][dups[[1]]$SAMPLNO == "98092812015005420", ]$LOC_CODE <- "0005"

dups[[1]][dups[[1]]$SAMPLNO == "9608291135006A420", ]$SAMPLNO <- "96082911350006420"
dups[[1]][dups[[1]]$SAMPLNO == "9609110805006A420", ]$SAMPLNO <- "96091108050006420"
dups[[1]][dups[[1]]$SAMPLNO == "9609260845006A420", ]$SAMPLNO <- "96092608450006420"
dups[[1]][dups[[1]]$SAMPLNO == "9610130945006A420", ]$SAMPLNO <- "96101309450006420"
dups[[1]][dups[[1]]$SAMPLNO == "9610290946006A420", ]$SAMPLNO <- "96102909460006420"
dups[[1]][dups[[1]]$SAMPLNO == "96101311130005420", ]$SAMPLNO <- "96101311130034420"

dups[[1]][dups[[1]]$SAMPLNO == "97080511380014420", ]$SAMPLNO <- "97080511380014420"
dups[[1]][dups[[1]]$SAMPLNO == "9708081143006A420", ]$SAMPLNO <- "97080811430006420"
dups[[1]][dups[[1]]$SAMPLNO == "9708261545006A420", ]$SAMPLNO <- "97082615450006420"
dups[[1]][dups[[1]]$SAMPLNO == "97100512430008420", ]$SAMPLNO <- "97100512430038420"
dups[[1]][dups[[1]]$SAMPLNO == "97102314110023420", ]$SAMPLNO <- "97102314110024420"

dups[[1]][dups[[1]]$SAMPLNO == "98092812015005420", ]$SAMPLNO <- "98092812010005420"

dups[[2]][4509,]$dbo_es_st_sample_sample_id <- "000071146" #missing sample ID
#filling with what looks like the logical sequence, matches level 2 data

# I'm not sure this should be changed given GPS_Group_ID_lookup table
# dups[[2]][dups[[2]]$sample_description=="082720141019BWS28",]$gps_group_id <- "BWS28" #non-unique sample desc?

dups[[11]][dups[[11]]$SAMPLNO == "9608291135006A420", ]$SAMPLNO <- "96082911350006420"
dups[[11]][dups[[11]]$SAMPLNO == "9609110805006A420", ]$SAMPLNO <- "96091108050006420"
dups[[11]][dups[[11]]$SAMPLNO == "9609260845006A420", ]$SAMPLNO <- "96092608450006420"
dups[[11]][dups[[11]]$SAMPLNO == "9610130945006A420", ]$SAMPLNO <- "96101309450006420"
dups[[11]][dups[[11]]$SAMPLNO == "9610290946006A420", ]$SAMPLNO <- "96102909460006420"
dups[[11]][dups[[11]]$SAMPLNO == "96101311130005420", ]$SAMPLNO <- "96101311130034420"

dups[[11]][dups[[11]]$SAMPLNO == "97080511380014420", ]$SAMPLNO <- "97080511380014420"
dups[[11]][dups[[11]]$SAMPLNO == "9708081143006A420", ]$SAMPLNO <- "97080811430006420"
dups[[11]][dups[[11]]$SAMPLNO == "9708261545006A420", ]$SAMPLNO <- "97082615450006420"
dups[[11]][dups[[11]]$SAMPLNO == "97100512430008420", ]$SAMPLNO <- "97100512430038420"
dups[[11]][dups[[11]]$SAMPLNO == "97102314110023420", ]$SAMPLNO <- "97102314110024420"

lng <- c(
  dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lat_dd,
  dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lat_mm,
  dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lat_ss
)

lat <- c(
  dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lon_dd,
  dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lon_mm,
  dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lon_ss
)

dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lat_dd <- lat[1]
dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lat_mm <- lat[2]
dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lat_ss <- lat[3]

dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lon_dd <- lng[1]
dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lon_mm <- lng[2]
dups[[4]][dups[[4]]$dbo_es_st_sample_sample_id == "000072717", ]$end_lon_ss <- lng[3]

dups[[21]][dups[[21]]$SAMPLNO == "9608291135006A420", ]$SAMPLNO <- "96082911350006420"
dups[[21]][dups[[21]]$SAMPLNO == "9609110805006A420", ]$SAMPLNO <- "96091108050006420"
dups[[21]][dups[[21]]$SAMPLNO == "9609260845006A420", ]$SAMPLNO <- "96092608450006420"
dups[[21]][dups[[21]]$SAMPLNO == "9610130945006A420", ]$SAMPLNO <- "96101309450006420"
dups[[21]][dups[[21]]$SAMPLNO == "9610290946006A420", ]$SAMPLNO <- "96102909460006420"
dups[[21]][dups[[21]]$SAMPLNO == "96101311130005420", ]$SAMPLNO <- "96101311130034420"

dups[[21]][dups[[21]]$SAMPLNO == "97080511380014420", ]$SAMPLNO <- "97080511380014420"
dups[[21]][dups[[21]]$SAMPLNO == "9708081143006A420", ]$SAMPLNO <- "97080811430006420"
dups[[21]][dups[[21]]$SAMPLNO == "9708261545006A420", ]$SAMPLNO <- "97082615450006420"
dups[[21]][dups[[21]]$SAMPLNO == "97100512430008420", ]$SAMPLNO <- "97100512430038420"
dups[[21]][dups[[21]]$SAMPLNO == "97102314110023420", ]$SAMPLNO <- "97102314110024420"

dups[[2]]$comments <- NULL
dups[[2]]$depth_sample_max <- NULL
dups[[2]]$depth_sample_min <- NULL
dups[[2]]$depth_site_max <- NULL
dups[[2]]$depth_site_min <- NULL
dups[[2]]$do_bottom <- NULL
dups[[2]]$do_mid <- NULL
dups[[2]]$start_lat_dd <- NULL
dups[[2]]$start_lat_mm <- NULL
dups[[2]]$start_lat_ss <- NULL
dups[[2]]$start_lon_dd <- NULL
dups[[2]]$start_lon_mm <- NULL
dups[[2]]$start_lon_ss <- NULL
dups[[2]]$end_lat_dd <- NULL
dups[[2]]$end_lat_mm <- NULL
dups[[2]]$end_lat_ss <- NULL
dups[[2]]$end_lon_dd <- NULL
dups[[2]]$end_lon_mm <- NULL
dups[[2]]$end_lon_ss <- NULL
dups[[2]]$end_time <- NULL
dups[[2]]$ph_bottom <- NULL
dups[[2]]$ph_mid <- NULL
dups[[2]]$ph_surface <- as.numeric(dups[[2]]$ph_surface)
dups[[2]]$replicate_event_id <- NULL
dups[[2]]$salinity_bottom <- NULL
dups[[2]]$sample_depth_strata <- NULL
dups[[2]]$temp_water_bottom <- NULL
dups[[2]]$temp_water_mid <- NULL

#dups[[4]]land_use_cover_code only has 0
#dups[[4]]weather_condition_id only has 0
dups[[4]]$bottom_type_code <- NULL
dups[[4]]$region <- dups[[4]]$site_id #can we just drop this since it's only 1 value DE-EST?
dups[[4]]$site_id <- dups[[4]]$gps_point_id
dups[[4]]$strata <- gsub("\\D", "", dups[[4]]$gps_group_id)
dups[[4]]$gps_group_id <- NULL
dups[[4]]$photograph <- NULL
dups[[4]]$replicate_event_id <- NULL
dups[[4]]$volume_filtered1 <- NULL
dups[[4]]$weather_condition_id <- NULL
dups[[4]]$water_body_descr <- NULL
dups[[4]]$depth_site_min <- NULL
dups[[4]][which(!is.na(dups[[4]]$notes)),]$comments <- dups[[4]][which(!is.na(dups[[4]]$notes)),]$notes
dups[[4]]$notes <- dups[[4]]$comments
dups[[4]]$comments <- NULL
dups[[4]]$land_use_cover_code <- NULL

#NEED TO FIX dups[[6]]
#dups[[6]][which(is.na(dups[[6]]$dbo_es_st_sample_sample_id)),] sample_id takes over
#before this, it's that identifier^ flesh out both columns
dups[[6]]$sample_id <- NULL
dups[[6]]$collection_date <- NULL
dups[[6]]$bottom_type_code <- NULL
dups[[6]]$comments <- NULL
dups[[6]]$depth_sample_max <- NULL
dups[[6]]$depth_sample_min <- NULL
dups[[6]]$depth_site_max <- NULL
dups[[6]]$depth_site_min <- NULL
dups[[6]]$do_bottom <- NULL
dups[[6]]$do_mid <- NULL
dups[[6]]$ph_bottom <- NULL
dups[[6]]$ph_mid <- NULL
dups[[6]]$ph_surface <- NULL
dups[[6]]$salinity_bottom <- NULL
dups[[6]]$temp_water_bottom <- NULL
dups[[6]]$temp_water_mid <- NULL
dups[[6]]$end_lat_dd <- NULL
dups[[6]]$end_lat_mm <- NULL
dups[[6]]$end_lat_ss <- NULL
dups[[6]]$end_lon_dd <- NULL
dups[[6]]$end_lon_mm <- NULL
dups[[6]]$end_lon_ss <- NULL
dups[[6]]$start_lat_dd <- NULL
dups[[6]]$start_lat_mm <- NULL
dups[[6]]$start_lat_ss <- NULL
dups[[6]]$start_lon_dd <- NULL
dups[[6]]$start_lon_mm <- NULL
dups[[6]]$start_lon_ss <- NULL
dups[[6]]$sample_volume <- NULL
dups[[6]]$sample_depth_strata <- NULL
dups[[6]]$screen_11a_speed <- NULL
dups[[6]]$screen_11b_speed <- NULL
dups[[6]]$screen_12a_speed <- NULL
dups[[6]]$screen_12b_speed <- NULL
dups[[6]]$screen_13a_speed <- NULL
dups[[6]]$screen_13b_speed <- NULL
dups[[6]]$screen_21a_speed <- NULL
dups[[6]]$screen_21b_speed <- NULL
dups[[6]]$screen_22a_speed <- NULL
dups[[6]]$screen_22b_speed <- NULL
dups[[6]]$screen_23a_speed <- NULL
dups[[6]]$screen_23b_speed <- NULL
dups[[6]]$total_detritus_weight <- NULL
dups[[6]]$total_detritus_weight_units <- NULL
dups[[6]]$water_clarity <- NULL
dups[[6]]$dbo_es_st_obs_impent_obs_aqua_id <- NULL
dups[[6]]$dbo_es_st_obs_impent_sample_id <- NULL
dups[[6]]$do_surface <- NULL
dups[[6]]$salinity_mid <- NULL

dups[[12]]$notes <- NULL

dups[[16]]$dbo_es_st_lkp_species_fish_nodc_code <- NULL
dups[[16]][which(is.na(dups[[16]]$nodc_code)),]$nodc_code <- dups[[16]][which(is.na(dups[[16]]$nodc_code)),]$dbo_es_st_group_fish_nodc_code
dups[[16]]$dbo_es_st_group_fish_nodc_code <- NULL
#############################

# cleaning older data to match some fields in new data (per Alissa's Access queries)
dataa <- names(dups)
dups <- lapply(dups, function(x) {
  print(x$filename[1])
  names(x) <- toupper(names(x))
  x <- x %>% 
    mutate(across(everything(), ~ na_if(str_trim(.x), "")))
  
  x <- x %>% 
    mutate(across(everything(), ~ na_if(str_trim(.x), "`")))
  
  x <- x %>% 
    mutate(across(everything(), ~ na_if(str_trim(.x), ".")))
  
  #making dbo_es_st_sample_sample_id and samplno line up
  if ("SAMPLNO" %in% names(x)) { #samplno is the longer identifier where it exists
    x$DBO_ES_ST_SAMPLE_SAMPLE_ID <- sapply(x$SAMPLNO, function(q) { #you changed this, does this still work?
      if(nchar(q) > 9) {
        a <- paste0(substr(q, 1, 6), substr(q, 12, 14)) #this is the 9 digit sample ID
      } else { a <- q}
    return(a)})
    x$SAMPLE_ID <- x$SAMPLNO
  }
  #######
  #add here...
  if ("SAMPLE_ID" %in% names(x) & "DBO_ES_ST_SAMPLE_SAMPLE_ID" %in% names(x)) {
    x$SAMPLE_ID[is.na(x$SAMPLE_ID)] <- x$DBO_ES_ST_SAMPLE_SAMPLE_ID[is.na(x$SAMPLE_ID)]
  }
  
  if ("SAMPLE_ID" %in% names(x) & !"DBO_ES_ST_SAMPLE_SAMPLE_ID" %in% names(x)) { #this is now being defined as the "universal" sample ID reference
    x$DBO_ES_ST_SAMPLE_SAMPLE_ID <- x$SAMPLE_ID
  }
  
  if("DBO_ES_ST_SAMPLE_SAMPLE_ID" %in% names(x) & !"SAMPLE_ID" %in% names(x)) {
    x$SAMPLE_ID <- x$DBO_ES_ST_SAMPLE_SAMPLE_ID
  }
  
  ### standardizing the DATE field
  if("COLLECTION_DATE" %in% names(x)) { #DATE is gonna be the standardized date field name
    x$COLLECTION_DATE <- as.POSIXct(x$COLLECTION_DATE)
    x$DATE <- x$COLLECTION_DATE
    x$COLLECTION_DATE <- NULL
  }
  
  if("DATE" %in% names(x)) {
    x$DATE <- as.POSIXct(x$DATE)
  }
  if ("ST_TIME" %in% names(x)) {
    # tryCatch( {
    if ("DATE" %in% names(x)) {
      x$DATE <- as.POSIXct(paste(as.Date(x$DATE), regmatches(x$ST_TIME, regexpr("[0-9]{2}:[0-9]{2}:[0-9]{2}", x$ST_TIME))),
                                      format="%Y-%m-%d %H:%M:%S")
      x$ST_TIME <- NULL
    } else {
      x$DATE <- as.POSIXct(paste(as.Date(x$COL_DATE), regmatches(x$ST_TIME, regexpr("[0-9]{2}:[0-9]{2}:[0-9]{2}", x$ST_TIME))),
                                      format="%Y-%m-%d %H:%M:%S")
      x$COL_DATE <- NULL
      x$ST_TIME <- NULL
    }
    # )
    # }, error = function(e) {x$collection_date <- NA})
  }
  
  if("COL_DATE" %in% names(x) & !"DATE" %in% names(x)) {
    x$COL_DATE <- as.POSIXct(x$COL_DATE)
    x$DATE <- x$COL_DATE
    x$COL_DATE <- NULL
  }
  
  if ("END_DATE" %in% names(x) && "END_TIME" %in% names(x)) {
    # tryCatch( {
      x$END_DATE <- as.POSIXct(paste(as.Date(x$END_DATE), regmatches(x$END_TIME, regexpr("[0-9]{2}:[0-9]{2}:[0-9]{2}", x$END_TIME))),
                                      format="%Y-%m-%d %H:%M:%S")
      x$END_TIME <- NULL
    # )
    # }, error = function(e) {x$collection_date <- NA})
  }
  
  if("START_DATE" %in% names(x)) {
    x$START_DATE <- as.POSIXct(x$START_DATE)
  }
  
  if("START_DATE" %in% names(x) & !"DATE" %in% names(x)) {
    x$DATE <- x$START_DATE
  }
  
  if("DUR" %in% names(x)) {
    x$END_DATE <- NULL
  }
  
  if(!"DUR" %in% names(x)) {
    if("END_DATE" %in% names(x) & "START_DATE" %in% names(x) & !all(is.na(x$END_DATE))) {
      x$DUR <- difftime(x$END_DATE, x$START_DATE, units = "mins")
    }
  }
  ###########
  
  #######getting SITE ID standardized
  if (grepl("Level 1_Beach Seine", x$FILENAME[1])) {
    if ("LOC_CODE" %in% names(x)) { #no conflicts with constructing GPS_GROUP_ID from other identifiers
      x$GPS_GROUP_ID <- tryCatch( #make it out of a character string (SITE_ID) before GRID
        {
          gsub(" ", "", paste0("BWS", as.integer(x$LOC_CODE)))
        },
        warning = function(w) {
          return(gsub(" ", "", paste0("BWS", x$LOC_CODE)))
        }
      )
    }
    x$GPS_GROUP_ID <- toupper(x$GPS_GROUP_ID)
    x$GPS_GROUP_ID[grep("^B", x$GPS_GROUP_ID, invert = T)] <- paste0("BWS", x$GPS_GROUP_ID[grep("^B", x$GPS_GROUP_ID, invert = T)])
    x$GPS_GROUP_ID <- gsub("\\s+", "", x$GPS_GROUP_ID)
    x$LOC_CODE <- NULL
    x$site_id <- x$GPS_GROUP_ID
    x$GPS_GROUP_ID <- NULL
  } else if ("LOC_CODE" %in% names(x) && !"SITE_ID" %in% names(x)) {
    #x$GPS_GROUP_ID <- x$LOC_CODE
    x$site_id <- as.character(x$LOC_CODE)
    x$LOC_CODE <- NULL #drop LOCATION_ID
  } else if ("GRID" %in% names(x) && !"SITE_ID" %in% names(x)) {
    x$site_id <- as.character(x$GRID)
  } else if ("GPS_POINT_ID" %in% names(x) && !"SITE_ID" %in% names(x)) {
    x$site_id <- as.character(x$GPS_GROUP_ID)
  }
  
  if ("GPS_POINT_ID" %in% names(x)) {
    x$GPS_POINT_ID <- NULL
  }
  
  if ("LOCATION_ID" %in% names(x)) {
    x$LOCATION_ID <- NULL
  }
  
  if("START_LAT_DD" %in% names(x)) {
    x$START_LAT_DD <- as.numeric(x$START_LAT_DD)
  }
  
  if("START_LAT_MM" %in% names(x)) {
    x$START_LAT_MM <- as.numeric(x$START_LAT_MM)
  }
  
  if("START_LAT_SS" %in% names(x)) {
    x$START_LAT_SS <- as.numeric(x$START_LAT_SS)
  }
  
  if("END_LAT_SS" %in% names(x)) {
    x$END_LAT_SS <- as.numeric(x$END_LAT_SS)
  }
  
  if("END_LAT_DD" %in% names(x)) {
    x$END_LAT_DD <- as.numeric(x$END_LAT_DD)
  }
  
  if("END_LAT_MM" %in% names(x)) {
    x$END_LAT_MM <- as.numeric(x$END_LAT_MM)
  }
  
  if("START_LON_DD" %in% names(x)) {
    x$START_LON_DD <- as.numeric(x$START_LON_DD)
  }
  
  if("START_LON_MM" %in% names(x)) {
    x$START_LON_MM <- as.numeric(x$START_LON_MM)
  }
  
  if("START_LON_SS" %in% names(x)) {
    x$START_LON_SS <- as.numeric(x$START_LON_SS)
  }
  
  if("END_LON_SS" %in% names(x)) {
    x$END_LON_SS <- as.numeric(x$END_LON_SS)
  }
  
  if("END_LON_DD" %in% names(x)) {
    x$END_LON_DD <- as.numeric(x$END_LON_DD)
  }
  
  if("END_LON_MM" %in% names(x)) {
    x$END_LON_MM <- as.numeric(x$END_LON_MM)
  }
  ###################

  if ("CNT_LVL2" %in% names(x)) {
    x$total_counted <- x$CNT_LVL2
    x$CNT_LVL2 <- NULL
  }
  
  if ("CNT_LVL3" %in% names(x)) {
    x$CNT_LVL3 <- as.integer(x$CNT_LVL3)
  }
  
  if ("NODC_CODE" %in% names(x)) {
    x$nodc <- x$NODC_CODE
    x$NODC_CODE <- NULL
  }
  
  if ("LENGTH" %in% names(x) && !"LEN_METH" %in% names(x)) {
    x$LENGTH_MEASURE_TECHNIQUE_CODE <- "U"
  }
  
  if("LEN_METH" %in% names(x)) {
    x$LENGTH_MEASURE_TECHNIQUE_CODE <- x$LEN_METH
    x$LEN_METH <- NULL
  }
  
  if ("INDIVIDUAL_LENGTH" %in% names(x)) {
    x$LENGTH <- x$INDIVIDUAL_LENGTH
    x$INDIVIDUAL_LENGTH <- NULL
  }
  
  names(x) <- toupper(names(x))
  #LIF_STG dups[[15]], dups[[25]]
  #FISH_LIFE_STAGE_ID dups[[26]] does this need to be converted to the above
  
  #COND_COD dups[[28]] seems to have more values than CONDITION_ID dups[[26]]

  # if(grepl("Level 3", x$filename[1]) & grepl("200(4|5)", x$filename[1])) { #IMPORT_LEVEL_3_NO_NODCSIZES
  # fill nodc code where NA with code below, i.e. match up level 2 data and get nodc code from there
  #  surveyy <- gsub('Level [0-9]_([A-Za-z ]*) [0-9].*','\\1', x$filename[1])
  #  y <- dups[[which(grepl(paste0("Level 2_", surveyy, " 200(4|5)"), names(dups)))]]
  #  iin <- merge(x, y, by=c("group_fish_id", "sample_id"))
  #  iin$filename <- iin$filename.x
  #  iin$filename.y <- NULL
  #  names(iin) <- gsub("[.]y", "", names(iin))
  #  x <- iin[,names(x)]
  # }
  
  if ("COM_NAME" %in% names(x)) {
    x$COMMON_NAME <- x$COM_NAME
    x$COM_NAME <- NULL
  }
  
  if ("GEAR_CODE" %in% names(x)) {
    x$GEAR <- x$GEAR_CODE
    x$GEAR_CODE <- NULL
  } else if ("GEARCHAR" %in% names(x)) {
    x$GEAR <- x$GEARCHAR
    x$GEARCHAR <- NULL
  }
  
  if ("GEAR" %in% names(x)) {
    x$GEAR <- as.integer(x$GEAR)
  }
  
  if ("SAMP_VOL" %in% names(x)) {
    x$SAMPLE_VOLUME <- as.integer(x$SAMP_VOL)
    x$SAMP_VOL <- NULL
  }
  
  if ("COL_YR" %in% names(x)) {
    x$COL_YR <- NULL
  }
  
  if ("COL_DAY" %in% names(x)) {
    x$COL_DAY <- NULL
  }
  
  if ("CALIBRATION_CONSTANT" %in% names(x)) {
    x$CALIBRATION_CONSTANT <- NULL
  }
  
  if ("DISTANCE_OVERGROUND" %in% names(x)) {
    x$DISTANCE_OVERGROUND <- NULL
  }
  
  if ("FLOW_METER1_COUNT" %in% names(x)) {
    x$FLOW_METER1_COUNT <- NULL
  }
  
  if ("FLOW_METER2_COUNT" %in% names(x)) {
    x$FLOW_METER2_COUNT <- NULL
  }
  
  if ("FLOW_METER1_USE_CODE" %in% names(x)) {
    x$FLOW_METER1_USE_CODE <- NULL
  }
  
  if ("FLOW_METER2_USE_CODE" %in% names(x)) {
    x$FLOW_METER2_USE_CODE <- NULL
  }
  
  if ("PHOTO_CODE" %in% names(x)) {
    x$PHOTO_CODE <- NULL
  }
  
  if ("PHOTOGRAPH" %in% names(x)) {
    x$PHOTO_CODE <- NULL
  }
  
  if ("START_FLOW" %in% names(x)) {
    x$START_FLOW <- NULL
  }
  
  if ("START_TIME" %in% names(x)) {
    x$START_TIME<- NULL
  }
  
  if ("STATION_DEPTH" %in% names(x)) {
    x$STATION_DEPTH <- NULL
  }
  
  if ("STOP_FLOW" %in% names(x)) {
    x$STOP_FLOW <- NULL
  }
  
  if ("SURVEY_NUMBER" %in% names(x)) {
    x$SURVEY_NUMBER <- NULL
  }
  
  if ("SURVEY_SEQUENCE_NO" %in% names(x)) {
    x$SURVEY_SEQUENCE_NO <- NULL
  }
  
  if ("TOW_DISTANCE" %in% names(x)) {
    x$TOW_DISTANCE <- NULL
  }
  
  if ("VALIDITY_CODE" %in% names(x)) {
    x$VALIDITY_CODE <- NULL
  }
  
  if ("COL_MM" %in% names(x)) {
    x$COL_MM <- NULL
  }
  
  if("MONTH" %in% names(x)) {
    x$MONTH <- NULL
  }
  
  if("WEEK" %in% names(x)) {
    x$WEEK <- NULL
  }
  
  if("YEAR" %in% names(x)) {
    x$YEAR <- NULL
  }
  
  if ("SURVEY_YEAR" %in% names(x)) {
    x$SURVEY_YEAR <- NULL
  }
  
  if ("SURVEY_MONTH" %in% names(x)) {
    x$SURVEY_MONTH <- NULL
  }
  
  if ("COLLECTION_TIME" %in% names(x)) {
    x$COLLECTION_TIME <- NULL
  }
  
  if("SPECIES" %in% names(x)) {
    x$ESCI_CODE <- x$SPECIES
    #x$ESCI_CODE <- NULL
  }
  
  if("SAMPLING_PROGRAM_CODE" %in% names(x)) {
    x$PROGRAM <- x$SAMPLING_PROGRAM_CODE
    x$SAMPLING_PROGRAM_CODE <- NULL
  }
  
  if("DBO_ES_ST_OBS_AQUA_CONTRACTOR_ID" %in% names(x)) {
    x$DBO_ES_ST_OBS_AQUA_CONTRACTOR_ID <- NULL
  }
  
  if("DBO_ES_ST_SAMPLE_CONTRACTOR_ID" %in% names(x)) {
    x$CONTRACTOR_ID <- x$DBO_ES_ST_SAMPLE_CONTRACTOR_ID
    x$DBO_ES_ST_SAMPLE_CONTRACTOR_ID <- NULL
  }
  
  if("DBO_ES_ST_OBS_AQUA_CONFIRMED" %in% names(x)) {
    x$CONFIRMED <- x$DBO_ES_ST_OBS_AQUA_CONFIRMED
    x$DBO_ES_ST_OBS_AQUA_CONFIRMED <- NULL
  }
  
  if("VOLUME_FILTERED2" %in% names(x)) {
    x$VOLUME_FILTERED2 <- NULL
  }
  
  if("CONDENSER_DELTA_T" %in% names(x)) {
    x$CONDENSER_DELTA_T <- NULL
  }
  
  if("DETRITUS_CODE" %in% names(x)) {
    x$DETRITUS_CODE <- NULL
  }
  
  if("FLOWMETER_CODE" %in% names(x)) {
    x$FLOWMETER_CODE <- NULL
  }
  
  if("MAXIMUM_WEIGHT" %in% names(x)) {
    x$MAXIMUM_WEIGHT <- NULL
  }
  
  if("MINIMUM_WEIGHT" %in% names(x)) {
    x$MINIMUM_WEIGHT <- NULL
  }
  
  if("SUBSAMPLE_YN" %in% names(x)) {
    x$SUBSAMPLE_YN <- NULL
  }
  
  if("TOTAL_CONTAINERS" %in% names(x)) {
    x$TOTAL_CONTAINERS <- NULL
  }
  
  if("TOTAL_LEFT_OVER" %in% names(x)) {
    x$TOTAL_LEFT_OVER <- NULL
  }
  
  if("TOTAL_WEIGHT" %in% names(x)) {
    x$TOTAL_WEIGHT <- NULL
  }

  if("UNIT_MEASURE_WEIGHT_CODE" %in% names(x)) {
    x$UNIT_MEASURE_WEIGHT_CODE <- NULL
  }
  
  if("DBO_ES_ST_OBS_AQUA_INVESTIGATORS" %in% names(x)) {
    x$DBO_ES_ST_OBS_AQUA_INVESTIGATORS <- NULL
  }
  
  if("DBO_ES_ST_SAMPLE_INVESTIGATORS" %in% names(x)) {
    x$INVESTIGATORS <- x$DBO_ES_ST_SAMPLE_INVESTIGATORS
    x$DBO_ES_ST_SAMPLE_INVESTIGATORS <- NULL
  }
  
  if("DBO_ES_ST_SAMPLE_CONFIRMED" %in% names(x)) {
  x$DBO_ES_ST_SAMPLE_CONFIRMED <- NULL
  }

  if("DBO_ES_ST_OBS_AQUA_SAMPLE_ID" %in% names(x)) {
    x$DBO_ES_ST_OBS_AQUA_SAMPLE_ID <- NULL
  }
  
  if("GPS_LOC_YN" %in% names(x)) {
    x$GPS_LOC_YN <- NULL
  }
  
  if("DBO_ES_ST_OBS_IMPENT_CONFIRMED" %in% names(x)) {
    x$DBO_ES_ST_OBS_IMPENT_CONFIRMED <- NULL
  }
  
  if("END_DATE" %in% names(x)) {
    x$END_DATE <- NULL
  }
  
  if("OBS_AQUA_ID" %in% names(x)) {
    x$OBS_AQUA_ID <- NULL
  }
  
  if("OBS_IMPENT_ID" %in% names(x)) {
    x$OBS_IMPENT_ID <- NULL
  }
  
  if("DELAYED_MORTALITY" %in% names(x)) {
    x$DELAYED_MORTALITY <- NULL
  }
  
  return(x)
})

getspp <- function(x) {
  if ("NOTES" %in% names(x)) {
    x <- x[grep("Sturgeon", x$NOTES), ] # "STRIPED BASS STURGEON|TURTLE
  } else {
    x <- data.frame()
  }
  return(x)
}

turtle <- lapply(dups, getspp)
turtle <- turtle[sapply(turtle, nrow) > 0]

all(sapply(dups, function(x) {
return(all(is.na(x$OBS_IMPENT_ID))) #left off dups[[6]] do  next
}))

#NEXT...
#when you make this into a db, probably have separate look up table for nodc code, scientific name etc 
#ESCI code corresponds to SPECIES in other dataframes, but do we need that column at all?
#that's the more common field^ but seems less useful
#is SPINDEX useful?

LENGTH_TYPE_NAME <- c("FL", "NM", "O", "TL", "U") # need this to import similarly to Alissa's schema
# LENGTH_TYPE and ID_LENGTH_TYPE are just "which" position matches to the above vector

names(dups) <- dataa # 27 and 30: is SPECIES the same as nodc_code?
#save(dups, file = file.path(myroot, "data/PSEG/PSEG_Data_compiled.RData"))
