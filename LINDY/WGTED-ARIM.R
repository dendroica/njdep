library(foreign)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# FYI you haven't done this part yet
# **********FOR SIZED LOBSTER ONLY WHEN USING HASabun***********************;
#  **********FOR LEGAL SIZED LOBSTER;
# SET abund;
# NUMBER=LEGALN;
# RUN;
#  **********FOR SUBLEGAL SIZED LOBSTER;
# SET abund;
# NUMBER=SUBLEGALN;
# RUN;
#  **********FOR ALL LOBSTER WHEN USING HASabun;
# SET abund;
# NUMBER=TOTALN;
# RUN;

# if quarterly summaries end up being necessary, you need to implement this
# you would also have to re-code the CRUISE from 3 to 2 accordingly, as below...
# abund[abund$CRUISE == 3,]$CRUISE <- 2
# abund[abund$CRUISE == 6,]$CRUISE <- 5
Macro20 <- function(abund, timescale = "month") {
  if (timescale == "season" | timescale == "quarter") {
    abund <- abund %>% mutate(
      CRUCODE = case_match(CRUCODE,
        19896 ~ 19895,
        .default = CRUCODE
      )
    )
    crucode <- as.character(abund$CRUCODE)
    crucode <- str_replace(crucode, "3$", "2") # '$' anchors to end
    # Convert back to numeric (handling potential errors if input wasn't purely digits)
    abund$CRUCODE <- as.numeric(crucode)
  }
  return(abund)
}

# setups here would have to be a data frame of NUMBER,WEIGHT
Macro6 <- function(setups) {
  tsam <- setups %>% mutate(SPTOW = ifelse(NUMBER > 0, 1, 0)) %>%
    summarise(
      TN = sum(NUMBER, na.rm = T), # 10469.680381
      NM = mean(NUMBER, na.rm = T),
      NMSE = sd(NUMBER, na.rm = T) / sqrt(n()),
      NV = var(NUMBER, na.rm = T),
      NN = n(), WN = n(), SPTOW = sum(SPTOW, na.rm=T)
    )
  if ("WEIGHT" %in% colnames(setups)) {
    weight <- setups %>% summarise(
      TW = sum(WEIGHT, na.rm = T),
      WM = mean(WEIGHT, na.rm = T),
      WMSE = sd(WEIGHT, na.rm = T) / sqrt(n()),
      WV = var(WEIGHT, na.rm = T)
    )
    tsam <- cbind(tsam, TW = weight$TW, WM = weight$WM, WMSE = weight$WMSE, WV = weight$WV)
  }

  tsamw <- tsam %>%
    mutate(SAMPLES = NN)
  return(tsamw)
}

# Function to apply factor (stratum weight) to means, variance, and standard errors
Macro2 <- function(tsamw, factor) {
  tsamw$NM <- factor * tsamw$NM
  if ("WM" %in% colnames(tsamw)) {
    tsamw$WM <- factor * tsamw$WM
    tsamw$WMSE <- tsamw$WV / tsamw$WN * factor^2
  }
  tsamw$NMSE <- tsamw$NV / tsamw$NN * factor^2
  return(data.frame(tsamw))
}

# MACRO 7
Macro7 <- function(tsamw) { # you get this from Macro6
  factor <- unname(unlist(Map(function(AREA, STRATUM) {
      factor <- NA
      if (AREA == "ALL") {
        if (STRATUM == 12) {
          factor <- 0.008
        } else if (STRATUM == 13) {
          factor <- 0.018
        } else if (STRATUM == 14) {
          factor <- 0.043
        } else if (STRATUM == 15) {
          factor <- 0.010
        } else if (STRATUM == 16) {
          factor <- 0.045
        } else if (STRATUM == 17) {
          factor <- 0.164
        } else if (STRATUM == 18) {
          factor <- 0.033
        } else if (STRATUM == 19) {
          factor <- 0.132
        } else if (STRATUM == 20) {
          factor <- 0.156
        } else if (STRATUM == 21) {
          factor <- 0.010
        } else if (STRATUM == 22) {
          factor <- 0.111
        } else if (STRATUM == 23) {
          factor <- 0.103
        } else if (STRATUM == 24) {
          factor <- 0.019
        } else if (STRATUM == 25) {
          factor <- 0.075
        } else if (STRATUM == 26) factor <- 0.074
      } else if (AREA == "INM") {
        if (STRATUM == 12) {
          factor <- 0.018
        } else if (STRATUM == 13) {
          factor <- 0.039
        } else if (STRATUM == 15) {
          factor <- 0.021
        } else if (STRATUM == 16) {
          factor <- 0.099
        } else if (STRATUM == 18) {
          factor <- 0.073
        } else if (STRATUM == 19) {
          factor <- 0.286
        } else if (STRATUM == 21) {
          factor <- 0.021
        } else if (STRATUM == 22) {
          factor <- 0.241
        } else if (STRATUM == 24) {
          factor <- 0.041
        } else if (STRATUM == 25) factor <- 0.162
      } else if (AREA == "SOU") {
        if (STRATUM == 21) {
          factor <- 0.024
        } else if (STRATUM == 22) {
          factor <- 0.284
        } else if (STRATUM == 23) {
          factor <- 0.264
        } else if (STRATUM == 24) {
          factor <- 0.049
        } else if (STRATUM == 25) {
          factor <- 0.191
        } else if (STRATUM == 26) factor <- 0.188
      } else if (AREA == "NOR") {
        if (STRATUM == 12) {
          factor <- 0.013
        } else if (STRATUM == 13) {
          factor <- 0.030
        } else if (STRATUM == 14) {
          factor <- 0.070
        } else if (STRATUM == 15) {
          factor <- 0.016
        } else if (STRATUM == 16) {
          factor <- 0.074
        } else if (STRATUM == 17) {
          factor <- 0.268
        } else if (STRATUM == 18) {
          factor <- 0.055
        } else if (STRATUM == 19) {
          factor <- 0.216
        } else if (STRATUM == 20) factor <- 0.256
      } else if (AREA == "SHR") {
        if (STRATUM == 12) {
          factor <- 0.216
        } else if (STRATUM == 13) {
          factor <- 0.314
        } else if (STRATUM == 14) factor <- 0.470
      } else if (AREA == "MON") {
        if (STRATUM == 12) {
          factor <- 0.119
        } else if (STRATUM == 13) {
          factor <- 0.262
        } else if (STRATUM == 14) factor <- 0.619
      } else if (AREA == "BSB") {
        if (STRATUM == 15) {
          factor <- 0.044
        } else if (STRATUM == 16) {
          factor <- 0.208
        } else if (STRATUM == 17) factor <- 0.748
      } else if (AREA == "SBS") {
        if (STRATUM == 18) {
          factor <- 0.104
        } else if (STRATUM == 19) {
          factor <- 0.410
        } else if (STRATUM == 20) factor <- 0.486
      } else if (AREA == "SIC") {
        if (STRATUM == 21) {
          factor <- 0.043
        } else if (STRATUM == 22) {
          factor <- 0.496
        } else if (STRATUM == 23) factor <- 0.462
      } else if (AREA == "CMH") {
        if (STRATUM == 24) {
          factor <- 0.114
        } else if (STRATUM == 25) {
          factor <- 0.446
        } else if (STRATUM == 26) factor <- 0.440
      } else if (AREA == "INS") {
        if (STRATUM == 12) {
          factor <- 0.103
        } else if (STRATUM == 15) {
          factor <- 0.122
        } else if (STRATUM == 18) {
          factor <- 0.419
        } else if (STRATUM == 21) {
          factor <- 0.119
        } else if (STRATUM == 24) factor <- 0.238
      } else if (AREA == "HOG") {
        if (STRATUM == 18) {
          factor <- 0.104
        } else if (STRATUM == 21) {
          factor <- 0.030
        } else if (STRATUM == 22) {
          factor <- 0.345
        } else if (STRATUM == 24) {
          factor <- 0.059
        } else if (STRATUM == 25) {
          factor <- 0.232
        } else if (STRATUM == 26) factor <- 0.229
      } else if (AREA == "PST") factor <- 1
  }, tsamw$AREA, tsamw$STRATUM)))
  tsamw <- Macro2(tsamw, factor)
  # setups <- tsamw %>% select(-NN, -WN)
  return(tsamw)
}

# MACRO 8
Macro8 <- function(tsamw) {
  # tsamw <- Macro7(tsamw)
  tssum <- tsamw %>%
    summarise(
      SAMPLET = sum(SAMPLES), NMSUM = sum(NM),
      NMSESUM1 = sum(NMSE),
      TNSUM = sum(TN),
      SPTOW = sum(SPTOW)
    )
  if ("WM" %in% colnames(tsamw)) {
    weight <- tsamw %>% summarise(
      WMSUM = sum(WM), WMSESUM1 = sum(WMSE),
      TWSUM = sum(TW)
    )
    tssum <- cbind(tssum, WMSUM = weight$WMSUM, WMSESUM1 = weight$WMSESUM1, TWSUM = weight$TWSUM)
  }

  tssum2 <- tssum %>%
    mutate(
      SAMPLES = SAMPLET,
      NMSESUM = sqrt(NMSESUM1)
    )

  if ("WMSESUM1" %in% colnames(tssum)) {
    weight <- tssum2 %>% mutate(WMSESUM = sqrt(WMSESUM1))
    tssum2 <- cbind(tssum2, WMSESUM = weight$WMSESUM)
  }

  setups <- tssum2 %>%
    select(-NMSESUM1) #-WMSESUM1
}

WgtedAriM <- function(mypath, myspp, area = "ALL", cruise = "ALL", outdir) {
  value_map <- c(
    "Black drum" = "PC",
    "Lobster - F (GE53mm)" = "HA53F",
    "Lobster - M (GE53mm)" = "HA53M",
    "Scup" = "SC",
    "Spot" = "LX",
    "Summer flounder" = "PD",
    "Tautog" = "TO",
    "Weakfish" = "CR",
    "Atl croaker" = "MU"
  )
  abund <- read.dbf(file.path(mypath, paste0(value_map[myspp],"ABUN.dbf")))
  abund$NUMBER <- abund$NUMBER / abund$MINOUT * 20
  if (!myspp %in% c("Lobster - F (GE53mm)", "Lobster - M (GE53mm)")) {
    abund$WEIGHT <- abund$WEIGHT / abund$MINOUT * 20
  }
  abund$CRUCODE[abund$CRUCODE == 19882] <- 19884
  abund$CRUCODE[abund$CRUCODE == 19883] <- 19885
  abund$CRUISE <- substr(abund$CRUCODE, 5, 5)
  abund$CRUISE <- as.integer(abund$CRUISE)
  abund$AREA <- "ALL"

  if (area == "INM") {
    abund <- abund[!abund$STRATUM %in% c(14, 17, 20, 23, 26), ]
    abund$AREA <- "INM"
  }

  cruiseno <- 1:6
  if (cruise == "AprOct") {
    abund <- abund[abund$YEAR > 1988, ]
    cruiseno <- c(2, 5)
  } else if (cruise == "AugOct") {
    cruiseno <- 4:5
  } else if (cruise == "Oct") {
    cruiseno <- 5
  } else if (cruise == "AprthruOct") {
    cruiseno <- 2:5
    abund <- abund[abund$YEAR > 1988, ]
  } else if (cruise == "Spring") {
    cruiseno <- 2:3
  }
  abund <- abund[abund$CRUISE %in% cruiseno, ]
  spp <- abund[abund$NUMBER > 0, ]

  for (grouping in c("YEAR")) { # c("YEAR", "STRATUM", "CRUISE", "CRUCODE"),
    #stratsp <- spp %>%
    #  group_by(across(all_of(grouping))) %>%
    #  summarise(SPTOW = n(), .groups = "drop") %>%
    #  select(all_of(c("SPTOW", grouping))) %>%
    #  drop_na(all_of(grouping))

    tsspps2 <- spp %>%
      summarise(SPTOW = n(), .groups = "drop") %>%
      mutate(YEAR = "Grand Total") %>%
      select(YEAR, SPTOW)

    if (all(grouping %in% c("YEAR"))) { # , "CRUISE", "CRUCODE"
      setups <- abund %>%
        group_by(across(all_of(c(grouping, "STRATUM", "AREA")))) %>%
        Macro6() %>%
        Macro7() %>%
        group_by(across(all_of(grouping))) %>%
        Macro8()

      yam2 <- setups %>% #merge(setups, stratsp, by = grouping, all = TRUE) %>%
        mutate(
          STRATUM = 0,
          # SPTOW = n(),
          PERCENTF = ifelse(SAMPLES > 0, SPTOW / SAMPLES, 0),
          TOTALN = TNSUM,
          STRATNM = NMSUM,
          STRATNSE = NMSESUM
        ) %>%
        mutate(across(where(is.numeric), ~ replace_na(., 0)))

      if ("TWSUM" %in% colnames(yam2)) {
        yam2 <- yam2 %>% mutate(
          TOTALW = TWSUM,
          STRATWM = WMSUM,
          STRATWSE = WMSESUM
        )
      }

      if ("CRUCODE" %in% colnames(yam2)) {
        yam2$CRUISE <- substr(yam2$CRUCODE, 5, 5)
      }

      if ("CRUISE" %in% colnames(yam2)) {
        yam2 <- yam2 %>% mutate(
          MONTH = case_match(CRUISE,
            "1" ~ "JANUARY",
            "2" ~ "APRIL",
            "3" ~ "JUNE",
            "4" ~ "AUGUST",
            "5" ~ "OCTOBER",
            "6" ~ "DECEMBER",
            .default = CRUISE
          )
        ) # %>% #LINE 458 DROPS?
      }

      # SAMPLES = 0, #fill samples here?
      # PERCENTF = 0,
      # TOTALN = 0, STRATNM = 0, STRATNSE = 0,
      # TOTALW = 0, STRATWM = 0, STRATWSE = 0) %>% select(-STRAT) %>% #line 368 drop more for TSystratS?
      
      setups <- abund %>%
        group_by(across(all_of(c("STRATUM", "AREA")))) %>%
        Macro6() %>%
        Macro7() %>%
        Macro8()

      tsstrata <- setups %>%
        mutate(
          YEAR = "Grand Total",
          STRATUM = 0,
          SAMPLES = setups$SAMPLES,
          TOTALN = setups$TNSUM,
          TOTALW = setups$TWSUM,
          STRATNM = setups$NMSUM,
          STRATNSE = setups$NMSESUM,
          STRATWM = setups$WMSUM, # this still works?
          STRATWSE = setups$WMSESUM,
          MONTHN = "1"
        ) %>%
        mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% select(-SPTOW)
      # SAMPLES = 0, #fill samples here?
      # PERCENTF = 0,
      # TOTALN = 0, STRATNM = 0, STRATNSE = 0,
      # TOTALW = 0, STRATWM = 0, STRATWSE = 0)

      tsstrats <- list(tsstrata, yam2)
      # tsstrata <- tsstrats[[1]]
      # yam2 <- tsstrats[[2]]

      tsstratb <- merge(tsstrata, tsspps2, by = "YEAR", all = T) %>%
        mutate(
          PERCENTF = ifelse(SAMPLES > 0, SPTOW / SAMPLES, 0),
          MONTH = "Grand Total"
        ) %>%
        select(-MONTHN, -STRATUM)

      if (grouping == "YEAR") {
        ystrat <- tsstratb %>% select(-MONTH)
        ystrat <- rbind(yam2[, which(colnames(yam2) %in% colnames(ystrat))], ystrat) #%>%
          #rename(TOTALN = TNSUM, STRATNM = NMSUM, STRATNSE = NMSESUM)
        keepercols <- c("YEAR", "SAMPLES", "SPTOW", "PERCENTF", "TOTALN", "STRATNM", "STRATNSE")
        if ("TWSUM" %in% colnames(ystrat)) {
          #ystrat <- ystrat %>% rename(TOTALW = TWSUM, STRATWM = WMSUM, STRATWSE = WMSESUM)
          keepercols <- c(keepercols, c("TOTALW", "STRATWM", "STRATWSE"))
        }
        ystrat <- ystrat[, keepercols]
      }

      if (grouping == "CRUISE") {
        MSTRAT <- tsstratb %>% select(-YEAR) # SPTOW Calc differs here...
        MSTRAT <- rbind(yam2[, which(colnames(yam2) %in% colnames(MSTRAT))], MSTRAT)
        MSTRAT <- MSTRAT %>%
          rename(TOTALN = TNSUM, STRATNM = NMSUM, STRATNSE = NMSESUM, TOTALW = TWSUM, STRATWM = WMSUM, STRATWSE = WMSESUM)
        MSTRAT <- MSTRAT[, c("MONTH", "SAMPLES", "SPTOW", "PERCENTF", "TOTALN", "STRATNM", "STRATNSE", "TOTALW", "STRATWM", "STRATWSE")]
      }

      if (grouping == "CRUCODE") {
        YMSTRATA <- tsstratb %>% mutate(MONTH = "All") # %>% bind_rows(yam2) #YMSTRATS
        yam2$YEAR <- substr(yam2$CRUCODE, 1, 4)
        YMSTRATA <- rbind(yam2[, which(colnames(yam2) %in% colnames(YMSTRATA))], YMSTRATA) %>%
          rename(TOTALN = TNSUM, STRATNM = NMSUM, STRATNSE = NMSESUM, TOTALW = TWSUM, STRATWM = WMSUM, STRATWSE = WMSESUM)
        YMSTRATA <- YMSTRATA[, c("YEAR", "MONTH", "SAMPLES", "SPTOW", "PERCENTF", "TOTALN", "STRATNM", "STRATNSE", "TOTALW", "STRATWM", "STRATWSE")]
      }
    }

    tsamw <- abund %>%
      group_by(across(all_of(grouping))) %>%
      Macro6()
    factor <- 1
    tsamw <- Macro2(tsamw, factor = factor)
    tsamw$NMSE <- sqrt(tsamw$NV / tsamw$NN)
    if ("WV" %in% colnames(tsamw)) {
      tsamw$WMSE <- sqrt(tsamw$WV / tsamw$WN)
    }
    setups <- tsamw %>% # beware that in the original code line 251 drops from tsamw itself
      select(-NV, -NN, -WN)

    # i think this is gonna be an output specific to STRATUM
    if (grouping == "STRATUM") {
      TSSTRATS <- merge(setups, stratsp, by = grouping, all = TRUE) %>%
        mutate(
          PERCENTF = ifelse(tsamw$SAMPLES > 0, SPTOW / tsamw$SAMPLES, 0),
          TOTALN = tsamw$TN,
          TOTALW = tsamw$TW,
          STRATNM = tsamw$NM,
          STRATNSE = tsamw$NMSE,
          STRATWM = tsamw$WM,
          STRATWSE = tsamw$WMSE
        ) %>%
        mutate(across(where(is.numeric), ~ replace_na(., 0)))
      # select(-STRAT) %>% #line 368 drop more for TSystratS?
      # SAMPLES = 0, #fill samples here?
      # PERCENTF = 0,
      # TOTALN = 0, STRATNM = 0, STRATNSE = 0,
      # TOTALW = 0, STRATWM = 0, STRATWSE = 0)
      # mutate(across(where(is.numeric), ~replace_na(.,0)))
      # }

      # tsam <- setups %>%
      #  summarise(TN = sum(NUMBER), TW = sum(WEIGHT),
      #            NM = mean(NUMBER), WM = mean(WEIGHT),
      #            NMSE = sd(NUMBER) / sqrt(n()), WMSE = sd(WEIGHT) / sqrt(n()),
      #            NV = var(NUMBER), WV = var(WEIGHT),
      #            NN = n(), WN = n())
      # tsamw <- tsam %>%
      #  mutate(SAMPLES = n())

      TSSTRATX <- setups %>% mutate(
        YEAR = "Grand Total", # this used to be a merge/altered template e.g. line 394
        STRATUM = 0,
        TOTALN = tsamw$TN,
        TOTALW = tsamw$TW,
        STRATNM = tsamw$NM,
        STRATNSE = tsamw$NMSE,
        STRATWM = tsamw$WM,
        STRATWSE = tsamw$WMSE
      ) # %>%
      # select(-MONTH, -MONTHN, -SPTOW) #line 306: added SPTOW here for the merge below

      TSSTRATZ <- merge(TSSTRATX, tsspps2, by = "YEAR", all = TRUE) %>%
        mutate(PERCENTF = ifelse(tsamw$SAMPLES > 0, SPTOW / tsamw$SAMPLES, 0))

      TSSTRATZY <- TSSTRATZ
      if (!"YEAR" %in% grouping) {
        TSSTRATZY <- TSSTRATZY %>% select(-YEAR)
      }

      TSSTRATS <- TSSTRATS[, c(grouping, "SAMPLES", "SPTOW", "PERCENTF", "TOTALN", "STRATNM", "STRATNSE", "TOTALW", "STRATWM", "STRATWSE")]
      tsamw_all <- Macro6(abund)
      tsamw_all$PERCENTF <- ifelse(tsamw_all$SAMPLES > 0, tsamw_all$NN / tsamw_all$SAMPLES, 0)
      TSSTRATS <- rbind(TSSTRATS, c("All Combined", tsamw_all$NN, tsamw_all$SAMPLES, tsamw_all$PERCENTF, tsamw_all$TN, tsamw_all$NM, tsamw_all$NMSE, tsamw_all$TW, tsamw_all$WM, tsamw_all$WMSE))
    }
    # TSSTRATY <- TSSTRATZY %>%
    #  mutate(STRATA = ifelse(STRATUM == 0, "All Combined", STRATUM)) %>%
    #  select(-STRATUM) %>%
    #  rename(STRATUM = STRATA)
  }

  # the main outputs are TSSTRATS, ystrat, MSTRAT, YMSTRATA
  out <- list(ystrat) # TSSTRATS, MSTRAT, YMSTRATA #LINE 424 what you'll need to do is get the var naming the same/compatible...
  cruises <- paste0("cruise", paste(cruise, collapse = ""))
  myfile <- paste(myspp, paste0("strata", area), paste0("cru", cruise), sep = "_")
  # yrs <- 1988:2025
  annual <- out[[1]]
  annual_yrs <- annual[-nrow(annual), ]
  annual_yrs$YEAR <- as.integer(annual_yrs$YEAR)
  # missing <- yrs[!yrs %in% annual$YEAR]
  annual_yrs <- annual_yrs %>%
    complete(
      YEAR = full_seq(c(1988, max(YEAR)), period = 1), # Fills sequence based on min/max in group
      fill = list(value = NA)
    ) %>% # Fills new value columns with NA
    ungroup()
  annual <- rbind(annual[nrow(annual), ], annual_yrs)
  annual$month <- cruise
  annual$strata <- area
  write.csv(annual, file = file.path(outdir, paste(myfile, "annualindex.csv", sep = "_")), row.names = F)
  # write.csv(out[[2]], file = file.path(outdir, paste(myfile, "strata.csv", sep="_")), row.names=F)
  # write.csv(out[[3]], file = file.path(outdir, paste(myfile, "month.csv", sep="_")), row.names=F)
  return(out)
}
