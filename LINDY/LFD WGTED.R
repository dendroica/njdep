library(foreign)
library(dplyr)
library(purrr)
library(tidyr)
#TO DO: expand lengths such that lengths that never occur in the data are filled with 0
#this might be accomplished by full join (see line 908)
#...or my own code fixing this at 940 may need to be applied elsewhere

#APR & OCT
#ABUND <- ABUND[ABUND$CRUISE %in% c(2,5) & ABUND$YEAR != 1988,]
#LENG <- LENG[LENG$CRUISE %in% c(2,5) & LENG$YEAR != 1988,]

factor_assignment <- function(area, stratum) { #MACRO1
  factor <- NA
  
  if (area == "ALL") {
    if (stratum == 12) factor <- 0.008
    else if (stratum == 13) factor <- 0.018
    else if (stratum == 14) factor <- 0.043
    else if (stratum == 15) factor <- 0.010
    else if (stratum == 16) factor <- 0.045
    else if (stratum == 17) factor <- 0.164
    else if (stratum == 18) factor <- 0.033
    else if (stratum == 19) factor <- 0.132
    else if (stratum == 20) factor <- 0.156
    else if (stratum == 21) factor <- 0.010
    else if (stratum == 22) factor <- 0.111
    else if (stratum == 23) factor <- 0.103
    else if (stratum == 24) factor <- 0.019
    else if (stratum == 25) factor <- 0.075
    else if (stratum == 26) factor <- 0.074
  } else if (area == "INM") {
    if (stratum == 12) factor <- 0.018
    else if (stratum == 13) factor <- 0.039
    else if (stratum == 15) factor <- 0.021
    else if (stratum == 16) factor <- 0.099
    else if (stratum == 18) factor <- 0.073
    else if (stratum == 19) factor <- 0.286
    else if (stratum == 21) factor <- 0.021
    else if (stratum == 22) factor <- 0.241
    else if (stratum == 24) factor <- 0.041
    else if (stratum == 25) factor <- 0.162
  } else if (area == "SOU") {
    if (stratum == 21) factor <- 0.024
    else if (stratum == 22) factor <- 0.284
    else if (stratum == 23) factor <- 0.264
    else if (stratum == 24) factor <- 0.049
    else if (stratum == 25) factor <- 0.191
    else if (stratum == 26) factor <- 0.188
  } else if (area == "NOR") {
    if (stratum == 12) factor <- 0.013
    else if (stratum == 13) factor <- 0.030
    else if (stratum == 14) factor <- 0.070
    else if (stratum == 15) factor <- 0.016
    else if (stratum == 16) factor <- 0.074
    else if (stratum == 17) factor <- 0.268
    else if (stratum == 18) factor <- 0.055
    else if (stratum == 19) factor <- 0.216
    else if (stratum == 20) factor <- 0.256
  } else if (area == "MON") {
    if (stratum == 12) factor <- 0.119
    else if (stratum == 13) factor <- 0.262
    else if (stratum == 14) factor <- 0.619
  } else if (area == "BSB") {
    if (stratum == 15) factor <- 0.044
    else if (stratum == 16) factor <- 0.208
    else if (stratum == 17) factor <- 0.748
  } else if (area == "SBS") {
    if (stratum == 18) factor <- 0.104
    else if (stratum == 19) factor <- 0.410
    else if (stratum == 20) factor <- 0.486
  } else if (area == "SHR") {
    if (stratum == 12) factor <- 0.216
    else if (stratum == 13) factor <- 0.314
    else if (stratum == 14) factor <- 0.470
  } else if (area == "SIC") {
    if (stratum == 21) factor <- 0.043
    else if (stratum == 22) factor <- 0.496
    else if (stratum == 23) factor <- 0.462
  } else if (area == "CMH") {
    if (stratum == 24) factor <- 0.114
    else if (stratum == 25) factor <- 0.446
    else if (stratum == 26) factor <- 0.440
  } else if (area == "INS") {
    if (stratum == 12) factor <- 0.103
    else if (stratum == 15) factor <- 0.122
    else if (stratum == 18) factor <- 0.419
    else if (stratum == 21) factor <- 0.119
    else if (stratum == 24) factor <- 0.238
  } else if (area == "HOG") {
    if (stratum == 18) factor <- 0.104
    else if (stratum == 21) factor <- 0.030
    else if (stratum == 22) factor <- 0.345
    else if (stratum == 24) factor <- 0.059
    else if (stratum == 25) factor <- 0.232
    else if (stratum == 26) factor <- 0.229
  } else if (area == "PST") {
    factor <- 1
  }
  
  return(factor)
}

MACRO2 <- function(ABUND, LENG, TEMPLATE) {
  TEMPLATE$YEAR <- as.numeric(as.character(TEMPLATE$YEAR))
  
  ABUNDALL <- ABUND %>%
    mutate(YEAR = "ALLC")
  
  AMALL <- ABUNDALL %>%
    group_by(YEAR, STRATUM, AREA) %>%
    summarise(TN = sum(NUMBER, na.rm = TRUE),
              NM = mean(NUMBER, na.rm = TRUE),
              NV = var(NUMBER, na.rm = TRUE),
              NN = n(), .groups = 'drop')
  
  AMALL$FACTOR <- unname(unlist(Map(factor_assignment, AMALL$AREA, AMALL$STRATUM)))
  
  AM2ALL <-  AMALL %>% mutate(NM1 = FACTOR * NM, SNV1 = NV / NN * FACTOR^2) %>%
    group_by(YEAR) %>%
    summarise(NM2 = sum(NM1, na.rm = TRUE),
              SNV2 = sum(SNV1, na.rm = TRUE), .groups = 'drop')
  
  #AM2YALL <- AM2ALL %>% LOOK OUT HERE FOR NEEDING TO RENAME THIS DF
  #  select(-_FREQ_, -_TYPE_)
  
  AM3ALL <- ABUNDALL %>%
    group_by(YEAR) %>%
    summarise(TOTALN = sum(NUMBER, na.rm = TRUE), SAMPLES=n(), .groups = 'drop')
  
  YMEANSALL <- left_join(AM3ALL, AM2ALL, by = "YEAR") %>%
    mutate(SNVE = sqrt(SNV2)) %>%
    mutate(NMEAN = NM2) %>%
    select(YEAR, SAMPLES, TOTALN, NMEAN, SNVE)
  
  AM <- ABUND %>%
    group_by(YEAR, STRATUM, AREA) %>%
    summarise(TN = sum(NUMBER, na.rm = TRUE),
              NM = mean(NUMBER, na.rm = TRUE),
              NV = var(NUMBER, na.rm = TRUE),
              NN = n(),
              .groups = 'drop')
  AM$FACTOR <- unname(unlist(Map(factor_assignment, AM$AREA, AM$STRATUM)))
  # Create AMW dataset
  AM2 <- AM %>% mutate(NM1 = FACTOR * NM, SNV1 = NV / NN * FACTOR^2) %>%
    group_by(YEAR) %>%
    summarise(NM2 = sum(NM1, na.rm = TRUE),
              SNV2 = sum(SNV1, na.rm = TRUE),
              .groups = 'drop')
  
  # Merge datasets
  AM4 <- ABUND %>% group_by(YEAR) %>% summarise(TOTALN = sum(NUMBER, na.rm = TRUE),
                                                SAMPLES=n(),
                                                .groups = 'drop') %>%
    left_join(AM2, by = "YEAR") %>% mutate(NMEAN = NM2, SNVE = sqrt(SNV2)) %>%
    select(-c(NM2, SNV2))
  #YMEANSALL$YEAR <- as.integer(YMEANSALL$YEAR)
  # Append to YMEANS
  YMEANS <- rbind(YMEANSALL, AM4[,names(YMEANSALL)])
  
  AMM <- ABUND %>%
    group_by(CRUCODE, STRATUM, AREA) %>%
    summarise(TN = sum(NUMBER, na.rm = TRUE),
              NM = mean(NUMBER, na.rm = TRUE),
              NV = var(NUMBER, na.rm = TRUE),
              NN = n(), .groups = 'drop')

  AMM$FACTOR <- unname(unlist(Map(factor_assignment, AMM$AREA, AMM$STRATUM)))
  AM2M <- AMM %>% mutate(NM1 = FACTOR * NM, SNV1 = NV / NN * FACTOR^2) %>%
    group_by(CRUCODE) %>% summarise(NM2 = sum(NM1, na.rm = TRUE),
              SNV2 = sum(SNV1, na.rm = TRUE), .groups = 'drop') %>%
    mutate(CRUCODES = CRUCODE,
           CRUISE = substr(trimws(as.character(CRUCODES)), 5, 5),
           CRUISE = case_when(
             CRUCODE == 19882 ~ "4",
             CRUCODE == 19883 ~ "5",
             TRUE ~ CRUISE
           ),
           MONTH = case_when(
             CRUISE == "1" ~ "JANUARY",
             CRUISE == "2" ~ "APRIL",
             CRUISE == "3" ~ "JUNE",
             CRUISE == "4" ~ "AUGUST",
             CRUISE == "5" ~ "OCTOBER",
             CRUISE == "6" ~ "DECEMBER",
             TRUE ~ NA_character_
           )) #%>%
    #select(-_FREQ_, -_TYPE_)
  
  # Sort and summarize again
  AM3M <- ABUND %>%
    group_by(CRUCODE) %>%
    summarise(TOTALN = sum(NUMBER, na.rm = TRUE), SAMPLES = n(), .groups = 'drop')
  
  # Create MEANS
  MEANS <- left_join(AM3M, AM2M, by = "CRUCODE") %>%
    mutate(YEAR = substr(CRUCODES, 1, 4), SNVE = sqrt(SNV2)) %>%
    mutate(NMEAN = NM2) %>%
    select(-NM2, -SNV2)
  
  # Create MMEANS
  MMEANS <- MEANS %>%
    select(-CRUCODES, -CRUISE)
  
  # All strata combined
  STABUNDALL <- ABUND %>%
    mutate(STRATUM = 0) %>%
    arrange(STRATUM)
  
  STAMWALL <- STABUNDALL %>%
    group_by(STRATUM) %>%
    summarise(TN = sum(NUMBER, na.rm = TRUE),
              NM = mean(NUMBER, na.rm = TRUE),
              NV = var(NUMBER, na.rm = TRUE),
              NN = n(), .groups = 'drop') %>%
    mutate(FACTOR = 1,
           NM1 = FACTOR * NM,
           SNV1 = NV / NN * FACTOR^2) 
  
  STAM2ALL <- STAMWALL %>%
    group_by(STRATUM) %>%
    summarise(NM2 = sum(NM1, na.rm = TRUE),
              SNV2 = sum(SNV1, na.rm = TRUE), .groups = 'drop')

  # Arithmetic
  STYMEANSALL <- STABUNDALL %>%
    group_by(STRATUM) %>%
    summarise(TOTALN = sum(NUMBER, na.rm = TRUE), .groups = 'drop') %>% 
    left_join(STAM2ALL, by = "STRATUM") %>%
    mutate(SNVE = sqrt(SNV2))  %>% mutate(NMEAN = NM2, SAMPLES=STAMWALL$NN) %>%
    select(-NM2, -SNV2)

  # Calculate means for NM1 and SNV1
  STAM2 <- ABUND %>% group_by(STRATUM) %>%
    summarise(TN = sum(NUMBER, na.rm = TRUE), NM = mean(NUMBER, na.rm = TRUE),
              NV = var(NUMBER, na.rm = TRUE), NN = n()) %>%
    mutate(FACTOR = 1, NM1 = FACTOR * NM, SNV1 = NV / NN * FACTOR^2) %>%
    group_by(STRATUM) %>%
    summarise(NM2 = sum(NM1, na.rm = TRUE),
              SNV2 = sum(SNV1, na.rm = TRUE))
  
  # Prepare STAM2Y
  #STAM2Y <- STAM2 %>%
  #  select(-c(_FREQ_, _TYPE_))

  # Merge STAM3 and STAM2Y
  STAM4 <- ABUND %>%
    group_by(STRATUM) %>%
    summarise(TOTALN = sum(NUMBER, na.rm = TRUE), SAMPLES=n()) %>%
    left_join(STAM2, by = "STRATUM") %>%
    mutate(#SAMPLES = n(),
           NMEAN = NM2,
           SNVE = sqrt(SNV2)) %>%
    select(-c(NM2, SNV2))
  
  # Append to STYMEANS
  STYMEANS <- bind_rows(STYMEANSALL, STAM4) 
  ############LENGTHS######################
  # Frequency table
  SAMPLESALL <- ABUNDALL %>%
    count(YEAR, STRATUM, AREA, name="COUNT") %>%
    ungroup()

  # Merge data
  LENG2 <- TEMPLATE %>% bind_rows(LENG) %>%
    mutate(YEAR = "ALLC", LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
    group_by(YEAR, STRATUM, LENGA) %>%
    summarise(SUM = sum(FREQUENCY, na.rm = TRUE), .groups = 'drop') %>%
    inner_join(SAMPLESALL, by = c("YEAR", "STRATUM")) %>%
    filter(SUM > 0)
  
  LENG2$FACTOR <- unname(unlist(Map(factor_assignment, LENG2$AREA, LENG2$STRATUM)))
  
  # Transpose again
  TRANSY2AALL <- LENG2 %>%
    mutate(MEAN1 = FACTOR * (SUM / COUNT)) %>% group_by(YEAR, LENGA) %>%
    summarise(MEANS = sum(MEAN1, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = LENGA, values_from = MEANS) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    pivot_longer(-YEAR, names_to = "LENGTH", values_to = "MEANS")
  TRANSY2AALL$LENGTH <- as.integer(TRANSY2AALL$LENGTH)
  
  TRANSY3ALL <- TRANSY2AALL %>% group_by(YEAR) %>%
    complete(LENGTH = full_seq(c(1, max(LENGTH)), period = 1),
             fill = list(value = 0)) %>% ungroup() %>%
    pivot_wider(names_from = LENGTH, values_from = MEANS)
  
  # Final merge
  FINALYALL <- YMEANSALL %>%
    inner_join(TRANSY3ALL, by = "YEAR") %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    filter(YEAR != "1901")
  
  # PROC FREQ;
  # TABLE YEAR*STRATUM*AREA / NOPRINT OUT=SAMPLES;
  # NOPRINT means we only need the output dataset 'SAMPLES' which contains counts.
  SAMPLES <- ABUND %>%
    count(YEAR, STRATUM, AREA, name = "COUNT") # 'name' specifies the column name for the counts
  
  # DATA YTEMPLT;
  # SET TEMPLATE;
  ytemplt <- TEMPLATE %>% bind_rows(ytemplt, LENG)
  # PROC APPEND BASE=YTEMPLT DATA=LENG;
  # In R, this is simply row-binding.
  #ytemplt$YEAR <- as.integer(ytemplt$YEAR)
  
  # DATA LENGY;
  # LENGTH YEAR $4; # R handles types dynamically, no direct equivalent needed for length
  # LENGTH LENGA $3; # R handles types dynamically, no direct equivalent needed for length
  # SET YTEMPLT;
  # YEAR=(INT(CRUCODE*.1));
  # LENGA=LENGTH;
  # FREQUENCY=FREQUENCY/MINOUT*20;
  lengy <- LENG %>%
    mutate(
      YEAR = floor(CRUCODE * 0.1), # INT() in SAS is floor() in R for positive numbers
      LENGA = LENGTH, # Renaming 'LENGTH' to 'LENGA'
      FREQUENCY = FREQUENCY / MINOUT * 20
    ) %>%
    # Select and reorder columns to match SAS output structure if necessary,
    # but for functional equivalence, it's not strictly required here.
    select(-LENGTH) # Remove the original 'LENGTH' column if LENGA is its replacement
  
  # PROC SORT;
  # BY YEAR STRATUM LENGA;
  # Note: 'STRATUM' might not be in 'lengy' if it wasn't in TEMPLATE/LENG.
  # Assuming 'STRATUM' is a column that should exist from a previous step or join.
  # If 'STRATUM' is missing, this line will cause an error.
  # For this example, let's add a dummy STRATUM to lengy for demonstration.
  if (!"STRATUM" %in% colnames(lengy)) {
    lengy$STRATUM <- sample(LETTERS[1:3], nrow(lengy), replace = TRUE)
  }
  
  # PROC MEANS NOPRINT;
  # BY YEAR STRATUM LENGA;
  # VAR FREQUENCY;
  # OUTPUT OUT=LENGY1 SUM=SUM;
  lengy1 <- lengy %>% group_by(YEAR, STRATUM, LENGA) %>%
    summarise(SUM = sum(FREQUENCY, na.rm = TRUE), .groups = 'drop')
  
  # DATA LENG2;
  # MERGE LENGY1 SAMPLES;
  # BY YEAR STRATUM;
  # IF SUM>.; # In SAS, '.' is missing. SUM>. means SUM is not missing.
  leng2 <- left_join(lengy1, SAMPLES, by = c("YEAR", "STRATUM")) %>%
    filter(!is.na(SUM)) # Equivalent to IF SUM>. in SAS
  
  # %MACRO1;
  # MEAN1=FACTOR*(SUM/COUNT);
  # This calculation is applied directly to the 'leng2' data frame.
  # Assuming 'FACTOR' is a column in 'leng2' (which came from TEMPLATE/LENG via ytemplt).
  leng2$FACTOR <- unname(unlist(Map(factor_assignment, leng2$AREA, leng2$STRATUM)))
  
  # --- Displaying results (optional) ---
  
  print("lengy (first few rows):")
  print(head(lengy))
  
  print("leng2 (first few rows):")
  print(head(leng2))
  
  # Replace NA with 0
  ZEROSY <- leng2 %>% mutate(MEAN1 = FACTOR * (SUM / COUNT)) %>% 
    group_by(YEAR, LENGA) %>%
    summarise(MEANS = sum(MEAN1, na.rm = TRUE), .groups = 'drop') %>% #this matches! :) (need to flesh out/expand colnames)
    group_by(YEAR) %>%
    complete(LENGA = full_seq(c(1, max(LENGA)), period = 1),
             fill = list(value = 0)) %>%
    pivot_wider(names_from = LENGA, values_from = MEANS) %>% #matches!
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))
  
  # Transpose ZEROSY 
  TRANSY2 <- ZEROSY %>% #MATCHES
    pivot_longer(cols = -YEAR, names_to = "NAME", values_to = "MEANS") 
  
  # Merge with AM4 and filter
  FINALY1 <- AM4 %>% #matches here! LINE 619
    full_join(ZEROSY, by = "YEAR") %>%
    filter(YEAR != "1901") %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))
  
  # Assuming FINALYALL is a data frame in R
  FINALY <- rbind(FINALYALL, FINALY1) %>% #LINE 628 matches!
    mutate(
      YEAR = as.character(YEAR),
      SAMPLES = as.numeric(SAMPLES),
      TOTALN = as.numeric(TOTALN),
      NMEAN = as.numeric(NMEAN),
      SNVE = as.numeric(SNVE)
    )
  
  SAMPLES <- ABUND %>%
    count(CRUCODE, STRATUM, AREA, name = "COUNT")

  LENGA <-  bind_rows(TEMPLATE, LENG) %>%
    mutate(
      LENGA = as.character(LENGTH),
      FREQUENCY = FREQUENCY / MINOUT * 20
    )
  
  LENG1 <- LENGA %>%
    group_by(CRUCODE, STRATUM, LENGA) %>%
    summarise(SUM = sum(FREQUENCY, na.rm = TRUE), .groups = 'drop')
  
  SAMPLES$FACTOR <- unname(unlist(Map(factor_assignment, SAMPLES$AREA, SAMPLES$STRATUM)))
  
  ZEROS <- LENG1 %>% full_join(SAMPLES, by = c("CRUCODE", "STRATUM")) %>%
    filter(SUM > 0) %>% mutate(MEAN1 = FACTOR * (SUM / COUNT)) %>%
    group_by(CRUCODE, LENGA) %>%
    summarise(MEANS = sum(MEAN1, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = LENGA, values_from = MEANS) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))
  
  TRANS2 <- ZEROS %>% #pick back up here! 7/30 probably similar fix as above...
    pivot_longer(cols = -CRUCODE, names_to = "NAME", values_to = "MEANS") #%>%
  
  FINAL <- MEANS %>%
    full_join(ZEROS, by = "CRUCODE") %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
    filter(CRUCODE != 19011) %>%
    select(-CRUCODE, -CRUCODES)
  
  print(FINAL) #matches!

  STSAMPLESALL <- STABUNDALL %>% count(STRATUM, name="COUNT")

  STLENG2 <- bind_rows(TEMPLATE, LENG) %>%
    mutate(STRATUM = 0, LENGA = as.character(LENGTH), FREQUENCY = FREQUENCY / MINOUT * 20) %>%
    group_by(STRATUM, LENGA) %>%
    summarise(SUM = sum(FREQUENCY, na.rm = TRUE), .groups = 'drop') %>%
    full_join(STSAMPLESALL, by = "STRATUM") %>%
    filter(SUM > 0) %>%
    mutate(FACTOR = 1, MEAN1 = FACTOR * (SUM / COUNT)) %>%
    group_by(STRATUM, LENGA) %>%
    summarise(MEANS = sum(MEAN1, na.rm = TRUE), .groups = 'drop')
  
  # Transpose the data
  STTRANSY3ALL <- STLENG2 %>%
    pivot_wider(names_from = LENGA, values_from = MEANS) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>% #matches!
    pivot_longer(cols = -STRATUM, names_to = "LENGTH", values_to = "MEANS") %>%
    pivot_wider(names_from = LENGTH, values_from = MEANS) #line 760
  
  STFINALYALL <- STYMEANSALL %>%
    full_join(STTRANSY3ALL, by = "STRATUM") %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
  print(STFINALYALL) #MATCHES!
  
  #BY STRATUM - NOT WEIGHTED BY STRATUM FACTORS*******************;
  # Frequency table
  STSAMPLES <- ABUND %>%
    group_by(STRATUM) %>%
    summarise(COUNT = n(), .groups = 'drop') %>%
    select(STRATUM, COUNT)
  
  # Append data
  STYTEMPLT <- TEMPLATE
  
  # Sort and transpose again
  STTRANSY3 <- bind_rows(STYTEMPLT, LENG) %>%
    mutate(LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
    group_by(STRATUM, LENGA) %>% summarise(SUM = sum(FREQUENCY),
                                           .groups = 'drop') %>% #MATCHES
    left_join(STSAMPLES, by = "STRATUM") %>% filter(SUM > 0) %>%
    mutate(FACTOR = 1, MEAN1 = FACTOR * (SUM / COUNT)) %>%
    group_by(STRATUM, LENGA) %>%
    summarise(MEANS = sum(MEAN1), .groups = 'drop') %>%
    pivot_wider(names_from = LENGA, values_from = MEANS) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    pivot_longer(-STRATUM, names_to = "LENGTH", values_to = "MEANS")  %>%
    pivot_wider(names_from = LENGTH, values_from = MEANS)
  
  # Final merge and cleanup
  STFINALY1 <- STAM4 %>%
    left_join(STTRANSY3, by = "STRATUM") %>%
    filter(STRATUM != 0) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
  # Final data structure
  STFINALY <- STFINALYALL %>%
    mutate(STRATUM = as.integer(STRATUM),
           SAMPLES = as.integer(SAMPLES),
           TOTALN = as.integer(TOTALN),
           NMEAN = as.integer(NMEAN),
           SNVE = as.integer(SNVE))
  
  # Append final data
  STFINALY <- bind_rows(STFINALY, STFINALY1)
  
  #BY YEAR - WEIGHTED BY STRATUM FACTORS***STRAIGHT LFD, NOT CPUE****************;
  
  # Create LFWSA
  LFWSA <- ABUNDALL %>%
    count(STRATUM, AREA, name="COUNT") %>%
    ungroup() %>%
    mutate(PERCENT = COUNT / sum(COUNT) * 100)  %>%
    mutate(YEAR = "ALLC") %>%
    select(-PERCENT, -COUNT)
  LFWSA$FACTOR <- unname(unlist(Map(factor_assignment, LFWSA$AREA, LFWSA$STRATUM)))
  
  # Replace NA with 0
  LFWZEROSYALL <- bind_rows(TEMPLATE, LENG) %>%
    mutate(YEAR = "ALLC", LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
    group_by(YEAR, STRATUM, LENGA) %>%
    summarise(SUM = sum(FREQUENCY, na.rm = TRUE), .groups = 'drop') %>%
    inner_join(LFWSA, by = c("YEAR", "STRATUM")) %>%
    filter(SUM > 0) %>%
    mutate(WSUM = FACTOR * SUM) %>%
    group_by(YEAR, LENGA) %>%
    summarise(WSUMSALL = sum(WSUM, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = LENGA, values_from = WSUMSALL) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
  # Final merge
  LFWFINALYALL <- YMEANSALL %>% #MATCHES
    left_join(LFWZEROSYALL, by = "YEAR") %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% select(-NMEAN, -SNVE)
  
  # Frequency table
  YSA <- ABUND %>%
    count(YEAR, STRATUM, AREA) %>%
    ungroup()
  
  # Create YLSTTEMPLT
  YLSTTEMPLT <- TEMPLATE
  
  # Append data
  YLSTTEMPLT$YEAR <- as.integer(YLSTTEMPLT$YEAR)
  
  YLSTLENGY1 <- bind_rows(YLSTTEMPLT, LENG) %>%
    mutate(LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
    group_by(YEAR, STRATUM, LENGA) %>%
    summarise(SUM = sum(FREQUENCY, na.rm = TRUE), .groups = 'drop')
  
  # Merge data
  YLSTLENG2 <- left_join(YSA, YLSTLENGY1, by = c("YEAR", "STRATUM")) %>%
    filter(SUM > 0)
  
  YLSTLENG2$FACTOR <- unname(unlist(Map(factor_assignment, YLSTLENG2$AREA, YLSTLENG2$STRATUM)))
  
  # Replace NA with 0
  YLSTZEROSY <- YLSTLENG2 %>%
    mutate(STSUM = FACTOR * SUM) %>%
    group_by(YEAR, LENGA) %>%
    summarise(STSUMS = sum(STSUM, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = LENGA, values_from = STSUMS) %>%
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    filter(YEAR != "1901")
  
  # Final merge
  YLSTFINALY1 <- left_join(AM4, YLSTZEROSY, by = "YEAR") %>%
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    select(-NMEAN, -SNVE)
  
  # Final data structure
  YLSTFINALY <- rbind(LFWFINALYALL, YLSTFINALY1) 

  #BY YEAR - NOT WEIGHTED BY STRATUM FACTORS***STRAIGHT LFD, NOT CPUE****************;
  #NOT TESTED
  
  # Assuming ABUNDALL, TEMPLATE, LENG, and AM4 are data frames in R
  
  # Create LFALLTOTAL data frame
  LFALLTOTAL <- ABUNDALL %>%
    summarise(N = n(), SUM = sum(NUMBER, na.rm = TRUE)) %>%
    mutate(YEAR = "ALLC") #%>%
  
  # Replace NA with 0
  LFZEROSYALL <- bind_rows(TEMPLATE, LENG) %>%
    mutate(YEAR = "ALLC", LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
    group_by(YEAR, LENGA) %>%
    summarise(SUM = sum(FREQUENCY, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = LENGA, values_from = SUM) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
  # Merge LFALLTOTAL and LFZEROSYALL
  LFFINALYALL <- full_join(LFALLTOTAL, LFZEROSYALL, by = "YEAR") %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
  # Create YLSAMPLES data frame
  YLSAMPLES <- ABUND %>%
    count(YEAR)
  
  # Append LENG to TEMPLATE
  TEMPLATE$YEAR <- as.integer(TEMPLATE$YEAR)
  
  # Sort and summarize by YEAR and LENGA
  YLLENGY1 <- bind_rows(TEMPLATE, LENG) %>%
    mutate(LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
    group_by(YEAR, LENGA) %>%
    summarise(SUM = sum(FREQUENCY, na.rm = TRUE)) %>%
    ungroup()
  
  # Merge YLLENGY1 and YLSAMPLES
  YLLENGY2 <- inner_join(YLLENGY1, YLSAMPLES, by = "YEAR") %>%
    filter(!is.na(SUM))
  
  df <- data.frame(
    YEAR = 1901,
    SUM = 0, 
    n = 0
  )
  df$LENGA <- list(c(1:160))
  expanded_df <- unnest(df, LENGA)
  
  YLLENGY2 <- rbind(expanded_df, YLLENGY2) %>% complete(YEAR, LENGA, fill = list(SUM = 0, n = 0)) 
  YLLENGY2 <- YLLENGY2[YLLENGY2$YEAR > 1901,]
  
  # Replace NA with 0 and remove specific year
  YLZEROSY <- YLLENGY2 %>%
    pivot_wider(names_from = LENGA, values_from = SUM) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    filter(YEAR != "1901")
  
  # Merge AM4 and YLZEROSY
  YLFINALY1 <- full_join(AM4, YLZEROSY, by = "YEAR") %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    select(-NMEAN, -SNVE, -n)
  
  # Create YLFINALY data frame
  YLFINALY <- LFFINALYALL %>%
    mutate(YEAR = as.character(YEAR), TOTALN = SUM, SAMPLES = N) %>%
    select(-N, -SUM)
  
  # Append YLFINALY1 to YLFINALY
  YLFINALY <- rbind(YLFINALY, YLFINALY1)

return(list(FINALY, STFINALY, FINAL, YLFINALY, YLSTFINALY))} #finaly STFINALY

LFD <- function(mypath, myspp, area="ALL", cruise="ALL", outdir) {
  value_map <- c(
    "Black drum" = "PC",
    "Lobster - F (GE53mm)" = "HA53F",
    "Lobster - M (GE53mm)" = "HA53M",
    "Scup" = "SC",
    "Spot" = "LX",
    "Summer flounder" = "PD",
    "Tautog" = "TO",
    "Weakfish" = "CR",
    "Atl croaker" = "MU",
    "Striped bass" = "MS"
  )
  FIRST <- read.dbf(file.path(mypath, paste0(value_map[myspp],"ABUN.dbf")))
  
  if (myspp == "Black drum") {
    SECOND <- read.dbf(file.path(mypath, "PCLENG.dbf"))
  } else if (myspp == "Lobster - F (GE53mm)") {
    #LOBSTER
    #Female index & catch at length (CAL), GE 53mm+ (i.e., lengths equal to 53 mm and greater)
    SECOND <- read.dbf(file.path(mypath, "HAXLENG.dbf"))
    SECOND <- SECOND[SECOND$SEX==2 & SECOND$LENGTH >= 53,]	
    SECOND$SEX <- NULL
  } else if (myspp == "Lobster - M (GE53mm)") {
    #Male index & catch at length (CAL), GE 53mm+ (i.e., lengths equal to 53 mm and greater)
    SECOND <- read.dbf(file.path(mypath, "HAXLENG.dbf"))
    SECOND <- SECOND[SECOND$SEX==1 & SECOND$LENGTH >= 53,]	
    SECOND$SEX <- NULL
  } else if (myspp=="Scup") {
    SECOND <- read.dbf(file.path(mypath, "SCLENG.dbf"))
  } else if (myspp=="Spot") {
    SECOND <- read.dbf(file.path(mypath, "LXLENG.dbf"))
  } else if (myspp=="Summer flounder") {
    SECOND <- read.dbf(file.path(mypath, "PDLENG.dbf"))
  } else if (myspp=="Tautog") {
    SECOND <- read.dbf(file.path(mypath, "TOLENG.dbf"))
  } else if (myspp=="Weakfish") {
    SECOND <- read.dbf(file.path(mypath, "CRLENG.dbf"))
  } else if (myspp=="Atl croaker") {
    SECOND <- read.dbf(file.path(mypath, "MULENG.dbf"))
  } else if (myspp=="Striped bass") {
    SECOND <- read.dbf(file.path(mypath, "MSLENG.dbf"))
  }
  
  LENG1 <- SECOND #%>% select(-COMMON, -LATIN)
  #need more code for lobster...
  LENG1$CRUCODE[LENG1$CRUCODE==19882] <- 19884
  LENG1$CRUCODE[LENG1$CRUCODE==19883] <- 19885 #LENGS
  
  FIRST$CRUCODE[FIRST$CRUCODE==19882] <- 19884
  FIRST$CRUCODE[FIRST$CRUCODE==19883] <- 19885
  FIRST$CRUISE <- substr(FIRST$CRUCODE,5,5)
  FIRST$NUMBER <- FIRST$NUMBER/FIRST$MINOUT*20 #ABUN
  
  ABUND <- FIRST %>% mutate(AREA="ALL")
  LENG <- LENG1 %>% select(-TOW)
  LENG$CRUISE <- substr(LENG$CRUCODE,5,5)
  
  if(area=="INM") {
    ABUND <- ABUND[!ABUND$STRATUM %in% c(14,17,20,23,26),]
    ABUND$AREA <- "INM"
    LENG <- LENG[!LENG$STRATUM %in% c(14,17,20,23,26),]
  }
  
  cruiseno <- 1:6
  if(cruise=="AprOct") {
    ABUND <- ABUND[ABUND$YEAR > 1988,]
    LENG <- LENG[LENG$YEAR > 1988,]
    cruiseno <- c(2,5)
  } else if (cruise=="AugOct") {
    cruiseno <- 4:5
  } else if (cruise=="Oct") {
    cruiseno <- 5
  }  else if(cruise=="AprthruOct") {
    cruiseno <- 2:5
    ABUND <- ABUND[ABUND$YEAR > 1988,]
    LENG <- LENG[LENG$YEAR > 1988,]
  } else if (cruise=="Spring") {
    cruiseno <- 2:3
  } else if (cruise=="Apr") {
    cruiseno <- 2
  }
  ABUND <- ABUND[ABUND$CRUISE %in% cruiseno,]
  LENG <- LENG[LENG$CRUISE %in% cruiseno,]
  ABUND$TOW <- NULL
  TEMPLATE <- read.dbf(file.path(mypath, "LTEMPLTE.dbf"))
  out <- MACRO2(ABUND, LENG, TEMPLATE)
  myfile <- paste(myspp, paste0("strata",area), paste0("cru",cruise), sep="_")
  annual <- out[[1]]
  annual_yrs <- annual[-1, ]
  annual_yrs$YEAR <- as.integer(annual_yrs$YEAR)
  annual_yrs <- annual_yrs %>%
    complete(YEAR = full_seq(c(1988, max(YEAR)), period = 1), # Fills sequence based on min/max in group
             fill = list(value = NA)) %>% # Fills new value columns with NA
    ungroup()
  annual <- rbind(annual[1,], annual_yrs)
  #annual$month <- cruise
  #annual$strata <- area
  #write.csv(out[[2]], file = file.path(outdir, paste(myfile, "CAL-STRATA.csv", sep="_")), row.names=F)
  write.csv(annual, file = file.path(outdir, paste(myfile, "CAL-ANNUAL.csv", sep="_")), row.names=F)
return(out)}

# --- 0. Set up dummy data for demonstration ---
# Replace these with your actual data loading
#set.seed(123) #you'll have to check data names, setups etc

#ABUNDALL <- data.frame(
#  YEAR = rep(c("2020", "2021"), each = 10),
#  STRATUM = rep(c("A", "B"), 10),
#  AREA = rep(c("North", "South", "East", "West"), 5),
#  FREQUENCY = runif(20, 10, 100),
#  FACTOR = runif(20, 0.5, 2) # Adding FACTOR for later steps
#)

#-- End of dummy data setup ---

# --- 1. Create dummy dataframes (replace with your actual data loading) ---
# Assuming LENGY3ALL has columns: YEAR (character), LENGA (numeric), MEANS (numeric)
# Assuming YMEANSALL has columns: YEAR (character) and other relevant data
# These are placeholder dataframes to make the R code runnable.
#LENGY3ALL <- tibble(
#  YEAR = c("1900", "1900", "1900", "1901", "1901", "1901", "1902", "1902"),
#  LENGA = c(1, 2, 3, 1, 2, 3, 1, 2),
#  MEANS = c(10.1, 20.2, 30.3, 11.1, NA, 31.3, 12.1, 22.2)
#)

#YMEANSALL <- tibble(
#  YEAR = c("1900", "1901", "1902", "1903"),
#  OTHER_VAR1 = c(100, 200, 300, 400),
#  OTHER_VAR2 = c(10, 20, 30, 40)
#)

#October = 5
#Aug & Oct = 4:5
#Apr & Oct = c(2,5)
#"Atl croaker","Black drum","Horseshoe crab","Lobster - F (GE53mm)","Lobster - M (GE53mm)","Scup","Spot","Summer flounder")
#mypath <- "C:/Users/jgorzo/Documents/SASLoads"
#sppdata <- Spp(mypath, "Lobster - F (GE53mm)")
#sppdata <- Spp(mypath, "Lobster - M (GE53mm)")

#output: FINALY, STFINALY, FINAL, YLFINALY, YLSTFINALY