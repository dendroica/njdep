library(foreign)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

# FYI you haven't done this part yet
# **********FOR SIZED LOBSTER ONLY WHEN USING HASabun***********************;
#  **********FOR LEGAL SIZED LOBSTER;
#SET first;
#NUMBER=LEGALN;
#RUN;
#  **********FOR SUBLEGAL SIZED LOBSTER;
#SET first;
#NUMBER=SUBLEGALN;
#RUN;
#  **********FOR ALL LOBSTER WHEN USING HASabun;
#SET first;
#NUMBER=TOTALN;
#RUN;

Macro20 <- function(abund, timescale = "month") {
  if (timescale == "season" | timescale=="quarter") { #Macro20
    abund <- abund %>% mutate(
      CRUCODE = case_match(CRUCODE,
                           19896 ~ 19895,
                           .default = CRUCODE))
    crucode <- as.character(abund$CRUCODE)
    crucode <- str_replace(crucode, "3$", "2") # '$' anchors to end
    # Convert back to numeric (handling potential errors if input wasn't purely digits)
    abund$CRUCODE <- as.numeric(crucode)
  } #you would also have to re-code the CRUISE from 3 to 2 accordingly
return(abund)}

#setups here would have to be a data frame of NUMBER,WEIGHT
Macro6 <- function(setups) {
  tsam <- setups %>%
    summarise(TN = sum(NUMBER, na.rm=T), #10469.680381
              NM = mean(NUMBER, na.rm=T), 
              NMSE = sd(NUMBER, na.rm=T) / sqrt(n()),
              NV = var(NUMBER, na.rm=T),
              NN = n(), WN = n())
  if("WEIGHT" %in% colnames(setups)) {
    weight  <- setups %>% summarise(TW = sum(WEIGHT, na.rm=T), 
                                    WM = mean(WEIGHT, na.rm=T),
                                    WMSE = sd(WEIGHT, na.rm=T) / sqrt(n()),
                                    WV = var(WEIGHT, na.rm=T))
    tsam <- cbind(tsam, TW=weight$TW, WM=weight$WM, WMSE=weight$WMSE, WV=weight$WV)
  }
  
  tsamw <- tsam %>%
    mutate(SAMPLES = NN) 
  return(tsamw)}

# Function to assign factor (stratum weight) values
Macro3 <- function(AREA, STRATUM) {
  factor <- NA
  if (AREA == "ALL") {
    if (STRATUM == 12) factor <- 0.008
    else if (STRATUM == 13) factor <- 0.018
    else if (STRATUM == 14) factor <- 0.043
    else if (STRATUM == 15) factor <- 0.010
    else if (STRATUM == 16) factor <- 0.045
    else if (STRATUM == 17) factor <- 0.164
    else if (STRATUM == 18) factor <- 0.033
    else if (STRATUM == 19) factor <- 0.132
    else if (STRATUM == 20) factor <- 0.156
    else if (STRATUM == 21) factor <- 0.010
    else if (STRATUM == 22) factor <- 0.111
    else if (STRATUM == 23) factor <- 0.103
    else if (STRATUM == 24) factor <- 0.019
    else if (STRATUM == 25) factor <- 0.075
    else if (STRATUM == 26) factor <- 0.074
  } else if (AREA == "INM") {
    if (STRATUM == 12) factor <- 0.018
    else if (STRATUM == 13) factor <- 0.039
    else if (STRATUM == 15) factor <- 0.021
    else if (STRATUM == 16) factor <- 0.099
    else if (STRATUM == 18) factor <- 0.073
    else if (STRATUM == 19) factor <- 0.286
    else if (STRATUM == 21) factor <- 0.021
    else if (STRATUM == 22) factor <- 0.241
    else if (STRATUM == 24) factor <- 0.041
    else if (STRATUM == 25) factor <- 0.162
  } else if (AREA == "SOU") {
    if (STRATUM == 21) factor <- 0.024
    else if (STRATUM == 22) factor <- 0.284
    else if (STRATUM == 23) factor <- 0.264
    else if (STRATUM == 24) factor <- 0.049
    else if (STRATUM == 25) factor <- 0.191
    else if (STRATUM == 26) factor <- 0.188
  } else if (AREA == "NOR") {
    if (STRATUM == 12) factor <- 0.013
    else if (STRATUM == 13) factor <- 0.030
    else if (STRATUM == 14) factor <- 0.070
    else if (STRATUM == 15) factor <- 0.016
    else if (STRATUM==16) factor <- 0.074
    else if (STRATUM==17) factor <- 0.268
    else if (STRATUM==18) factor <- 0.055
    else if (STRATUM==19) factor <- 0.216
    else if (STRATUM==20) factor <- 0.256
  } else if (AREA=="SHR") {
    if (STRATUM==12) factor <- 0.216
    else if (STRATUM==13) factor <- 0.314
    else if (STRATUM==14) factor <- 0.470
  } else if (AREA=="MON") {
    if (STRATUM==12) factor <- 0.119
    else if (STRATUM==13) factor <- 0.262
    else if (STRATUM==14) factor <- 0.619
  } else if (AREA=="BSB") {
    if (STRATUM==15) factor <- 0.044
    else if (STRATUM==16) factor <- 0.208
    else if (STRATUM==17) factor <- 0.748
  } else if (AREA=="SBS") {
    if (STRATUM==18) factor <- 0.104
    else if (STRATUM==19) factor <- 0.410
    else if (STRATUM==20) factor <- 0.486
  } else if (AREA=="SIC") {
    if (STRATUM==21) factor <- 0.043
    else if (STRATUM==22) factor <- 0.496
    else if (STRATUM==23) factor <- 0.462
  } else if (AREA=="CMH") {
    if (STRATUM==24) factor <- 0.114
    else if (STRATUM==25) factor <- 0.446
    else if (STRATUM==26) factor <- 0.440
  } else if (AREA=="INS") {
    if (STRATUM==12) factor <- 0.103
    else if (STRATUM==15) factor <- 0.122
    else if (STRATUM==18) factor <- 0.419
    else if (STRATUM==21) factor <- 0.119
    else if (STRATUM==24) factor <- 0.238
  } else if (AREA=="HOG") {
    if (STRATUM==18) factor <- 0.104
    else if (STRATUM==21) factor <- 0.030
    else if (STRATUM==22) factor <- 0.345
    else if (STRATUM==24) factor <- 0.059
    else if (STRATUM==25) factor <- 0.232
    else if (STRATUM==26) factor <- 0.229
  } else if(AREA=="PST") factor <- 1			
  
  return(factor)
}

# Function to apply factor (stratum weight) to means, variance, and standard errors
Macro2 <- function(tsamw, factor) { #Macro3 goes in here
  tsamw$NM <- factor * tsamw$NM
  if("WM" %in% colnames(tsamw)) {
    tsamw$WM <- factor * tsamw$WM
    tsamw$WMSE <- tsamw$WV / tsamw$WN * factor^2
  }
  tsamw$NMSE <- tsamw$NV / tsamw$NN * factor^2
  return(data.frame(tsamw))
}

# MACRO 7
Macro7 <- function(tsamw) { #you get this from Macro6
  factor <- unname(unlist(Map(Macro3, tsamw$AREA, tsamw$STRATUM)))
  tsamw <- Macro2(tsamw, factor) 
  #setups <- tsamw %>% select(-NN, -WN)
return(tsamw)}

# MACRO 8
Macro8 <- function(tsamw) { 
#tsamw <- Macro7(tsamw)  
tssum <- tsamw %>%
  summarise(SAMPLET = sum(SAMPLES), NMSUM = sum(NM), 
            NMSESUM1 = sum(NMSE), 
            TNSUM = sum(TN))
if("WM" %in% colnames(tsamw)) {
  weight <- tsamw %>% summarise(WMSUM = sum(WM), WMSESUM1 = sum(WMSE), 
                                TWSUM = sum(TW))
  tssum <- cbind(tssum, WMSUM=weight$WMSUM, WMSESUM1=weight$WMSESUM1, TWSUM=weight$TWSUM)
}

tssum2 <- tssum %>%
  mutate(SAMPLES = SAMPLET, 
         NMSESUM = sqrt(NMSESUM1))

if("WMSESUM1" %in% colnames(tssum)) {
  weight <- tssum2 %>% mutate(WMSESUM = sqrt(WMSESUM1))
  tssum2 <- cbind(tssum2, WMSESUM=weight$WMSESUM)
}

setups <- tssum2 %>%
  select(-NMSESUM1) #-WMSESUM1
}


Posstrat <- function(grouping, spp) {
  spp <- spp %>%
    arrange(across(all_of(grouping)))
  #superseded <- mtcars %>%
  #filter(disp < 160) %>%
  #  group_by(across(all_of(cols))) %>%
  #  summarise(n = n(), .groups = 'drop_last')
  POSSTRAT <- spp %>%
    group_by(across(all_of(grouping))) %>%
    summarise(SPTOW = n(), .groups = 'drop')
  
  stratsp <- POSSTRAT %>%
    select(all_of(c("SPTOW", grouping))) %>% drop_na(all_of(grouping))
}

#the proc means doesn't mean anything, so much as the "by"
#so for example...
#
#PROC SORT DATA=abund;
#BY STRATUM;
#PROC MEANS;
#BY STRATUM;
#%MACRO6; *CALCULATE MEAN AND VARIANCE;
#
#is the same as...
#tsamw <- abund %>% group_by(across(all_of(c("STRATUM")))) %>% Macro6

Yr <- function(abund, stratsp, grouping) {
  abund <- abund %>%
    arrange(across(all_of(c(grouping, "STRATUM", "AREA"))))
  
  setups <- abund %>% 
    group_by(across(all_of(c(grouping, "STRATUM", "AREA")))) %>% Macro6 %>%
    Macro7 %>% group_by(across(all_of(grouping))) %>% Macro8
  
  YAM2 <- merge(setups, stratsp, by=grouping, all = TRUE) %>% mutate(STRATUM = 0,
                             #SPTOW = n(),
                             PERCENTF = ifelse(SAMPLES > 0, SPTOW / SAMPLES, 0),
                             TOTALN = TNSUM,
                             STRATNM = NMSUM,
                             STRATNSE = NMSESUM) %>% 
    mutate(across(where(is.numeric), ~replace_na(.,0)))
  
  if("TWSUM" %in% colnames(YAM2)) {
    YAM2 <- YAM2 %>% mutate(
      TOTALW = TWSUM,
      STRATWM = WMSUM,
      STRATWSE = WMSESUM
    )
  }
  
  if("CRUCODE" %in% colnames(YAM2)) {
    YAM2$CRUISE <- substr(YAM2$CRUCODE,5,5)
  }
  
  if("CRUISE" %in% colnames(YAM2)) {
    YAM2 <- YAM2 %>% mutate(
      MONTH = case_match(CRUISE,
                         "1" ~ "JANUARY",
                         "2" ~ "APRIL",
                         "3" ~ "JUNE",
                         "4" ~ "AUGUST",
                         "5" ~ "OCTOBER",
                         "6" ~ "DECEMBER",
                         .default = CRUISE)) #%>% #LINE 458 DROPS?
  }
                             
    #SAMPLES = 0, #fill samples here?
    #PERCENTF = 0, 
    #TOTALN = 0, STRATNM = 0, STRATNSE = 0, 
    #TOTALW = 0, STRATWM = 0, STRATWSE = 0) %>% select(-STRAT) %>% #line 368 drop more for TSYSTRATS?
  
  tsamw <- abund %>%
    arrange(across(all_of(c("STRATUM", "AREA"))))
  setups <- tsamw %>% group_by(across(all_of(c("STRATUM", "AREA")))) %>%
    Macro6 %>% Macro7 %>% Macro8
  
  TSSTRATA <- setups %>% mutate(
    YEAR="Grand Total",
    STRATUM=0,
    SAMPLES=setups$SAMPLES,
    TOTALN = tsamw$TNSUM,
    TOTALW = tsamw$TWSUM,
    STRATNM = tsamw$NMSUM,
    STRATNSE = tsamw$NMSESUM,
    STRATWM = tsamw$WMSUM, #this still works?
    STRATWSE = tsamw$WMSESUM,
    MONTHN="1") %>% mutate(across(where(is.numeric), ~replace_na(.,0)))
  #SAMPLES = 0, #fill samples here?
  #PERCENTF = 0, 
  #TOTALN = 0, STRATNM = 0, STRATNSE = 0, 
  #TOTALW = 0, STRATWM = 0, STRATWSE = 0)
return(list(TSSTRATA, YAM2))
}

# MACRO 9
#grouping <- "STRATUM"
Macro9 <- function(spp, abund) {
#*BY STRATUM BY STRATUM BY STRATUM;

  # MACRO 4
template <- data.frame(YEAR = character(), MONTH = character(), STRATUM = numeric(), 
                      SAMPLES = numeric(), SPTOW = numeric(), PERCENTF = numeric(), 
                      TOTALN = numeric(), STRATNM = numeric(), STRATNSE = numeric(), 
                      TOTALW = numeric(), STRATWM = numeric(), STRATWSE = numeric(), 
                      MONTHN = character(), stringsAsFactors = FALSE)

for (grouping in c("YEAR")) { #c("YEAR", "STRATUM", "CRUISE", "CRUCODE"),
tsamw_all <- Macro6(abund)
tsamw_all$PERCENTF = ifelse(tsamw_all$SAMPLES > 0, tsamw_all$NN / tsamw_all$SAMPLES, 0)
stratsp <- Posstrat(grouping, spp)

TSsppS2 <- spp %>%
  summarise(SPTOW = n(), .groups = 'drop') %>%
  mutate(YEAR = "Grand Total") %>%
  select(YEAR, SPTOW)

if(all(grouping %in% c("YEAR"))) { #, "CRUISE", "CRUCODE"
 TSSTRATs <- Yr(abund, stratsp, grouping)
 TSSTRATA <- TSSTRATs[[1]]
 YAM2 <- TSSTRATs[[2]]
 
 TSSTRATB <- merge(TSSTRATA,TSsppS2, by = "YEAR", all=T) %>% mutate(
   PERCENTF = ifelse(SAMPLES > 0, SPTOW / SAMPLES, 0),
   MONTH = "Grand Total"
 ) %>% select(-MONTHN, -STRATUM)
 
 if(grouping=="YEAR") {
 YSTRAT <- TSSTRATB %>% select(-MONTH) 
 YSTRAT <- rbind(YAM2[,which(colnames(YAM2) %in% colnames(YSTRAT))], YSTRAT) %>%
   rename(TOTALN = TNSUM, STRATNM = NMSUM, STRATNSE=NMSESUM)
 keepercols <- c("YEAR", "SAMPLES", "SPTOW", "PERCENTF", "TOTALN", "STRATNM", "STRATNSE")
 if("TWSUM" %in% colnames(YSTRAT)) {
   YSTRAT <- YSTRAT %>% rename(TOTALW = TWSUM, STRATWM = WMSUM, STRATWSE = WMSESUM)
   keepercols <- c(keepercols, c("TOTALW", "STRATWM", "STRATWSE"))
 }
 YSTRAT <- YSTRAT[,keepercols]
 }
 
 if(grouping=="CRUISE") {
 MSTRAT <- TSSTRATB %>% select(-YEAR) #SPTOW Calc differs here...
 MSTRAT <- rbind(YAM2[,which(colnames(YAM2) %in% colnames(MSTRAT))], MSTRAT) 
 MSTRAT <- MSTRAT %>%
   rename(TOTALN = TNSUM, STRATNM = NMSUM, STRATNSE=NMSESUM, TOTALW = TWSUM, STRATWM = WMSUM, STRATWSE = WMSESUM)
 MSTRAT <- MSTRAT[,c("MONTH", "SAMPLES", "SPTOW", "PERCENTF", "TOTALN", "STRATNM", "STRATNSE", "TOTALW", "STRATWM", "STRATWSE")]
 }
 
 if(grouping=="CRUCODE") {
 YMSTRATA <- TSSTRATB %>% mutate(MONTH="All") #%>% bind_rows(YAM2) #YMSTRATS
 YAM2$YEAR <- substr(YAM2$CRUCODE,1,4)
 YMSTRATA <- rbind(YAM2[,which(colnames(YAM2) %in% colnames(YMSTRATA))], YMSTRATA) %>%
   rename(TOTALN = TNSUM, STRATNM = NMSUM, STRATNSE=NMSESUM, TOTALW = TWSUM, STRATWM = WMSUM, STRATWSE = WMSESUM)
 YMSTRATA <- YMSTRATA[,c("YEAR", "MONTH", "SAMPLES", "SPTOW", "PERCENTF", "TOTALN", "STRATNM", "STRATNSE", "TOTALW", "STRATWM", "STRATWSE")]
 }
} 

abund <- abund %>%
  arrange(across(all_of(grouping))) #line 243 in SAS
#line 244-245 means "by stratum" which in SAS would produce different tables...
#line 244: do you need to put the MEANS summary stats by stratum back in?
tsamw <- abund %>% group_by(across(all_of(grouping))) %>% Macro6
factor <- 1
tsamw <- Macro2(tsamw, factor=factor)
tsamw$NMSE<- sqrt(tsamw$NV/tsamw$NN)
if("WV" %in% colnames(tsamw)) {tsamw$WMSE<- sqrt(tsamw$WV/tsamw$WN)}
setups <- tsamw %>% #beware that in the original code line 251 drops from tsamw itself
  select(-NV, -NN, -WN) 

#i think this is gonna be an output specific to STRATUM
if(grouping == "STRATUM") {
  TSSTRATS <- merge(setups, stratsp, by=grouping, all = TRUE) %>%
    mutate(
      PERCENTF = ifelse(tsamw$SAMPLES > 0, SPTOW / tsamw$SAMPLES, 0),
      TOTALN = tsamw$TN,
      TOTALW = tsamw$TW,
      STRATNM = tsamw$NM,
      STRATNSE = tsamw$NMSE,
      STRATWM = tsamw$WM,
      STRATWSE = tsamw$WMSE) %>% 
    mutate(across(where(is.numeric), ~replace_na(.,0)))
  #select(-STRAT) %>% #line 368 drop more for TSYSTRATS?
  #SAMPLES = 0, #fill samples here?
  #PERCENTF = 0, 
  #TOTALN = 0, STRATNM = 0, STRATNSE = 0, 
  #TOTALW = 0, STRATWM = 0, STRATWSE = 0)
  #mutate(across(where(is.numeric), ~replace_na(.,0)))
  #}
  
  #tsam <- setups %>%
  #  summarise(TN = sum(NUMBER), TW = sum(WEIGHT),
  #            NM = mean(NUMBER), WM = mean(WEIGHT),
  #            NMSE = sd(NUMBER) / sqrt(n()), WMSE = sd(WEIGHT) / sqrt(n()),
  #            NV = var(NUMBER), WV = var(WEIGHT),
  #            NN = n(), WN = n())
  #tsamw <- tsam %>%
  #  mutate(SAMPLES = n())
  
  TSSTRATX <- setups %>% mutate(YEAR = "Grand Total", #this used to be a merge/altered template e.g. line 394
                                STRATUM = 0, 
                                TOTALN = tsamw$TN,
                                TOTALW = tsamw$TW, 
                                STRATNM = tsamw$NM,
                                STRATNSE = tsamw$NMSE,
                                STRATWM = tsamw$WM,
                                STRATWSE = tsamw$WMSE) #%>%
  #select(-MONTH, -MONTHN, -SPTOW) #line 306: added SPTOW here for the merge below
  
  TSSTRATZ <- merge(TSSTRATX, TSsppS2, by = "YEAR", all = TRUE) %>%
    mutate(PERCENTF = ifelse(tsamw$SAMPLES > 0, SPTOW / tsamw$SAMPLES, 0))
  
  TSSTRATZY <- TSSTRATZ
  if(!"YEAR" %in% grouping) {
    TSSTRATZY <- TSSTRATZY %>% select(-YEAR)
  }
  
TSSTRATS<- TSSTRATS[,c(grouping, "SAMPLES", "SPTOW", "PERCENTF", "TOTALN", "STRATNM", "STRATNSE", "TOTALW", "STRATWM", "STRATWSE")]
TSSTRATS <- rbind(TSSTRATS, c("All Combined", tsamw_all$NN, tsamw_all$SAMPLES, tsamw_all$PERCENTF, tsamw_all$TN, tsamw_all$NM, tsamw_all$NMSE, tsamw_all$TW, tsamw_all$WM, tsamw_all$WMSE))
}
#TSSTRATY <- TSSTRATZY %>%
#  mutate(STRATA = ifelse(STRATUM == 0, "All Combined", STRATUM)) %>%
#  select(-STRATUM) %>%
#  rename(STRATUM = STRATA)
}
#the main outputs are TSSTRATS, YSTRAT, MSTRAT, YMSTRATA
return(list(YSTRAT))} #TSSTRATS, MSTRAT, YMSTRATA #LINE 424 what you'll need to do is get the var naming the same/compatible...

WgtedAriM <- function(mypath, myspp, area="ALL", cruise="ALL", outdir) {
  if (myspp == "Black drum") {
    first <- read.dbf(file.path(mypath, "PCabun.dbf"))
    second <- read.dbf(file.path(mypath, "PCLENG.dbf"))
  } else if (myspp == "Lobster - F (GE53mm)") {
    #LOBSTER
    #Female index & catch at length (CAL), GE 53mm+ (i.e., lengths equal to 53 mm and greater)
    first <- read.dbf(file.path(mypath, "HA53Fabun.dbf"))
  } else if (myspp == "Lobster - M (GE53mm)") {
    #Male index & catch at length (CAL), GE 53mm+ (i.e., lengths equal to 53 mm and greater)
    first <- read.dbf(file.path(mypath, "HA53Mabun.dbf"))
  } else if (myspp=="Scup") {
    first <- read.dbf(file.path(mypath, "SCabun.dbf"))
  } else if (myspp=="Spot") {
    first <- read.dbf(file.path(mypath, "LXabun.dbf"))
  } else if (myspp=="Summer flounder") {
    first <- read.dbf(file.path(mypath, "PDabun.dbf"))
  } else if (myspp=="Tautog") {
    first <- read.dbf(file.path(mypath, "TOabun.dbf"))
  } else if (myspp=="Weakfish") {
    first <- read.dbf(file.path(mypath, "CRabun.dbf"))
  } else if (myspp=="Atl croaker") {
    first <- read.dbf(file.path(mypath, "MUabun.dbf"))
  }
  
  # Setting up the data
  abun <- first
  abund0 <- abun
  abund0$NUMBER <- abund0$NUMBER/abund0$MINOUT*20;
  if (!myspp %in% c("Lobster - F (GE53mm)", "Lobster - M (GE53mm)")) {
    abund0$WEIGHT <- abund0$WEIGHT/abund0$MINOUT*20; 
  }
  abund0$CRUCODE[abund0$CRUCODE==19882] <- 19884
  abund0$CRUCODE[abund0$CRUCODE==19883] <- 19885
  abund0$CRUISE <- substr(abund0$CRUCODE,5,5)
  abund0$CRUISE <- as.integer(abund0$CRUISE)
  #abund0[abund0$CRUCODE==19882,]$CRUISE <- "4"
  #abund0[abund0$CRUCODE==19883,]$CRUISE <- "5"
  abund <- abund0
  abund$AREA <- "ALL"
  
  if(area=="INM") {
    abund <- abund[!abund$STRATUM %in% c(14,17,20,23,26),]
    abund$AREA <- "INM"
  }
  
  cruiseno <- 1:6
  if(cruise=="AprOct") {
    abund <- abund[abund$YEAR > 1988,]
    cruiseno <- c(2,5)
  } else if (cruise=="AugOct") {
    cruiseno <- 4:5
  } else if (cruise=="Oct") {
    cruiseno <- 5
  } else if(cruise=="AprthruOct") {
    cruiseno <- 2:5
    abund <- abund[abund$YEAR > 1988,]
  } else if (cruise=="Spring") {
    #abund <- Macro20(abund, "Spring")
    #abund[abund$CRUISE == 3,]$CRUISE <- 2
    #abund[abund$CRUISE == 6,]$CRUISE <- 5
    cruiseno <- 2:3
  }
  abund <- abund[abund$CRUISE %in% cruiseno,]
  spp <- abund[abund$NUMBER > 0,]
  
  out <- Macro9(spp, abund)
  cruises <- paste0("cruise", paste(cruise, collapse=""))
  myfile <- paste(myspp, paste0("strata",area), paste0("cru",cruise), sep="_")
  #yrs <- 1988:2025
  annual <- out[[1]]
  annual_yrs <- annual[-nrow(annual), ]
  annual_yrs$YEAR <- as.integer(annual_yrs$YEAR)
  #missing <- yrs[!yrs %in% annual$YEAR]
  annual_yrs <- annual_yrs %>%
    complete(YEAR = full_seq(c(1988, max(YEAR)), period = 1), # Fills sequence based on min/max in group
             fill = list(value = NA)) %>% # Fills new value columns with NA
    ungroup()
  annual <- rbind(annual[nrow(annual),], annual_yrs)
  annual$month <- cruise
  annual$strata <- area
  write.csv(annual, file = file.path(outdir, paste(myfile, "annualindex.csv", sep="_")), row.names=F)
  #write.csv(out[[2]], file = file.path(outdir, paste(myfile, "strata.csv", sep="_")), row.names=F)
  #write.csv(out[[3]], file = file.path(outdir, paste(myfile, "month.csv", sep="_")), row.names=F)
return(out)}

