library(foreign)
library(dplyr)
library(purrr)
library(tidyr)
# Importing the data

macro20 <- function() {
  timescale <- "month"
  if (timescale == "season" | timescale=="quarter") { #macro20
    ABUND <- ABUND %>% mutate(
      CRUCODE = case_match(CRUCODE,
                           19882 ~ 19884,
                           19883 ~ 19885,
                           19893 ~ 19892,
                           19896 ~ 19895,
                           19903 ~ 19902,  
                           19913 ~ 19912,  
                           19923 ~ 19922,  
                           19933 ~ 19932,  
                           19943 ~ 19942,  
                           19953 ~ 19952,  
                           19963 ~ 19962, 
                           19973 ~ 19972,  
                           19983 ~ 19982, 
                           19993 ~ 19992,  
                           20003 ~ 20002,
                           20013 ~ 20012,   
                           20023 ~ 20022, 
                           20033 ~ 20032, 
                           20043 ~ 20042,  
                           20053 ~ 20052,  
                           20063 ~ 20062, 
                           20073 ~ 20072,
                           20083 ~ 20082, 
                           20093 ~ 20092,  
                           20103 ~ 20102,  
                           20113 ~ 20112,  
                           20123 ~ 20122,  
                           20133 ~ 20132, 
                           20143 ~ 20142,
                           20153 ~ 20152,
                           20163 ~ 20162,
                           20173 ~ 20172,
                           20183 ~ 20182,
                           20193 ~ 20192,
                           20223 ~ 20222,
                           20233 ~ 20232,
                           20243 ~ 20242,
                           .default = CRUCODE))
  }
}

#SETUPS here would have to be a data frame of NUMBER,WEIGHT
macro6 <- function(SETUPS) {
  TSAM <- SETUPS %>%
    summarise(TN = sum(NUMBER, na.rm=T), #10469.680381
              TW = sum(WEIGHT, na.rm=T),
              NM = mean(NUMBER, na.rm=T), WM = mean(WEIGHT, na.rm=T),
              NMSE = sd(NUMBER, na.rm=T) / sqrt(n()), WMSE = sd(WEIGHT, na.rm=T) / sqrt(n()),
              NV = var(NUMBER, na.rm=T), WV = var(WEIGHT, na.rm=T),
              NN = n(), WN = n())
  
  TSAMW <- TSAM %>%
    mutate(SAMPLES = NN) 
  return(TSAMW)}

# Function to assign factor (stratum weight) values
macro3 <- function(AREA, STRATUM) {
  FACTOR <- NA
  if (AREA == "ALL") {
    if (STRATUM == 12) FACTOR <- 0.008
    else if (STRATUM == 13) FACTOR <- 0.018
    else if (STRATUM == 14) FACTOR <- 0.043
    else if (STRATUM == 15) FACTOR <- 0.010
    else if (STRATUM == 16) FACTOR <- 0.045
    else if (STRATUM == 17) FACTOR <- 0.164
    else if (STRATUM == 18) FACTOR <- 0.033
    else if (STRATUM == 19) FACTOR <- 0.132
    else if (STRATUM == 20) FACTOR <- 0.156
    else if (STRATUM == 21) FACTOR <- 0.010
    else if (STRATUM == 22) FACTOR <- 0.111
    else if (STRATUM == 23) FACTOR <- 0.103
    else if (STRATUM == 24) FACTOR <- 0.019
    else if (STRATUM == 25) FACTOR <- 0.075
    else if (STRATUM == 26) FACTOR <- 0.074
  } else if (AREA == "INM") {
    if (STRATUM == 12) FACTOR <- 0.018
    else if (STRATUM == 13) FACTOR <- 0.039
    else if (STRATUM == 15) FACTOR <- 0.021
    else if (STRATUM == 16) FACTOR <- 0.099
    else if (STRATUM == 18) FACTOR <- 0.073
    else if (STRATUM == 19) FACTOR <- 0.286
    else if (STRATUM == 21) FACTOR <- 0.021
    else if (STRATUM == 22) FACTOR <- 0.241
    else if (STRATUM == 24) FACTOR <- 0.041
    else if (STRATUM == 25) FACTOR <- 0.162
  } else if (AREA == "SOU") {
    if (STRATUM == 21) FACTOR <- 0.024
    else if (STRATUM == 22) FACTOR <- 0.284
    else if (STRATUM == 23) FACTOR <- 0.264
    else if (STRATUM == 24) FACTOR <- 0.049
    else if (STRATUM == 25) FACTOR <- 0.191
    else if (STRATUM == 26) FACTOR <- 0.188
  } else if (AREA == "NOR") {
    if (STRATUM == 12) FACTOR <- 0.013
    else if (STRATUM == 13) FACTOR <- 0.030
    else if (STRATUM == 14) FACTOR <- 0.070
    else if (STRATUM == 15) FACTOR <- 0.016
    else if (STRATUM==16) FACTOR <- 0.074
    else if (STRATUM==17) FACTOR <- 0.268
    else if (STRATUM==18) FACTOR <- 0.055
    else if (STRATUM==19) FACTOR <- 0.216
    else if (STRATUM==20) FACTOR <- 0.256
  } else if (AREA=="SHR") {
    if (STRATUM==12) FACTOR <- 0.216
    else if (STRATUM==13) FACTOR <- 0.314
    else if (STRATUM==14) FACTOR <- 0.470
  } else if (AREA=="MON") {
    if (STRATUM==12) FACTOR <- 0.119
    else if (STRATUM==13) FACTOR <- 0.262
    else if (STRATUM==14) FACTOR <- 0.619
  } else if (AREA=="BSB") {
    if (STRATUM==15) FACTOR <- 0.044
    else if (STRATUM==16) FACTOR <- 0.208
    else if (STRATUM==17) FACTOR <- 0.748
  } else if (AREA=="SBS") {
    if (STRATUM==18) FACTOR <- 0.104
    else if (STRATUM==19) FACTOR <- 0.410
    else if (STRATUM==20) FACTOR <- 0.486
  } else if (AREA=="SIC") {
    if (STRATUM==21) FACTOR <- 0.043
    else if (STRATUM==22) FACTOR <- 0.496
    else if (STRATUM==23) FACTOR <- 0.462
  } else if (AREA=="CMH") {
    if (STRATUM==24) FACTOR <- 0.114
    else if (STRATUM==25) FACTOR <- 0.446
    else if (STRATUM==26) FACTOR <- 0.440
  } else if (AREA=="INS") {
    if (STRATUM==12) FACTOR <- 0.103
    else if (STRATUM==15) FACTOR <- 0.122
    else if (STRATUM==18) FACTOR <- 0.419
    else if (STRATUM==21) FACTOR <- 0.119
    else if (STRATUM==24) FACTOR <- 0.238
  } else if (AREA=="HOG") {
    if (STRATUM==18) FACTOR <- 0.104
    else if (STRATUM==21) FACTOR <- 0.030
    else if (STRATUM==22) FACTOR <- 0.345
    else if (STRATUM==24) FACTOR <- 0.059
    else if (STRATUM==25) FACTOR <- 0.232
    else if (STRATUM==26) FACTOR <- 0.229
  } else if(AREA=="PST") FACTOR <- 1			
  
  return(FACTOR)
}

# Function to apply factor (stratum weight) to means, variance, and standard errors
macro2 <- function(TSAMW, FACTOR) { #macro3 goes in here
  TSAMW$NM <- FACTOR * TSAMW$NM
  TSAMW$WM <- FACTOR * TSAMW$WM
  TSAMW$NMSE <- TSAMW$NV / TSAMW$NN * FACTOR^2
  TSAMW$WMSE <- TSAMW$WV / TSAMW$WN * FACTOR^2
  return(data.frame(TSAMW))
}

# MACRO 7
macro7 <- function(TSAMW) { #you get this from macro6
  FACTOR <- unname(unlist(Map(macro3, TSAMW$AREA, TSAMW$STRATUM)))
  TSAMW <- macro2(TSAMW, FACTOR) 
  #SETUPS <- TSAMW %>% select(-NN, -WN)
return(TSAMW)}

# MACRO 8
MACRO8 <- function(TSAMW) { 
#TSAMW <- macro7(TSAMW)  
TSSUM <- TSAMW %>%
  summarise(SAMPLET = sum(SAMPLES), NMSUM = sum(NM), WMSUM = sum(WM), 
            NMSESUM1 = sum(NMSE), WMSESUM1 = sum(WMSE), 
            TNSUM = sum(TN), TWSUM = sum(TW))

TSSUM2 <- TSSUM %>%
  mutate(SAMPLES = SAMPLET, 
         NMSESUM = sqrt(NMSESUM1), 
         WMSESUM = sqrt(WMSESUM1))

SETUPS <- TSSUM2 %>%
  select(-NMSESUM1, -WMSESUM1)
}


posstrat <- function(grouping, SPP) {
  SPP <- SPP %>%
    arrange(across(all_of(grouping)))
  #superseded <- mtcars %>%
  #filter(disp < 160) %>%
  #  group_by(across(all_of(cols))) %>%
  #  summarise(n = n(), .groups = 'drop_last')
  POSSTRAT <- SPP %>%
    group_by(across(all_of(grouping))) %>%
    summarise(SPTOW = n(), .groups = 'drop')
  
  STRATSP <- POSSTRAT %>%
    select(all_of(c("SPTOW", grouping))) %>% drop_na(all_of(grouping))
}

#the proc means doesn't mean anything, so much as the "by"
#so for example...
#
#PROC SORT DATA=ABUND;
#BY STRATUM;
#PROC MEANS;
#BY STRATUM;
#%MACRO6; *CALCULATE MEAN AND VARIANCE;
#
#is the same as...
#TSAMW <- ABUND %>% group_by(across(all_of(c("STRATUM")))) %>% macro6

YR <- function(ABUND, STRATSP, grouping) {
  ABUND <- ABUND %>%
    arrange(across(all_of(c(grouping, "STRATUM", "AREA"))))
  #line 442: do I need to summarize before piping into MACRO8?
  TSAMW <- ABUND %>% group_by(across(all_of(c(grouping, "STRATUM", "AREA")))) %>% macro6
  TEMPLATE <- TSAMW %>% macro7 #%>% select(-NN, -WN)
  SETUPS <- TEMPLATE %>% group_by(across(all_of(grouping))) %>% MACRO8 #LINE 444 #PICK BACK UP HERE 7/25 LINE 595
  # WILL NEED TO SAS TEST TO SEE WHAT'S GOING ON WITH THE TEMPLATE (LINE 572) ...how is this passed? what's the context for having it in that code block?
  YAM2 <- merge(SETUPS, STRATSP, by=grouping, all = TRUE) %>% mutate(STRATUM = 0,
                             #SPTOW = n(),
                             PERCENTF = ifelse(SAMPLES > 0, SPTOW / SAMPLES, 0),
                             TOTALN = TNSUM,
                             TOTALW = TWSUM,
                             STRATNM = NMSUM,
                             STRATNSE = NMSESUM,
                             STRATWM = WMSUM,
                             STRATWSE = WMSESUM) %>% mutate(across(where(is.numeric), ~replace_na(.,0)))
  
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
  
  TSAMW <- ABUND %>%
    arrange(across(all_of(c("STRATUM", "AREA"))))
  SETUPS <- TSAMW %>% group_by(across(all_of(c("STRATUM", "AREA")))) %>% macro6 %>% macro7 %>% MACRO8
  
  TSSTRATA <- SETUPS %>% mutate(
    YEAR="Grand Total",
    STRATUM=0,
    SAMPLES=SETUPS$SAMPLES,
    TOTALN = TSAMW$TNSUM,
    TOTALW = TSAMW$TWSUM,
    STRATNM = TSAMW$NMSUM,
    STRATNSE = TSAMW$NMSESUM,
    STRATWM = TSAMW$WMSUM,
    STRATWSE = TSAMW$WMSESUM,
    MONTHN="1") %>% mutate(across(where(is.numeric), ~replace_na(.,0))) #select(-STRAT) %>% #line 368 drop more for TSYSTRATS?
  #SAMPLES = 0, #fill samples here?
  #PERCENTF = 0, 
  #TOTALN = 0, STRATNM = 0, STRATNSE = 0, 
  #TOTALW = 0, STRATWM = 0, STRATWSE = 0)
return(list(TSSTRATA, YAM2))}

# MACRO 9
#grouping <- "STRATUM"
macro9 <- function(SPP, ABUND) {
#*BY STRATUM BY STRATUM BY STRATUM;

  # MACRO 4
TEMPLATE <- data.frame(YEAR = character(), MONTH = character(), STRATUM = numeric(), 
                      SAMPLES = numeric(), SPTOW = numeric(), PERCENTF = numeric(), 
                      TOTALN = numeric(), STRATNM = numeric(), STRATNSE = numeric(), 
                      TOTALW = numeric(), STRATWM = numeric(), STRATWSE = numeric(), 
                      MONTHN = character(), stringsAsFactors = FALSE)

for (grouping in c("STRATUM", "YEAR", "CRUISE", "CRUCODE")) { #c("YEAR", "STRATUM"),
TSAMW_all <- macro6(ABUND)
TSAMW_all$PERCENTF = ifelse(TSAMW_all$SAMPLES > 0, TSAMW_all$NN / TSAMW_all$SAMPLES, 0)
STRATSP <- posstrat(grouping, SPP)

TSSPPS <- SPP %>%
  summarise(SPTOW = n(), .groups = 'drop') #line 374 does this need more summary stats?

TSSPPS2 <- TSSPPS %>%
  mutate(YEAR = "Grand Total") %>%
  select(YEAR, SPTOW)

if(all(grouping %in% c("YEAR", "CRUISE", "CRUCODE"))) {
 TSSTRATs <- YR(ABUND, STRATSP, grouping)
 TSSTRATA <- TSSTRATs[[1]]
 YAM2 <- TSSTRATs[[2]]
 
 TSSTRATB <- merge(TSSTRATA,TSSPPS2, by = "YEAR", all=T) %>% mutate(
   PERCENTF = ifelse(SAMPLES > 0, SPTOW / SAMPLES, 0),
   MONTH = "Grand Total"
 ) %>% select(-MONTHN, -STRATUM)
 
 if(grouping=="YEAR") {
 YSTRAT <- TSSTRATB %>% select(-MONTH) 
 YSTRAT <- rbind(YAM2[,which(colnames(YAM2) %in% colnames(YSTRAT))], YSTRAT) %>%
   rename(TOTALN = TNSUM, STRATNM = NMSUM, STRATNSE=NMSESUM, TOTALW = TWSUM, STRATWM = WMSUM, STRATWSE = WMSESUM)
 YSTRAT <- YSTRAT[,c("YEAR", "SAMPLES", "SPTOW", "PERCENTF", "TOTALN", "STRATNM", "STRATNSE", "TOTALW", "STRATWM", "STRATWSE")]
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
ABUND <- ABUND %>%
  arrange(across(all_of(grouping))) #line 243 in SAS
#line 244-245 means "by stratum" which in SAS would produce different tables...
#line 244: do you need to put the MEANS summary stats by stratum back in?
TSAMW <- ABUND %>% group_by(across(all_of(grouping))) %>% macro6
FACTOR <- 1
TSAMW <- macro2(TSAMW, FACTOR=FACTOR)
TSAMW$NMSE<- sqrt(TSAMW$NV/TSAMW$NN)
TSAMW$WMSE<- sqrt(TSAMW$WV/TSAMW$WN)
SETUPS <- TSAMW %>% #beware that in the original code line 251 drops from TSAMW itself
  select(-NV, -WV, -NN, -WN) 

#i think this is gonna be an output specific to STRATUM
if(grouping == "STRATUM") {
  TSSTRATS <- merge(SETUPS, STRATSP, by=grouping, all = TRUE) %>%
    mutate(
      PERCENTF = ifelse(TSAMW$SAMPLES > 0, SPTOW / TSAMW$SAMPLES, 0),
      TOTALN = TSAMW$TN,
      TOTALW = TSAMW$TW,
      STRATNM = TSAMW$NM,
      STRATNSE = TSAMW$NMSE,
      STRATWM = TSAMW$WM,
      STRATWSE = TSAMW$WMSE) %>% 
    mutate(across(where(is.numeric), ~replace_na(.,0)))
  #select(-STRAT) %>% #line 368 drop more for TSYSTRATS?
  #SAMPLES = 0, #fill samples here?
  #PERCENTF = 0, 
  #TOTALN = 0, STRATNM = 0, STRATNSE = 0, 
  #TOTALW = 0, STRATWM = 0, STRATWSE = 0)
  #mutate(across(where(is.numeric), ~replace_na(.,0)))
  #}
  
  #TSAM <- SETUPS %>%
  #  summarise(TN = sum(NUMBER), TW = sum(WEIGHT),
  #            NM = mean(NUMBER), WM = mean(WEIGHT),
  #            NMSE = sd(NUMBER) / sqrt(n()), WMSE = sd(WEIGHT) / sqrt(n()),
  #            NV = var(NUMBER), WV = var(WEIGHT),
  #            NN = n(), WN = n())
  #TSAMW <- TSAM %>%
  #  mutate(SAMPLES = n())
  
  TSSTRATX <- SETUPS %>% mutate(YEAR = "Grand Total", #this used to be a merge/altered TEMPLATE e.g. line 394
                                STRATUM = 0, 
                                TOTALN = TSAMW$TN,
                                TOTALW = TSAMW$TW, 
                                STRATNM = TSAMW$NM,
                                STRATNSE = TSAMW$NMSE,
                                STRATWM = TSAMW$WM,
                                STRATWSE = TSAMW$WMSE) #%>%
  #select(-MONTH, -MONTHN, -SPTOW) #line 306: added SPTOW here for the merge below
  
  TSSTRATZ <- merge(TSSTRATX, TSSPPS2, by = "YEAR", all = TRUE) %>%
    mutate(PERCENTF = ifelse(TSAMW$SAMPLES > 0, SPTOW / TSAMW$SAMPLES, 0))
  
  TSSTRATZY <- TSSTRATZ
  if(!"YEAR" %in% grouping) {
    TSSTRATZY <- TSSTRATZY %>% select(-YEAR)
  }
  
TSSTRATS<- TSSTRATS[,c(grouping, "SAMPLES", "SPTOW", "PERCENTF", "TOTALN", "STRATNM", "STRATNSE", "TOTALW", "STRATWM", "STRATWSE")]
TSSTRATS <- rbind(TSSTRATS, c("All Combined", TSAMW_all$NN, TSAMW_all$SAMPLES, TSAMW_all$PERCENTF, TSAMW_all$TN, TSAMW_all$NM, TSAMW_all$NMSE, TSAMW_all$TW, TSAMW_all$WM, TSAMW_all$WMSE))
}
#TSSTRATY <- TSSTRATZY %>%
#  mutate(STRATA = ifelse(STRATUM == 0, "All Combined", STRATUM)) %>%
#  select(-STRATUM) %>%
#  rename(STRATUM = STRATA)
}
#the main outputs are TSSTRATS, YSTRAT, MSTRAT, YMSTRATA
return(list(TSSTRATS, YSTRAT, MSTRAT, YMSTRATA))} #LINE 424 what you'll need to do is get the var naming the same/compatible...

Spp <- function(mypath, spp, area="ALL", cruise="ALL", outdir) {
  if (spp == "Black drum") {
    FIRST <- read.dbf(file.path(mypath, "PCABUN.dbf"))
    SECOND <- read.dbf(file.path(mypath, "PCLENG.dbf"))
  } else if (spp == "Lobster - F (GE53mm)") {
    #LOBSTER
    #Female index & catch at length (CAL), GE 53mm+ (i.e., lengths equal to 53 mm and greater)
    FIRST <- read.dbf(file.path(mypath, "HA53FABUN.dbf"))
  } else if (spp == "Lobster - M (GE53mm)") {
    #Male index & catch at length (CAL), GE 53mm+ (i.e., lengths equal to 53 mm and greater)
    FIRST <- read.dbf(file.path(mypath, "HA53MABUN.dbf"))
  } else if (spp=="Scup") {
    FIRST <- read.dbf(file.path(mypath, "SCABUN.dbf"))
  } else if (spp=="Spot") {
    FIRST <- read.dbf(file.path(mypath, "LXABUN.dbf"))
  } else if (spp=="Summer flounder") {
    FIRST <- read.dbf(file.path(mypath, "PDABUN.dbf"))
  } else if (spp=="Tautog") {
    FIRST <- read.dbf(file.path(mypath, "TOABUN.dbf"))
  } else if (spp=="Weakfish") {
    FIRST <- read.dbf(file.path(mypath, "CRABUN.dbf"))
  }
  
  # Setting up the data
  ABUN <- FIRST
  ABUND0 <- ABUN
  ABUND0$NUMBER <- ABUND0$NUMBER/ABUND0$MINOUT*20;
  ABUND0$WEIGHT <- ABUND0$WEIGHT/ABUND0$MINOUT*20;
  ABUND0$CRUCODE[ABUND0$CRUCODE==19882] <- 19884
  ABUND0$CRUCODE[ABUND0$CRUCODE==19883] <- 19885
  ABUND0$CRUISE <- substr(ABUND0$CRUCODE,5,5)
  #ABUND0[ABUND0$CRUCODE==19882,]$CRUISE <- "4"
  #ABUND0[ABUND0$CRUCODE==19883,]$CRUISE <- "5"
  ABUND <- ABUND0
  ABUND$AREA <- "ALL"
  
  if(area=="INM") {
    ABUND <- ABUND[!ABUND$STRATUM %in% c(14,17,20,23,26),]
    ABUND$AREA <- "INM"
  }
  
  cruiseno <- 1:6
  if(cruise=="AprOct") {
    ABUND <- ABUND[ABUND$YEAR > 1988,]
    cruiseno <- c(2,5)
  } else if (cruise=="AugOct") {
    cruiseno <- 4:5
  } else if (cruise=="Oct") {
    cruiseno <- 5
  }
  ABUND <- ABUND[ABUND$CRUISE %in% cruiseno,]
  SPP <- ABUND[ABUND$NUMBER > 0,]
  
  out <- macro9(SPP, ABUND)
  cruises <- paste0("cruise", paste(cruise, collapse=""))
  myfile <- paste(spp, area, cruise, sep="_")
  write.csv(out[[2]], file = file.path(outdir, paste(myfile, "annual.csv", sep="_")), row.names=F)
  write.csv(out[[1]], file = file.path(outdir, paste(myfile, "strata.csv", sep="_")), row.names=F)
  write.csv(out[[3]], file = file.path(outdir, paste(myfile, "month.csv", sep="_")), row.names=F)
return(out)}

