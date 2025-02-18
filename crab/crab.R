library(tidyr)
library(dplyr)
library(RODBC)
#RIVER HERRING#
rherring <- read.csv("~/crab/River Herring Seine Blue Crab Data_catch.csv")
rherring$STATION[rherring$STATION=="GEH03"] <- "GEHS03"
rherring$WATERBODY[rherring$WATERBODY=="GERH"] <- "GREAT EGG HARBOR RIVER"
rherring$id <- paste(rherring$STATION, rherring$DATE, sep="_")

rherring_effort <- read.csv("~/crab/River Herring Seine Blue Crab Data_head.csv", blank.lines.skip=T)
rherring_effort <- rherring_effort[!is.na(rherring_effort$YEAR),]
rherring_effort$STATION[rherring_effort$STATION=="GEHSO3"] <- "GEHS03"
rherring_effort$STATION[rherring_effort$STATION=="MRS03ALT"] <- "MRS03"
rherring_effort$id <- paste(rherring_effort$STATION, rherring_effort$DATE, sep="_")

rherring <- rherring %>% add_row(id = rherring_effort$id[!rherring_effort$id %in% rherring$id])
rherring$WATERBODY[grep("MR",rherring$id)] <- "MAURICE RIVER"
rherring$WATERBODY[grep("GEH",rherring$id)] <- "GREAT EGG HARBOR RIVER"
rherring$DATE <- sapply(strsplit(rherring$id, "_"), "[[", 2)
rherring$STATION <- sapply(strsplit(rherring$id, "_"), "[[", 1)
rherring$DATE <- strptime(rherring$DATE, "%m/%d/%Y")

#rherring <- rherring %>% group_by(WATERBODY) %>% complete(STATION, DATE)
rherring$YEAR <- format(rherring$DATE, "%Y")
rherring$MONTH <- format(rherring$DATE, "%m")
rherring <- rherring[order(rherring$DATE),]
#GEHR <- rherring[rherring$WATERBODY=="GREAT EGG HARBOR RIVER",]
#MR <- rherring[rherring$WATERBODY=="MAURICE RIVER",]
subsetGE15 <- rherring[rherring$YEAR==2015 & rherring$WATERBODY=="GREAT EGG HARBOR RIVER",]
subsetMR15 <- rherring[rherring$YEAR==2015 & rherring$WATERBODY=="MAURICE RIVER",]

test <- subsetMR15
test$NUMBER[is.na(test$NUMBER)] <- 0
geom_mean <- function(x) {exp(mean(log(x+1)))-1}
#test <- aggregate(rherring$NUMBER, by=list(year=rherring$YEAR, river=rherring$WATERBODY), geom_mean)
testout <- aggregate(test$NUMBER, by=list(year=test$YEAR, river=test$WATERBODY), geom_mean)
#################

#OcEAN TRAWL#
load("~/crab/crab.RData")
tows$MONTH = as.integer(sapply(tows$YRMODA, function(x) substr(x, 5, 6)))
crab <- boats %>% add_row(ID = tows$ID, STRATUM=tows$STRATUM, MONTH=tows$MONTH, CRUCODE=tows$CRUCODE)
crab$NUMBER[is.na(crab$NUMBER)] <- 1
geom_mean <- function(x) {exp(mean(log(x)))}
