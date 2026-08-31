library(dplyr)
#generate from accessdb, will have to update query annually
load(file.path(Sys.getenv("VPATH"), "weakfish.RData"))
tows$STATION <- as.character(tows$STATION)

#2025 Data
headcatch <- read.csv(file.path(Sys.getenv("VPATH"),"report/species/weakfish/detrawl/HEADCATCH.csv"))
headcatch$DATE <- as.Date(headcatch$DATE, format="%m/%d/%Y")
headcatch[headcatch$SALINITY=="N/A",]$SALINITY <- NA
headcatch$SALINITY <- as.numeric(headcatch$SALINITY)
headcatch[headcatch$TEMP=="N/A",]$TEMP <- NA
headcatch$TEMP <- as.numeric(headcatch$TEMP)
headcatch[headcatch$DO=="N/A",]$DO <- NA
headcatch$DO <- as.numeric(headcatch$DO)
headcatch[headcatch$pH=="N/A",]$pH <- NA
headcatch$pH <- as.numeric(headcatch$pH)

lengths <- read.csv(file.path(Sys.getenv("VPATH"),"report/species/weakfish/detrawl/LENGTHS.csv"))
lengths$DATE <- as.Date(lengths$DATE, format="%m/%d/%Y")

geom_mean <- function(x) {
  exp(mean(log(x)))
}
geom_mean0 <- function(x) {
  exp(mean(log(x + 1))) - 1
}

alpha <- 0.05
ci <- function(x) {
  x$se <- x$sample.sd / sqrt(x$tow)
  x$degrees.freedom <- as.integer(x$tow - 1)
  x$t.score <- qt(p = alpha / 2, df = x$degrees.freedom, lower.tail = F)
  x$margin.error <- x$t.score * x$se
  x$lower.bound <- x$geom - x$margin.error
  x$upper.bound <- x$geom + x$margin.error
  return(x)
}

# Return the desired percentiles plus the geometric mean
bp.vals <- function(x, probs = c(0.1, 0.25, 0.75, .9)) {
  r <- quantile(x, probs = probs, na.rm = TRUE)
  r <- c(r[1:2], geom_mean0(x), r[3:4])
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


#boats$ID <- paste(boats$DATE, boats$STATION, sep="_")
#boats$no1 <- boats$NUMBER + 1 
#boats$no0 <- boats$NUMBER 

tows <- bind_rows(tows, headcatch)
tows <- tows[order(tows$DATE),]
tows$ID <- paste(tows$DATE, tows$STATION, sep="_")

boats <- tows[tows$Common_Name=="Weakfish",]
boats$no1 <- boats$NUMBER + 1 
boats$no0 <- boats$NUMBER 

tows <- tows[!duplicated(tows$ID),]
tows <- tows[!tows$ID %in% boats$ID,]
tows$no0 <- 0
tows$no1 <- 1
tows$NUMBER <- NA
tows$SPECIES <- "Weakfish"
tows <- tows[,names(boats)]
weakfish <- rbind(boats, tows)
weakfish$YEAR <- format(weakfish$DATE, '%Y')

weakfish_annual <- weakfish %>%
  group_by(YEAR) %>%
  summarise(
    geom1 = geom_mean(no1), tow = n(), #yes = sum(ifelse(NUMBER > 0, 1, 0)),
    mysd = sd(NUMBER), m = mean(NUMBER)
  )

# First, read in all length freq data, split into separate tows, and calc fraction < month-specific cutoffs
lens <- length
lens$STATION <- as.character(lens$STATION)
lens <- bind_rows(lens, lengths)
lens$ID <- paste(lens$DATE, lens$STATION, sep="_")
y <- split(lens,lens$ID)

#hist(lens$LENGTH) # look at length frequency over the time series:

out <- as.data.frame(matrix(NA,nr=length(y),ncol=2)) # Setup some place to keep results
colnames(out) <- c("ID","keepPct")

# Loop through each tow, and calc percent of catch within YOY size bins:
for(i in 1:length(y)) {
  Y <- y[[i]]            # example: i <- 1224
  
  if(as.numeric(format(Y$DATE,"%m"))[1]==6) { # June
    cutoff <- 110                                                              
    out[i,] <- data.frame("ID"=Y$ID[1],"keepPct"=sum(Y$LENGTH < cutoff)/nrow(Y))
  }
  
  if(as.numeric(format(Y$DATE,"%m"))[1]==7) {
    cutoff <- 150                                                             
    out[i,] <- data.frame("ID"=Y$ID[1],"keepPct"=sum(Y$LENGTH < cutoff)/nrow(Y))
  }
  
  if(as.numeric(format(Y$DATE,"%m"))[1]==8) {
    cutoff <- 200                                                              
    out[i,] <- data.frame("ID"=Y$ID[1],"keepPct"=sum(Y$LENGTH < cutoff)/nrow(Y))
  }
}

Out <- na.omit(out)
Out

################################################################################
# Merge the information above with the catch records:
catch <- weakfish
# Confirm that Month field can be set from date:
#plot(as.numeric(format(catch$DATE,"%m"))-as.numeric(catch$Month),type="h",col="red",ylab="Difference") # FALSE! But only May 1st 1992, so ignoring for now; good to check annually
nrow(catch)
all <- merge(catch,Out,by="ID",all.x=TRUE)
nrow(all) # make sure we haven't manufactured any extra rows!

# Multiply total catch by the percentage of fish within the YOY size bins
all$newCatch <- round(all$NUMBER * all$keepPct) # round to the nearest whole fish

# replace all NAs with 0s!
all$newCatch[is.na(all$newCatch)] <- 0

# Calculate geometric mean for June-August
all$Month <- format(all$DATE,"%m")

index <- data.frame(
  "index"=with(all[is.element(as.numeric(all$Month),6:8),],
               tapply(newCatch,format(DATE,"%Y"),geom_mean0))
)
