hsc <- function(file, season) {
#-----------------------------------------------------------------------------------------------------------------------
#-LOAD PACKAGES (make sure they are installed first!)-------------------------------------------------------------------
library(fishmethods)
library(readxl)
library(dplyr)
  
capitalize_first <- function(x) {
    sub("(.)", "\\U\\1", x, perl = TRUE)
}

#=======================================================================================================================
#-READ IN DATA (change dir and file name as needed)---------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#==>Each row should represent a single unit of effort; observations of zero (zero tows or zero hauls) need to be included
#==>Make sure response values (frequencies) are counts

data <- read.csv(file.path("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/data/hsc", paste0("NJ_", file, "2024.csv"))) #Read in data
#my_data <- as.data.frame(read_excel("V:/Marine Fish/NJ Trawl Survey Data/HorseshoeCrab/HSCrabMaleCatch.xlsx"))
#anti_join(data, my_data) and subsequent exploration shows these files are the same

indata <- replace(data, is.null(data), "NA")		#==>replace empty cells with NA
dim(indata)				#==>check size of input data
head(indata)
#Prepare data for analysis----------------------------------------------------------------------------------------------
#Change CRUCODE values for 1988 to be able to subset data by survey cruise (1-January, 2-April, 3-June, 4-August, 5-October)
indata$CRUCODE<-as.character(indata$CRUCODE)#convert to character values

#If using entire time series from 1988 for all sexes and unsexed combined:
indata$CRUCODE[indata$CRUCODE=="19882"]<-"19884"#change last character of crucode for August 1988 from 2 to 4 to be 
                                                #  consistent with August cruises for the time series
indata$CRUCODE[indata$CRUCODE=="19883"]<-"19885"#change last character of crucode for October 1988 from 3 to 5 to be 
                                                #  consistent with October cruises for the time series

#Sexed Horseshoe Crab########################################################################################################
#Convert CRUCODE to a single character
indata$CRUCODE<-substring(indata$CRUCODE,5,5)

#if using male or female or all adult horseshoe crab data (crabs sexed consistently only after 1998; sexed crabs considered adult):
#For April and August################
if(season=="spring") {
  data <- subset(indata,indata$CRUCODE=="2" & indata$Year>1998|indata$CRUCODE=="4" & indata$Year>1998) 
} else {data <- subset(indata,indata$CRUCODE=="5" & indata$Year>1998) }

#####Examine data####################################################################################
#Prepare data fields
data$Year<-as.factor(data$Year)#make Year a categorical factor
data$StdNum<-(data$NUMBER/data$MINOUT)*20 #Standardize catch numbers to 20 minute tows (tow durations may vary)
#=======================================================================================================================
#If using sexed horseshoe crab data:
Year<-vector(length=length(1999:2024)) #create vector for year (crabs sexed consistently only after 1998)

#create index vectors
Yrmean<-vector(length=length(Year))#create vector for means
Yrvar<-vector(length=length(Year))#create vector for variance
Samples<-vector(length=length(Year))#create vector for number of samples
PosTows<-vector(length=length(Year))#create vector for number of positive tows
PPT<-vector(length=length(Year))#create vector for proportion of positive tows
Totaln<-vector(length=length(Year))#create vector for summing total number

x=1#initialize variable in for loop
for (i in 1999:2024){               #if using sexed horseshoe crab data
    ydata<-subset(data,data$Year==i)
      Year[x]=i
      Samples[x]=length(ydata$Year)
      pdata<-subset(ydata,ydata$StdNum!=0)
      PosTows[x]=length(pdata$Year)
      PPT[x]=PosTows[x]/Samples[x]
      Totaln[x]=sum(ydata$StdNum)
      dd=deltadist(ydata$StdNum)
      Yrmean[x]=dd[1]
      Yrvar[x]=dd[2]
      x=x+1
}

Index<-as.data.frame(cbind(Year,Samples,PosTows,PPT,Totaln,Yrmean,Yrvar))#create dataframe to hold sample n, mean and variance values
Index$SD<-sqrt(Index$Yrvar)
Index$SE<-Index$SD/sqrt(Index$Samples)
Index$LCI<-Index$Yrmean-(1.96*Index$SE)
Index$UCI<-Index$Yrmean+(1.96*Index$SE)
Index$sex <- capitalize_first(file)
Index$season <- capitalize_first(season)

index <- Index[Index$Samples > 0,]
return(Index, index)}