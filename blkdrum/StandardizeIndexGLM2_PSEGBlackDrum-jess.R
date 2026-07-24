########################################################################################################################
##DEVELOPING STANDARDIZED INDICES IN R USING GLM########################################################################
########################################################################################################################
##Note: This particular code does not address the issue of spatial or temporal autocorrelation; checking for     #######
##autocorrelation is part of the model validation process as its presence violates the assumption of independence#######
##and can lead to biased parameter estimates as well as overestimate precision; example code and relevant        #######
##references can be provided                                                                                     #######
##Also, this code only considers GLMs; GAMs are briefly mentioned, but code is not given (additional code can be #######
##provided on request)                                                                                           #######
########################################################################################################################
##Contact: L.M.Lee															     #######
########################################################################################################################
##References																     #######
##---------------------------------------------------------------------------------------------------------------#######
##The following texts were instrumental in developing this code:							           #######
##																		     #######
##Zuur, A.F., E.N. Ieno, N.J. Walker, A.A. Saveliev, and G.M. Smith. 2009. Mixed effects models and extensions   #######
##	in ecology with R. Springer-Verlag, New York. 574 p.									     #######
##																		     #######
##Zuur, A.F., A.A. Saveliev, and E.N. Ieno. 2012. Zero inflated models and generalized linear mixed models       #######
##	with R. Highland Statistics Ltd, United Kingdom. 324 p.								     #######
##																		     #######
##---------------------------------------------------------------------------------------------------------------#######
##The 'HighstatLib.r' code was kindly provided by Alain Zuur								     #######
########################################################################################################################

########################################################################################################################
##PREPARE R#############################################################################################################
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
#-LOAD PACKAGES (make sure they are installed first!)-------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

library(MASS)
library(survey)
library(pscl)
library(lmtest)
library(Hmisc)

#-----------------------------------------------------------------------------------------------------------------------
#-CLEAN R CONSOLE-------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

graphics.off()
#rm(list=ls(all=TRUE))

########################################################################################################################
##GET DATA READY########################################################################################################
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
#-READ IN DATA (change indir and file name as needed)---------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#==>Each row should represent a single unit of effort; observations of zero (zero tows or zero hauls) need to be included
#==>Make sure response values (frequencies) are counts
#==>If data missing, cells should be blank; some programs will put a period into blanks cells-->these need to be
#==>deleted (cleared)
indir <- file.path(Sys.getenv("FILEPATH"), "data/blackdrum")
source(file.path("./blkdrum","HighstatLibV13.R"))	#==>change path as needed; contains 'corvif' function
file1 <- file.path(Sys.getenv("FILEPATH"),"BLACK DRUM-catch_Beach Seine.csv")			#==>change file name as needed
blk <- read.csv(file.path(Sys.getenv("FILEPATH"),"BLACK DRUM_length.csv"))
indata <- read.csv(file1, header = TRUE, colClasses = c(SAMPLE_ID = "character", SAMPLNO="character", DBO_ES_ST_SAMPLE_SAMPLE_ID="character")) 	#==>reads in tab-delimited file
indata$DBO_ES_ST_SAMPLE_SAMPLE_ID <- sapply(indata$SAMPLE_ID, function(q) {
  if(nchar(q) > 9) { a <- paste0(substr(q, 1, 6), substr(q, 12, 14))
  } else { a <- q}
  return(a)})
indata$ID_SAMPLES <- as.character(as.integer(indata$DBO_ES_ST_SAMPLE_SAMPLE_ID))

file <- file.path(indir,"AllHaulsCatchTable_woSuspendedStations_YOY.txt")				#==>change file name as needed
indata2 <- read.delim(file, header = TRUE, sep = "\t", colClasses = c(ID_SAMPLES = "character")) 	#==>reads in tab-delimited file
#indata2[indata2$ID_SAMPLES=="960927027",]

indata <- indata[which(indata$SITE_ID %in% c(unique(indata2$STATION), "BWS28B")),] #Lindy's pull must have recoded this station
indata$DATE <- as.POSIXct(indata$DATE)
indata$Year <- as.integer(format(indata$DATE, "%Y"))
#indata <- indata[which(indata$Year < 2024),]
#indata2$ID_SAMPLES[which(!indata2$ID_SAMPLES %in% indata$ID_SAMPLES)]
#[1] "960927027" "970805013" "960826014"

indata[indata$DATE == as.POSIXct("1996/9/27 9:58"),]$ID_SAMPLES <- "960927027"
indata[indata$DATE == as.POSIXct("1997/8/5 11:38"),]$ID_SAMPLES <- "970805013"
indata[indata$DATE == as.POSIXct("1996/8/26 8:27"),]$ID_SAMPLES <- "960826014"
#indata <- replace(indata, is.null(indata), "NA")		#==>replace empty cells with NA

#This is where Lindy got rid of fish > 300mm
indata[indata$ID_SAMPLES=="71327",]$TOTAL_COUNTED <- 0
indata[indata$ID_SAMPLES=="71354",]$TOTAL_COUNTED <- 1
indata[indata$ID_SAMPLES=="960920025",]$TOTAL_COUNTED <- 0

dim(indata)				#==>check size of input data
state <- indata[which(!is.na(indata$STATE)),]
state <- state[which(state$STATE!="XX"),]
indata$STATE <- state$STATE[match(indata$SITE_ID, state$SITE_ID)]
#==>If any of the values for continuous covariates are orders of magnitude different from others, may want to rescale
#==>values (e.g., divide all by 1,000) so on similar scale to other measurements

#-----------------------------------------------------------------------------------------------------------------------
#-PREPARE DATA----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-Create Categorical Variables for Factors------------------------------------------------------------------------------
#==>Year should always be a categorical variable------------------------------------------------------------------------
#==>Relabel columns to match variable names in code below
indata$Year <- as.factor(indata$Year)	#==>make Year a categorical variable
levels(indata$Year)				#==>check levels of categorical Year

indata$Fish <- indata$TOTAL_COUNTED

str(indata)					#==>make sure data columns formatted as correct types

#-Create Offset Variable------------------------------------------------------------------------------------------------
#==>Needed to account for differences in effort across observations (see Crawley 2007; Zuur et al. 2009, 2012)

#data$LogEffort <- log(data$Effort)

#==>Not needed if effort constant across rows; only include in models if used
#==>Not needed for PSEG Black Drum

########################################################################################################################
##EXAMINE DATA##########################################################################################################
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
#-OBSERVED FREQUENCIES--------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#--Frequency Table------------------------------------------------------------------------------------------------------

frequencies <- table(indata$Fish)
frequencies

#-Create Cleveland Dotplot for Counts-----------------------------------------------------------------------------------

dotchart(indata$Fish, main = "Black Drum",xlab="Number of Fish",ylab="Order of the Data from Input File")

#-Create Frequency Plot for Counts--------------------------------------------------------------------------------------

#win.graph()		#==>start new graph window
plot(table(indata$Fish),type="h",main="PSEG Seine Survey Black Drum",xlab="Observed Catch",ylab="Frequency")
text(x=50,y=6000,"Proportion Zero Observations = 0.93")

#==>Examination of above graphs should give indication of whether data are zero-inflated; generally, data 
#==>characterized by >60% zeros are good candidates for zero-inflation; recommend to run Score Test (SAS code available;
#==>R code not yet available) to verify zero-inflation; if zero-inflation is detected, consider zero-inflated
#==>or zero-altered (hurdle) model

#-----------------------------------------------------------------------------------------------------------------------
#-CHECK FOR OUTLIERS----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#==>Better to do individual plots than multi-panel plot because sharing of x-axis can cause weird scaling effect
#==>that may make some data points appear as outliers


plot(indata$Fish)
plot(indata$Year)
indata$Month <- as.integer(format(indata$DATE, "%m"))
#test <- merge(indata, indata2, by="ID_SAMPLES")
#test[which(test$TOTAL_COUNTED!=test$Count),]

plot(indata$Month)
#plot(indata$STATE)

#-----------------------------------------------------------------------------------------------------------------------
#-CHECK COLLINEARITY----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#==>Presence of collinearity can result in inflated p-values and make it difficult to estimate effects of covariates

#-Create Correlation Plots----------------------------------------------------------------------------------------------
#==>Gives visual indication of potential collinearity
#pairs(~Year+Month+STATE,data=indata)

#-Calculate Variance Inflation Factors----------------------------------------------------------------------------------
#==>Variance Inflation Factors (VIFs) change as variables are removed so best to take a stepwise approach until all
#==>VIFs are below a pre-specified threshold
#==>VIF values > 3 are cause for concern and indicates presence of collinearity

MyX2 <- c("Year","Month","STATE")
corvif(indata[,MyX2])

#Console Output
#Variance inflation factors

#           GVIF Df GVIF^(1/2Df)
#Year  1.015281 28     1.000271
#Month 1.015277  1     1.007610
#State 1.000004  1     1.000002 

#All VIF values < 2

#==>If need to remove, remove one at a time and redo analysis; repeat process until all less than 3
#==>Don't ever want to remove Year-->needed for creating standardized index, better to remove the covariate with 
#==>which it is correlated

#-Correlation Analysis--------------------------------------------------------------------------------------------------
#==>If have no missing values, can run correlation analysis on pairs of variables using cor(x,y); this calculates
#==>Pearson correlation coefficients

#cor(data$BTemp,data$BSal)	#==>example of what code looks like; doesn't work for 'dummy2.txt' due to missing values

########################################################################################################################
##MODELS################################################################################################################
########################################################################################################################
#-Some Notes on Dispersion----------------------------------------------------------------------------------------------
#==>Here, selection of appropriate model is initially guided by the concept of dispersion--the relationship of the
#==>variance to the mean; the Poisson is a natural choice for counts and assumes the variance is equal to the mean;
#==>for ecological studies, the data are more often characterized by a variance that is larger than the mean-->a case
#==>of overdispersion; a less common situation is underdispersion in which the variance is less than the mean
#==>So, where does overdispersion come from? note that if there are sources of overdispersion that cannot be attributed 
#==>to excess zeros, failure to account for [those sources] them constitutes a model misspecification, which results in
#==>biased standard errors; overdispersion can result from missing covariates, missing interactions, outliers,
#==>spatial or temporal correlation, excessive number of zeros, noisy data, or a variance truly larger than the mean, to
#==>name a few (there are more sources, those listed are common)
#==>If considered all sources and examination suggests variance is truly larger than the mean, then apply a model that
#==>allows for (negative binomial) or accounts for (quasi-Poisson) this situation; if have excess zeros and no extra 
#==>overdispersion than a zero-inflated Poisson or zero-altered (hurdle) Poisson is appropriate; if have excess zeros
#==>and extra overdispersion, then a zero-inflated negative binomial or zero-altered (hurdle) negative binomial is
#==>recommended

#-Some Notes on Selecting Covariates------------------------------------------------------------------------------------
#==>There are different rules of thumb with regards to how many positive observations are needed per model parameter
#==>(covariate) to reduce bias and risk of overfitting; some papers recommend 5-10 positive events while others 
#==>recommend at least 30; for zero-inflated and zero-altered models, recommend using the guideline of 30 positive events 
#==>per model parameter
#==>For categorical covariates, each level is considered a separate parameter
#==>If determine that only significant covariate is Year, then calculate index using appropriate design-based estimator

#-----------------------------------------------------------------------------------------------------------------------
#-NON-STRATIFIED GLMS---------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#==>Interactions not considered in order to keep things simple 
#-Poisson---------------------------------------------------------------------------------------------------------------

P1 <- glm(Fish~Year+Month+SITE_ID+STATE,data=indata,family=poisson)
summary(P1)	#==>examine standard errors of covariates; unusually large values indicate problem with model
#==>Large stadard errors for STATIONs
#==>Run model without STATION
P2 <- glm(Fish~Year+Month+STATE,data=indata,family=poisson)
summary(P2)	#==>examine standard errors of covariates;
#Call:
#glm(formula = Fish ~ Year + Month + State, family = poisson, 
 #   data = data)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5717  -0.6583  -0.4539  -0.3059  21.7737  

#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  0.67650    0.21636   3.127 0.001767 ** 
#Year1996    -0.64436    0.26948  -2.391 0.016797 *  
#Year1997    -1.89520    0.43780  -4.329 1.50e-05 ***
#Year1998     0.25708    0.22663   1.134 0.256650    
#Year1999    -0.39304    0.24907  -1.578 0.114560    
#Year2000     0.02066    0.22224   0.093 0.925942    
#Year2001     1.12330    0.18202   6.171 6.77e-10 ***
#Year2002    -0.13703    0.20029  -0.684 0.493855    
#Year2003    -0.45389    0.21354  -2.126 0.033542 *  
#Year2004    -0.84522    0.23557  -3.588 0.000333 ***
#Year2005     0.08909    0.19260   0.463 0.643669    
#Year2006    -2.39582    0.40993  -5.844 5.08e-09 ***
#Year2007     0.46229    0.18272   2.530 0.011403 *  
#Year2008    -0.35274    0.20906  -1.687 0.091556 .  
#Year2009    -1.56914    0.29612  -5.299 1.16e-07 ***
#Year2010    -1.08739    0.25229  -4.310 1.63e-05 ***
#Year2011     0.81268    0.17589   4.620 3.83e-06 ***
#Year2012     0.19554    0.18958   1.031 0.302338    
#Year2013     0.57093    0.18039   3.165 0.001551 ** 
#Year2014    -0.40990    0.21165  -1.937 0.052786 .  
#Year2015     0.71452    0.17765   4.022 5.77e-05 ***
#Year2016    -0.35274    0.20906  -1.687 0.091556 .  
#Year2017     0.10093    0.19223   0.525 0.599575    
#Year2018     0.51809    0.18150   2.855 0.004310 ** 
#Year2019    -1.45135    0.28416  -5.108 3.26e-07 ***
#Year2020     0.36780    0.18493   1.989 0.046711 *  
#Year2021    -0.67816    0.22546  -3.008 0.002630 ** 
#Year2022     0.11262    0.19188   0.587 0.557242    
#Year2023    -0.84522    0.23557  -3.588 0.000333 ***
#Month       -0.21300    0.01657 -12.856  < 2e-16 ***
#StateNJ     -1.17723    0.04821 -24.419  < 2e-16 ***    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for poisson family taken to be 1)

#Null deviance: 12594  on 10400  degrees of freedom
#Residual deviance:  10939  on 10370  degrees of freedom
#AIC: 12771

#Number of Fisher Scoring iterations: 7

#==>Now, calculate dispersion

EP2 <- resid(P2,type="pearson")
Dispersion <- sum(EP2^2)/P2$df.resid
Dispersion

#==>Ideally, dispersion should be close to 1 for a Poisson; when values >~1.5 may want to consider a negative binomial;
#==>if not sure, can always run negative binomial and compare to Poisson (using odTest) to see which provides better fit;

#> EP1 <- resid(P1,type="pearson")
#> Dispersion <- sum(EP1^2)/P1$df.resid
#> Dispersion
#[1] 5.211912      
#-- WILL NOT USE POISSON

#-Negative Binomial-----------------------------------------------------------------------------------------------------

NB1<-glm.nb(Fish~Year+Month+SITE_ID+STATE,data=indata)	
summary(NB1)	#==>examine standard errors of covariates; unusually large values indicate problem with model
#==>Large stadard errors for STATIONs
#==>Run model without STATION
NB2<-glm.nb(Fish~Year+Month+STATE,data=indata)#These are the covariates used in the 2015 Assessment
summary(NB2) #==>examine standard errors of covariates; unusually large values indicate problem with model

#glm.nb(formula = Fish ~ Year + Month + State, data = data, init.theta = 0.05882715015, 
#       link = log)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6219  -0.4292  -0.3444  -0.2544   3.5168  

#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  1.17598    0.45826   2.566 0.010282 *  
#Year1996    -0.50708    0.46883  -1.082 0.279436    
#Year1997    -1.84668    0.58434  -3.160 0.001576 ** 
#Year1998     0.23731    0.48755   0.487 0.626444    
#Year1999    -0.17738    0.45462  -0.390 0.696414    
#Year2000     0.08509    0.44560   0.191 0.848567    
#Year2001     1.02362    0.42630   2.401 0.016343 *  
#Year2002    -0.16150    0.40274  -0.401 0.688412    
#Year2003    -0.39659    0.40848  -0.971 0.331604    
#Year2004    -0.78174    0.42078  -1.858 0.063193 .  
#Year2005     0.26255    0.39475   0.665 0.505983    
#Year2006    -2.52958    0.55642  -4.546 5.46e-06 ***
#Year2007     0.59346    0.39023   1.521 0.128311    
#Year2008    -0.58084    0.41393  -1.403 0.160548    
#Year2009    -1.46956    0.45441  -3.234 0.001221 ** 
#Year2010    -1.35825    0.44731  -3.036 0.002394 ** 
#Year2011     0.26277    0.39454   0.666 0.505405    
#Year2012    -0.24085    0.40482  -0.595 0.551869    
#Year2013     0.37197    0.39311   0.946 0.344037    
#Year2014    -0.67455    0.41699  -1.618 0.105734    
#Year2015     0.77931    0.38821   2.007 0.044701 *  
#Year2016    -0.65062    0.41618  -1.563 0.117985    
#Year2017     0.07945    0.39786   0.200 0.841711    
#Year2018     0.58855    0.39029   1.508 0.131557    
#Year2019    -1.50832    0.45689  -3.301 0.000962 ***
#Year2020     0.52507    0.39106   1.343 0.179373    
#Year2021    -0.71487    0.41838  -1.709 0.087512 .  
#Year2022     0.12985    0.39695   0.327 0.743571    
#Year2023    -1.12080    0.43513  -2.576 0.010000 *  
#Month       -0.26451    0.03620  -7.307 2.74e-13 ***
#StateNJ     -1.17282    0.10184 -11.516  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for Negative Binomial(0.06) family taken to be 1)

#    Null deviance: 2463.6  on 10400  degrees of freedom
#Residual deviance: 2053.3  on 10370  degrees of freedom
#AIC: 7182.7

#@Number of Fisher Scoring iterations: 1


#Theta:  0.06003  
#Std. Err.:  0.00337 

#2 x log-likelihood:  -7118.68500 

drop1(NB2,test="Chi")

#======Console Output=================================================
#Fish ~ Year + Month + State
#       Df Deviance    AIC     LRT  Pr(>Chi)    
#<none>      1854.7 6487.1                     
#Year   28   2271.2 7342.6 217.897   <2e-16 ***
#Month   1   2086.1 7211.5  32.841    1e-08 ***
#State   1   2178.8 7304.2 125.531   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#==>Compare negative binomial to equivalent Poisson model

odTest(NB2)

#======Console Output=================================================
#Likelihood ratio test of H0: Poisson, as restricted NB model:
#n.b., the distribution of the test-statistic under H0 is non-standard
#e.g., see help(odTest) for details/references

#Critical value of test statistic at the alpha= 0.05 level: 2.7055 
#Chi-Square Test Statistic =  5590.5044 p-value = < 2.2e-16 

#==>A significant P-value suggests the negative binomial provides a better fit than the Poisson

#==>Negative binomial a better fit and also used in 2015 Assessment

#==>Calculate dispersion

ENB2 <- resid(NB2,type="pearson")
Dispersion <- sum(ENB2^2)/NB2$df.resid
Dispersion

#[1]  1.245437

#-----------------------------------------------------------------------------------------------------------------------
########################################################################################################################
##DIAGNOSTICS###########################################################################################################
########################################################################################################################

Best <- NB2		#==>name of best model
Ebest <- ENB2		#==>residuals from best model; will not have for quasi-Poisson

#-----------------------------------------------------------------------------------------------------------------------
#-SIGNIFICANCE TEST-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#==>Test if model is overall statistically significant

lrtest(Best)

#============Console Output==========================================
#Likelihood ratio test
#
#Model 1: Fish ~ Year + Month + State 
#Model 2: Fish ~ 1
#   Df  LogLik  Df  Chisq Pr(>Chisq)    
#1  32 -3559.3                          
#2   2 -3737.6 -30 356.5  < 2.2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#-----------------------------------------------------------------------------------------------------------------------
#-GRAPHICAL DIAGNOSTICS-------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#==>For zero-inflated and zero-altered, residual plots are difficult to interpret as pattern is expected
#==>For stratified GLM, residuals should be plotted by strata (code for that not given here)
#-Usual Plots-----------------------------------------------------------------------------------------------------------

plot(Best)

#-Look for Evidence of Strong Outliers----------------------------------------------------------------------------------

plot(residuals(Best)~fitted(Best))

#-Create Index Plot of the Pearson Residuals----------------------------------------------------------------------------

plot(Ebest,ylab="Pearson Residuals")

########################################################################################################################
##CREATE INDEX##########################################################################################################
########################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------
#-NON-STRATIFIED GLMS---------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

Best <- NB2		#==>name of best model

Ebest <- ENB2

#-List Significant Covariates-------------------------------------------------------------------------------------------

Best$terms

#-Create Dummy Data Frame (based on significant covariates)-------------------------------------------------------------
#==>This new data object will include all levels of year and mean values of significant continuous covariates and
#==>mean of LogEffort if that was used 
#==>If there are significant covariates that are categorical (other than year), then create a variable of the 
#==>same name for that covariate and set equal to the first level in the code below; for example, if there was a 
#==>significant categorical covariate named "Area" and had possible values of "A", "B", and "C", add Area="A" to the
#==>code below

p.data<-data.frame(Year=levels(indata$Year),Month=mean(indata$Month,na.rm=T),STATE="DE",na.rm=T)

#-Get Means and SE for Year in Original Scale---------------------------------------------------------------------------

out<-predict(Best,newdata=p.data,type="response",se.fit=T)

yr.mean<-as.data.frame(cbind(as.numeric(levels(indata$Year)),out$fit,out$se.fit))
names(yr.mean)<-c("Year","Mean","SE")
yr.mean$CV<-yr.mean[,3]/yr.mean[,2]
yr.mean$LCI<-yr.mean[,2]-1.96*yr.mean[,3]
yr.mean$UCI<-yr.mean[,2]+1.96*yr.mean[,3]
yr.mean
#-Plot Means and 95% CIs------------------------------------------------------------------------------------------------

plot(yr.mean[,2]~yr.mean[,1],xlab="Year",ylab="Mean",main="PSEG Black Drum Index",
ylim=c(min(yr.mean[,5]),max(yr.mean[,6])),type="b")
segments(x0=yr.mean[,1],x1=yr.mean[,1],y0=yr.mean[,5],y1=yr.mean[,6])

#-Write Out Results-----------------------------------------------------------------------------------------------------
#==>change indirectory path as needed

write.csv(yr.mean,file=file.path(indir, "PSEGNegBinomialIndex-jess.csv"))	

#######################################################################################################################
##THE END ##########################################################################################
########################################################################################################################
