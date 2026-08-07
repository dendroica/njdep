library(mriptools)
catch <- ReadCatch(file.path("https://apps-st.fisheries.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/Wave%20Level%20Estimate%20Downloads", "mrip_catch_bywave_2025.csv"), 34, 1:6)

catch <- catch[catch$COMMON=="WEAKFISH",]
sum(catch$LBS_AB1) #lbs
sum(catch$LANDING) #
sum(catch$ESTREL) #released
