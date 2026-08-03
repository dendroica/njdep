#library(devtools)
#dev_mode(on=T)
#devtools::update_packages("mriptools")

#THIS IS SETUP TO GENERATE OUR OUTLIERS FOR REVIEW IN-HOUSE
#Run at office so output "OUT" goes to V drive
library(mriptools)

###EDIT THIS#########
comparison_timespan = c(2017:2019, 2021:2024)
prelim_yr = 2025
waves = 5 #c(2, 3, 4, 5, 6)
#########

myspecies <- c(
  "ATLANTIC CROAKER",
  "BLACK DRUM",
  "BLACK SEA BASS",
  "BLUEFISH",
  "COBIA",
  "DOLPHIN",
  "RED DRUM",
  "STRIPED BASS",
  "SUMMER FLOUNDER",
  "TAUTOG", "ATLANTIC MENHADEN", "SMOOTH DOGFISH", "WHITE PERCH", "SCUP",
  "SPINY DOGFISH", "SPOT", "WEAKFISH", "WINTER FLOUNDER"
)

modes = c("CHARTER BOAT", "PARTY BOAT", "PRIVATE/RENTAL BOAT", "SHORE")
areas = c("INLAND", "OCEAN (<= 3 MI)", "OCEAN (> 3 MI)")

Map(function(state, z) {
MRIPData <- CompileMRIPData(
  comparison_timespan,
  prelim_yr,
  waves,
  areas,
  modes,
  state
) # indir="~/data/MRIP",
#return(list(catchall, effortall))

my_outdir <- file.path(Sys.getenv("OUT"), prelim_yr, waves)
combined_catch <- OutlieMRIP(MRIPData[[1]], MRIPData[[2]], comparison_timespan, prelim_yr, species=myspecies)

#combined_catch2 <- OutlieMRIP(MRIPData[[1]], MRIPData[[2]], start_yr, end_yr, 
#                               prelim_yr, species, aggregate_factors = c("COMMON"))
#the order of variables in the outlier list: "TOT_CAT", "LANDING", "ESTREL"
totcat_outliers <- combined_catch[[1]][which(combined_catch[[1]]$outlier), ]

outliers <- lapply(combined_catch[1:3], function(x) {
 outlier <- x[which(x$outlier),]  
 print(outlier)
 outlier <- outlier[outlier$COMMON %in% myspecies,] 
 if(nrow(outlier) > 0 & !all(is.na(outlier))) {
   lapply(unique(outlier$COMMON), function(y) {
     print(y)
     dir.create(file.path(my_outdir, y), recursive = TRUE)
     outlier_spp <- outlier[outlier$COMMON==y,]
     outlier_spp$state <- z
     write.csv(outlier_spp, file.path(my_outdir, y, paste(unique(outlier_spp$var), "outliers.csv", sep = "-")))
     #write.csv(outlier_spp, file = file.path("~/output/outlier", prelim_yr, paste(unique(outlier_spp$var), "outliers.csv", sep = "-")), append=T)
     #dir.create(file.path(my_outdir, y), recursive = TRUE)
     outlier_waves <- outlier$WAVE
     outlier_var <- unique(outlier_spp$var)
     Plot(MRIPData[[1]], MRIPData[[2]], y, outlier_waves, vars = outlier_var,
               outdir = my_outdir)
   }) #catch, effort, species, waves, outdir
 } else {outlier_spp = data.frame()}
})
}, 34, "NJ") 


#c(37, 51, 24, 10, 34, 36, 9, 44, 25, 33, 23), c("NC", "VA","MD","DE","NJ","NY", "CT", "RI", "MA", "NH", "ME"))
#dev_mode(on = FALSE)