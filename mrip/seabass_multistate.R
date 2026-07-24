#library(mriptools)
myspecies <- c(
  "BLACK SEA BASS"
)


# NC=37, VA=51, MD=24, DE=10, NJ=34, NY=36, CT=9, RI=44, MA=25, NH=33, ME=23
Map(function(x, y) {
my_outdir <- file.path("~/output", y, "2024")
mrip(
  comparison_timespan = c(2017:2019,2021:2023),
  prelim_yr = 2024,
  species = myspecies,
  waves = c(2, 3, 4, 5, 6),
  areas = c("INLAND", "OCEAN (<= 3 MI)", "OCEAN (> 3 MI)"),
  modes = c("CHARTER BOAT", "PARTY BOAT", "PRIVATE/RENTAL BOAT", "SHORE"),
  state = x,
  out_dir = my_outdir
)}, c(37, 51, 24, 10, 34, 36, 9, 44, 25, 33, 23), c("NC", "VA","MD","DE","NJ","NY", "CT", "RI", "MA", "NH", "ME"))
