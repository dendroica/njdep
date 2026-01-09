setwd("C:/Users/jgorzo/Documents/njdep/LINDY")
source("OCEAN TRAWL ABUNDANCE WGTED-ARIM.R")
source("OCEAN TRAWL ABUNDANCE LFD WGTED.R")
#the output list structure is... TSSTRATS, YSTRAT, MSTRAT, YMSTRATA
#YSTRAT = ANNUAL INDEX
#TSSTRATS = by stratum
#MSTRAT = by month
#YMSTRATA = by month and year (not usually needed)
#mypath <- "C:/Users/galax/Downloads/SASLoads/SASLoads"
mypath <- "C:/Users/jgorzo/Documents/SASLoads"
outdir <- "~/output"
#sppdata <- Spp(mypath, "Black drum", "INM", "Oct", outdir="~/output/sasr")
#sppdata <- Spp(mypath, "Scup", outdir=outdir) #"INM", "AugOct"
#sppdata <- Spp(mypath, "Spot", "INM", "AugOct", outdir=outdir)
#sppdata <- Spp(mypath, "Summer flounder", outdir="~/output/sasr")
#sppdata <- Spp(mypath, "Summer flounder", cruise="AprthruOct", outdir="~/output/sasr")
#sppdata <- Spp(mypath, "Tautog", outdir="~/output/sasr")
#sppdata <- Spp(mypath, "Weakfish", cruise="AugOct", outdir="~/output/sasr")
##sppdata <- Spp(mypath, "Lobster - F (GE53mm)")
##sppdata <- Spp(mypath, "Lobster - M (GE53mm)")

#you might need to work through macro20 for lobster?
sppdata <- lfd(mypath, "Summer flounder", cruise="AprthruOct", outdir="~/output/sasr")
