source("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/code/LINDY/OCEAN TRAWL ABUNDANCE WGTED-ARIM.R")
source("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/code/LINDY/OCEAN TRAWL ABUNDANCE LFD WGTED.R")
#the output list structure is... TSSTRATS, YSTRAT, MSTRAT, YMSTRATA
#YSTRAT = ANNUAL INDEX
#TSSTRATS = by stratum
#MSTRAT = by month
#YMSTRATA = by month and year (not usually needed)
mypath <- "C:/Users/jgorzo/Documents/SASLoads"
sppdata <- Spp(mypath, "Black drum", "INM", "Oct", outdir="~/output/sasr")
sppdata <- Spp(mypath, "Scup", "INM", "AugOct", outdir="~/output/sasr")
sppdata <- Spp(mypath, "Summer flounder", outdir="~/output/sasr")
sppdata <- Spp(mypath, "Summer flounder", cruise="AprOct", outdir="~/output/sasr")
sppdata <- Spp(mypath, "Tautog", outdir="~/output/sasr")
sppdata <- Spp(mypath, "Weakfish", cruise="AugOct", outdir="~/output/sasr")
#sppdata <- Spp(mypath, "Lobster - F (GE53mm)")
#sppdata <- Spp(mypath, "Lobster - M (GE53mm)")

#you might need to work through macro20 for lobster?
sppdata <- lfd(mypath, "Summer flounder", cruise="AprOct", outdir="~/output/sasr")
