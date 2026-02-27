usr <- "jgorzo"
#mypath <- "C:/Users/galax/Downloads/SASLoads/SASLoads"
mypath <- "V:/Marine Fish/Lindy/OceanTrawl/SASLoads"
mywd <- file.path("C:/Users", usr, "Documents/njdep/LINDY")
setwd(mywd)
#source("OCEAN TRAWL ABUNDANCE WGTED-ARIM.R")
#source("OCEAN TRAWL ABUNDANCE LFD WGTED.R")
source("WGTED-ARIM.R")
source("LFD WGTED.R")
outdir <- file.path("C:/Users", usr,
                    "OneDrive - New Jersey Office of Information Technology/Documents/output/sasr")
#sppdata <- Spp(mypath, "Black drum", "INM", "Oct", outdir="~/output/sasr")
#sppdata <- Spp(mypath, "Scup", outdir=outdir) #"INM", "AugOct"
#sppdata <- Spp(mypath, spp="Spot", area="INM", cruise="AugOct", outdir=outdir)
#sppdata <- Spp(mypath, "Summer flounder", outdir="~/output/sasr")
#sppdata <- Spp(mypath, "Summer flounder", cruise="AprthruOct", outdir="~/output/sasr")
#sppdata <- Spp(mypath, "Tautog", outdir="~/output/sasr")
#sppdata <- Spp(mypath, "Weakfish", cruise="AugOct", outdir="~/output/sasr")
##sppdata <- Spp(mypath, "Lobster - F (GE53mm)")
##sppdata <- Spp(mypath, "Lobster - M (GE53mm)")
#Spp(mypath, "Atl croaker", cruise="AugOct", outdir=outdir)
#sppdata <- lfd(mypath, "Summer flounder", cruise="AprthruOct", outdir="~/output/sasr")
sppdata <- Spp(mypath, spp="Lobster - F (GE53mm)", outdir="~/output/sasr")
sppdata <- Spp(mypath, "Lobster - F (GE53mm)", cruise="Spring", outdir="~/output/sasr")
sppdata <- Spp(mypath, "Lobster - F (GE53mm)", cruise="Oct", outdir="~/output/sasr")
sppdata <- lfd(mypath, spp="Lobster - F (GE53mm)", cruise="Spring", outdir="~/output/sasr")
sppdata <- lfd(mypath, "Lobster - F (GE53mm)", cruise="Oct", outdir="~/output/sasr")

sppdata <- Spp(mypath, "Lobster - M (GE53mm)", outdir="~/output/sasr")
sppdata <- Spp(mypath, "Lobster - M (GE53mm)", cruise="Spring", outdir="~/output/sasr")
sppdata <- Spp(mypath, "Lobster - M (GE53mm)", cruise="Oct", outdir="~/output/sasr")
sppdata <- lfd(mypath, "Lobster - M (GE53mm)", cruise="Spring", outdir="~/output/sasr")
sppdata <- lfd(mypath, "Lobster - M (GE53mm)", cruise="Oct", outdir="~/output/sasr")

