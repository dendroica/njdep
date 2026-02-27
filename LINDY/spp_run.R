usr <- "jgorzo"
#input <- "C:/Users/galax/Downloads/SASLoads/SASLoads"
input <- "V:/Marine Fish/Lindy/OceanTrawl/SASLoads"
mywd <- file.path("C:/Users", usr, "Documents/njdep/LINDY")
setwd(mywd)
#source("OCEAN TRAWL ABUNDANCE WGTED-ARIM.R")
#source("OCEAN TRAWL ABUNDANCE LFD WGTED.R")
source("WGTED-ARIM.R")
source("LFD WGTED.R")
outdir <- file.path("C:/Users", usr,
                    "OneDrive - New Jersey Office of Information Technology/Documents/output/sasr")
#sppdata <- Spp(input, "Black drum", "INM", "Oct", outdir="~/output/sasr")
#sppdata <- Spp(input, "Scup", outdir=outdir) #"INM", "AugOct"
#sppdata <- Spp(input, spp="Spot", area="INM", cruise="AugOct", outdir=outdir)
#sppdata <- Spp(input, "Summer flounder", outdir="~/output/sasr")
#sppdata <- Spp(input, "Summer flounder", cruise="AprthruOct", outdir="~/output/sasr")
#sppdata <- Spp(input, "Tautog", outdir="~/output/sasr")
#sppdata <- Spp(input, "Weakfish", cruise="AugOct", outdir="~/output/sasr")
##sppdata <- Spp(input, "Lobster - F (GE53mm)")
##sppdata <- Spp(input, "Lobster - M (GE53mm)")
#Spp(input, "Atl croaker", cruise="AugOct", outdir=outdir)
#sppdata <- lfd(input, "Summer flounder", cruise="AprthruOct", outdir="~/output/sasr")
sppdata <- Spp(input, spp="Lobster - F (GE53mm)", outdir="~/output/sasr")
sppdata <- Spp(input, "Lobster - F (GE53mm)", cruise="Spring", outdir="~/output/sasr")
sppdata <- Spp(input, "Lobster - F (GE53mm)", cruise="Oct", outdir="~/output/sasr")
sppdata <- lfd(input, spp="Lobster - F (GE53mm)", cruise="Spring", outdir="~/output/sasr")
sppdata <- lfd(input, "Lobster - F (GE53mm)", cruise="Oct", outdir="~/output/sasr")

sppdata <- Spp(input, "Lobster - M (GE53mm)", outdir="~/output/sasr")
sppdata <- Spp(input, "Lobster - M (GE53mm)", cruise="Spring", outdir="~/output/sasr")
sppdata <- Spp(input, "Lobster - M (GE53mm)", cruise="Oct", outdir="~/output/sasr")
sppdata <- lfd(input, "Lobster - M (GE53mm)", cruise="Spring", outdir="~/output/sasr")
sppdata <- lfd(input, "Lobster - M (GE53mm)", cruise="Oct", outdir="~/output/sasr")

