usr <- "jgorzo"
#input <- "C:/Users/galax/Downloads/SASLoads/SASLoads"
input <- "V:/Marine Fish/Lindy/OceanTrawl/SASLoads"
repo_path <- file.path("C:/Users", usr, "Documents/njdep/LINDY")
setwd(repo_path)
source("WGTED-ARIM.R")
source("LFD WGTED.R")
outdir <- file.path("C:/Users", usr,
                    "OneDrive - New Jersey Office of Information Technology/Documents/output/sasr")

#index <- WgtedAriM(input, "Black drum", area="INM", cruise="Oct", outdir=outdir)
#index <- WgtedAriM(input, "Scup", outdir=outdir) #"INM", "AugOct"
#index <- WgtedAriM(input, spp="Spot", area="INM", cruise="AugOct", outdir=outdir)
#index <- WgtedAriM(input, "Summer flounder", outdir=outdir)
#index <- WgtedAriM(input, "Summer flounder", cruise="AprthruOct", outdir=outdir)
#index <- WgtedAriM(input, "Tautog", outdir=outdir)
#index <- WgtedAriM(input, "Weakfish", cruise="AugOct", outdir=outdir)
##index <- WgtedAriM(input, "Lobster - F (GE53mm)")
##index <- WgtedAriM(input, "Lobster - M (GE53mm)")
#WgtedAriM(mypath=input, myspp="Atl croaker", cruise="AugOct", outdir=outdir)
#lf <- LFD(input, "Summer flounder", cruise="AprthruOct", outdir=outdir)
#index <- WgtedAriM(input, myspp="Lobster - F (GE53mm)", outdir=outdir)
#index <- WgtedAriM(input, "Lobster - F (GE53mm)", cruise="Spring", outdir=outdir)
#index <- WgtedAriM(input, "Lobster - F (GE53mm)", cruise="Oct", outdir=outdir)
#lf <- LFD(input, myspp="Lobster - F (GE53mm)", cruise="Spring", outdir=outdir)
#lf <- LFD(input, "Lobster - F (GE53mm)", cruise="Oct", outdir=outdir)

#index <- WgtedAriM(input, "Lobster - M (GE53mm)", outdir=outdir)
#index <- WgtedAriM(input, "Lobster - M (GE53mm)", cruise="Spring", outdir=outdir)
#index <- WgtedAriM(input, "Lobster - M (GE53mm)", cruise="Oct", outdir=outdir)
#lf <- LFD(input, "Lobster - M (GE53mm)", cruise="Spring", outdir=outdir)
#lf <- LFD(input, "Lobster - M (GE53mm)", cruise="Oct", outdir=outdir)

#index <- WgtedAriM(input, "Striped bass", cruise="Apr", outdir=outdir)
lf <- LFD(input, "Striped bass", cruise="Apr", outdir=outdir)

