#https://cran.r-project.org/web/packages/ecostate/vignettes/model_of_intermediate_complexity.html
library(ecostate)
# load data
data(eastern_bering_sea)

# Reformat inputs
years = 1982:2021 # Catch only goes through 2021, and starting pre-data in 1982 doesn't play well with fit_B0
taxa = c( "Pollock", "Cod", "Arrowtooth", "Copepod", "Other_zoop", "Chloro", "NFS", "Krill", "Benthic_invert", "Benthos", "Detritus" )

# Define types
type_i = sapply( taxa, FUN=switch, "Detritus" = "detritus",
                 "Chloro" = "auto",
                 "hetero" )

# Starting values
U_i = EE_i = B_i = array( NA, dim=length(taxa), 
                          dimnames=list(names(eastern_bering_sea$P_over_B)))
B_i[c("Cod", "Arrowtooth", "NFS")] = c(1, 0.5, 0.02)
EE_i[] = 1
U_i[] = 0.2

# Define default vulnerability, except for primary producers
X_ij = array( 2, dim=c(length(taxa),length(taxa)) )
dimnames(X_ij) = list(names(B_i),names(B_i))
X_ij[,'Chloro'] = 91
