library(utils)

mytest <- function() {
test <- select.list(sort(c("Atl croaker","Black drum","Horseshoe crab","Lobster - F (GE53mm)","Lobster - M (GE53mm)",
                   "Scup","Spot","Summer flounder")),multiple=FALSE)
return(test)}
mytest()