#for accessing access db's MUST RUN WITH R-4.1.3
#open powershell
#cd "C:\Program Files\R\R-4.1.3\bin"
#.\Rscript.exe <path to here> accessdb.R
library(RODBC)

trawl <- file.path(Sys.getenv("CRAB"), "Ocean Trawl Survey.accdb")
outfile <-  file.path(Sys.getenv("CRAB"), "spot.RData")
oceantrawl_spp <- function(path_to_db, outfile) {
  dummy_db <- odbcConnectAccess2007(access.file = path_to_db)
  boats <- sqlQuery(dummy_db, "SELECT * FROM CATCH WHERE SPP = 149") #BLUE CRAB: SPP =  314
  tows <- sqlQuery(dummy_db, paste0("SELECT * FROM TOWS")) #WHERE ID NOT IN (", paste(boats$ID, collapse=","), ")"
  length <- sqlQuery(dummy_db, paste0("SELECT * FROM LENGTH WHERE SPP = 149")) #WHERE ID NOT IN (", paste(boats$ID, collapse=","), ")"
  odbcClose(dummy_db)
  save(boats,tows,length,file=outfile)
} 
#oceantrawl_spp(trawl, outfile)

outfile <-  file.path(Sys.getenv("CRAB"), "njot_all.RData")
oceantrawl <- function(path_to_db, outfile) {
  dummy_db <- odbcConnectAccess2007(access.file = path_to_db)
  catch <- sqlQuery(dummy_db, "SELECT * FROM CATCH")
  tows <- sqlQuery(dummy_db, paste0("SELECT * FROM TOWS"))
  length <- sqlQuery(dummy_db, paste0("SELECT * FROM LENGTH"))
  odbcClose(dummy_db)
  save(catch,tows,length,file=outfile)
} 
oceantrawl(trawl, outfile)

outfile <- "weakfish.RData"
seine <- file.path(Sys.getenv("VIN"), "Delaware River Seine Survey/Delaware River Seine 1980-2024_2-26-25.mdb")
weakfish_seine <- function(path_to_db, outfile) {
  dummy_db <- odbcConnectAccess2007(access.file = path_to_db)
  boats <- sqlQuery(dummy_db, 'SELECT * FROM "CATCH1980-2024" WHERE SpeciesCode = 9') #BLUE CRAB: SPP = 99
  tows <- sqlQuery(dummy_db, paste0('SELECT * FROM "HEAD1980-2024" WHERE ID NOT IN (', paste(boats$ID, collapse=","), ')'))
  odbcClose(dummy_db)
  save(boats,tows,file=outfile) #tows,
}

detrawl <- file.path(Sys.getenv("VIN"), "Delaware Bay Finfish Trawl Survey/DBTrawl1991-2024_12-26-2024.accdb")
weakfish_trawl <- function(path_to_db, outfile) {
  dummy_db <- odbcConnectAccess2007(access.file = path_to_db)
  boats <- sqlQuery(dummy_db, 'SELECT * FROM Weakfish') #BLUE CRAB: SPP = 99
  tows <- sqlQuery(dummy_db, paste0('SELECT * FROM "HEAD+CATCH"'))
  length <- sqlQuery(dummy_db, "SELECT * FROM LENGTHS WHERE COMMON_NAME = 'Weakfish'")
  odbcClose(dummy_db)
  save(boats,tows,length,file=outfile) #tows,
}
#weak <- weakfish_trawl(detrawl,file.path(Sys.getenv("FILEPATH"), "weakfish.RData"))
odbcCloseAll()
