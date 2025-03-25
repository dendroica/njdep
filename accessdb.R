#for accessing access db's MUST RUN WITH R-4.1.3
#open powershell
#cd .\Documents\R\R-4.1.3\bin
#Rscript C:\Users\jgorzo\Documents\crab\accessdb.R

library(RODBC)

outfile <- "c:/Users/jgorzo/Documents/crab/crab.RData"
oceantrawl <- function(pathtodb, outfile) {
  dummy_db <- odbcConnectAccess2007(access.file = pathtodb)
  boats <- sqlQuery(dummy_db, "SELECT * FROM CATCH WHERE SPP = 314") #BLUE CRAB: SPP =  314
  tows <- sqlQuery(dummy_db, paste0("SELECT * FROM TOWS")) #WHERE ID NOT IN (", paste(boats$ID, collapse=","), ")"
  length <- sqlQuery(dummy_db, paste0("SELECT * FROM LENGTH WHERE SPP = 314")) #WHERE ID NOT IN (", paste(boats$ID, collapse=","), ")"
  odbcClose(dummy_db)
  save(boats,tows,length,file=outfile)
} 

trawl <- "c:/Users/jgorzo/Documents/crab/Ocean Trawl Survey.accdb"
oceantrawl(trawl, outfile)

stripedbass <- function(pathtodb, outfile) {
  dummy_db <- odbcConnectAccess2007(access.file = pathtodb)
  boats <- sqlQuery(dummy_db, "SELECT * FROM CATCH1980-2023 WHERE SpeciesCode = 99") #BLUE CRAB: SPP = 99
  tows <- sqlQuery(dummy_db, paste0("SELECT * FROM TOWS WHERE ID NOT IN (", paste(boats$ID, collapse=","), ")"))
  odbcClose(dummy_db)
  save(boats,tows,file=outfile)
} 