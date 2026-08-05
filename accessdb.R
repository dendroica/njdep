#for accessing access db's MUST RUN WITH R-4.1.3
#open powershell
#cd "C:\Program Files\R\R-4.1.3\bin"
#.\Rscript.exe <path to here> accessdb.R

library(RODBC)

trawl <- file.path(Sys.getenv("CRAB"), "Ocean Trawl Survey.accdb")
outfile <-  file.path(Sys.getenv("CRAB"), "crab.RData")
oceantrawl <- function(pathtodb, outfile) {
  dummy_db <- odbcConnectAccess2007(access.file = pathtodb)
  boats <- sqlQuery(dummy_db, "SELECT * FROM CATCH WHERE SPP = 314") #BLUE CRAB: SPP =  314
  tows <- sqlQuery(dummy_db, paste0("SELECT * FROM TOWS")) #WHERE ID NOT IN (", paste(boats$ID, collapse=","), ")"
  length <- sqlQuery(dummy_db, paste0("SELECT * FROM LENGTH WHERE SPP = 314")) #WHERE ID NOT IN (", paste(boats$ID, collapse=","), ")"
  odbcClose(dummy_db)
  save(boats,tows,length,file=outfile)
} 
#oceantrawl(trawl, outfile)

outfile <- "weakfish.RData"
seine <- file.path(Sys.getenv("VIN"), "Delaware River Seine Survey/Delaware River Seine 1980-2024_2-26-25.mdb")
weakfish_seine <- function(pathtodb, outfile) {
  dummy_db <- odbcConnectAccess2007(access.file = pathtodb)
  boats <- sqlQuery(dummy_db, 'SELECT * FROM "CATCH1980-2024" WHERE SpeciesCode = 9') #BLUE CRAB: SPP = 99
  tows <- sqlQuery(dummy_db, paste0('SELECT * FROM "HEAD1980-2024" WHERE ID NOT IN (', paste(boats$ID, collapse=","), ')'))
  odbcClose(dummy_db)
  save(boats,tows,file=outfile) #tows,
}

detrawl <- file.path(Sys.getenv("VIN"), "Delaware Bay Finfish Trawl Survey/DBTrawl1991-2024_12-26-2024.accdb")
weakfish_trawl <- function(pathtodb, outfile) {
  dummy_db <- odbcConnectAccess2007(access.file = pathtodb)
  boats <- sqlQuery(dummy_db, 'SELECT * FROM Weakfish') #BLUE CRAB: SPP = 99
  tows <- sqlQuery(dummy_db, paste0('SELECT * FROM "HEAD+CATCH"'))
  length <- sqlQuery(dummy_db, "SELECT * FROM LENGTHS WHERE COMMON_NAME = 'Weakfish'")
  odbcClose(dummy_db)
  save(boats,tows,length,file=outfile) #tows,
}
#weakfish_trawl(detrawl,outfile)

wd <- file.path(Sys.getenv("JEFF"), "rawdata")
datadictionary <- function(x) {
  print(x)
  dummy_db <- odbcConnectAccess2007(access.file = x)
  tbls <- subset(sqlTables(dummy_db), TABLE_TYPE == "TABLE", TABLE_NAME)[, 1]
  tabs <- lapply(tbls, sqlFetch, 
                 channel = dummy_db,
                 as.is=T) #you need this in order to simplify/have it not try to guess data types
  odbcClose(dummy_db)
  names(tabs) <- tbls
  #print(tabs)
  return(list(tabs,tbls))}
#datatabs <- datadictionary(file.path(wd, "documentation/Data Dictionary.accdb"))
#save(datatabs, file="pseg_dict.RData")

alldb <- list.files(pattern="^[^D]*[.]accdb", file.path(wd, "rawdata") , full.names=T, recursive=T)

dbs <- function(z) {lapply(z, function(x) {
  readtabs <- datadictionary(x)
  tabs <- readtabs[[1]]
  tosort <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x))
  if(grepl("^[0-9]", tosort)) {
    yr<- substr(tosort, 1, 4)
    dataname <- substr(tosort, 6, nchar(tosort))
    tosort <- paste(dataname, yr)
  }
  if(grepl("^PSEG", tosort)) {
    tosort <- substr(tosort, 6, nchar(tosort))
  }
  avicii <- gsub('.*Level\\s([0-9]).*','\\1', readtabs[[2]])
  names(tabs) <- paste("Level ", avicii, "_", tosort, "_", x, sep="")
  #print(tabs)
  return(tabs)})}

#dbnames <- getnames(alldb) #function you accidentally overwrote
#save(dbnames, file="pseg2.RData")

mydbs <- dbs(alldb)
save(mydbs, file="pseg_data.RData")
#save(mydbs, file="pseg_data_types.RData") this was without as.is=T so that you could see data typing issues from error values

#I think just nest lapply for now to just get column names and flatten?
#return rows of a df you can rbind (db name, table name, field names...)
#possibly tibble so you can have vector of names nested, and maybe also static cat string
odbcCloseAll()
