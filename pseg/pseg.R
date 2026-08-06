#for accessing access db's MUST RUN WITH R-4.1.3
#open powershell
#cd "C:\Program Files\R\R-4.1.3\bin"
#.\Rscript.exe <path to here> pseg.R
library(RODBC)

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