library(purrr)
library(dplyr)
library(tidyverse)
options(tibble.print_min = Inf)

load("~/data/pseg.RData")
flattened_list <- flatten(dbnames)
flattened_list <- lapply(flattened_list, sort)
#get out levels with regex
flattened_list <- flattened_list[order(names(flattened_list))]

beach <- list(
flattened_list$`Level 1_Beach Seine 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Beach Seine/Beach Seine 1995.accdb`,
flattened_list$`Level 2_Beach Seine 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Beach Seine/Beach Seine 1995.accdb`,
flattened_list$`Level 3_Beach Seine 1995_V:/Marine Fish/Jeff B/PSEG data/rawdata/Beach Seine/Beach Seine 1995.accdb`,
flattened_list$`Level 1_Beach Seine 2004_V:/Marine Fish/Jeff B/PSEG data/rawdata/Beach Seine/Beach Seine 2004.accdb`,
flattened_list$`Level 2_Beach Seine 2004_V:/Marine Fish/Jeff B/PSEG data/rawdata/Beach Seine/Beach Seine 2004.accdb`,
flattened_list$`Level 3_Beach Seine 2004_V:/Marine Fish/Jeff B/PSEG data/rawdata/Beach Seine/Beach Seine 2004.accdb`)

duplicates <- c(FALSE,sapply(seq(2:length(flattened_list))+1, function(x) all(flattened_list[[x]] == flattened_list[[x-1]]))) #%in%
#duplicates <- duplicated(flattened_list)
chunks <- split(which(duplicates), cumsum(c(1, diff(which(duplicates)) != 1)))
chunks <- lapply(chunks, function(x) c(x[1] - 1, x))

duplicate_elements <- lapply(chunks, function(x) flattened_list[x])

chunks <- as.integer(unlist(chunks))
chunks <- setdiff(min(chunks):max(chunks), chunks) #indices of the tables with no consecutive tables with the same column names (i.e. missing from above)

nonconform <- flattened_list[chunks] 
tbls <- lapply(nonconform, function(x) { #which tables have all of the column names of each "nonconform" table 
  which(unlist(lapply(flattened_list, function(y) {
    comparetbls <- unlist(all(x %in% y))
  })))})

nomatch <- tbls[which(unlist(lapply(tbls, length)) < 2)]
#all in all, there are only 3 tables left that can't be neatly consolidated with field name filling

load("~/pseg_data_types.RData")
flattendb <- flatten(dbs)
flattened <- flattendb[order(names(flattendb))]
surveyy <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', names(flattened))
flattened <- Map(function(x, y) {cbind(x, y)}, flattened, surveyy)
nested <- lapply(split(seq_along(surveyy), surveyy), function(x) flattened[x])

datatype <- lapply(lapply(nested, function(x) {
  map_df(x, function(x) sapply(map(x, class), "[[", 1))
}), bind_rows)

lapply(datatype, function(sample) which(apply(sample, 2, function(x) length(unique(x[!is.na(x)]))) != 1))

which(names(datatype)=="Level 2_Bottom Trawl")