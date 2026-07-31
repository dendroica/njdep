library(purrr)
library(dplyr)
library(tidyverse)
root <- Sys.getenv("VPATH")
path <- file.path(root, "pseg/pseg_data.RData") #
#"~/data/PSEG/pseg_data.RData"
load(path)
flattendb <- flatten(mydbs)
#flattendb <- sort(flattendb)
flattened <- lapply(seq_along(flattendb), function(x) { 
  y <- flattendb[[x]][order(colnames(flattendb[[x]]))]
  y$filename <- names(flattendb)[x]
  return(y)})

names(flattened) <- names(flattendb)

flattened <- flattened[order(names(flattened))]

#samplesint <- lapply(flattened, function(x){
#  print(is.integer(x$SAMPLNO))
#})

flattened <- lapply(flattened, function(x) {
  if(any(names(x)=="GRID")) {
    x$GRID <- as.integer(x$GRID)
  }
  
  if(any(names(x)=="STRATA")) {
    x$STRATA <- as.integer(x$STRATA)
  }
  
  if(any(names(x)=="DUR")) { #is DUR correct with decimals e.g. flattened[[59]]? flattened[[33]] -1234
    x$DUR <- as.character(x$DUR)
  }
  
  if(any(names(x)=="SAMPLNO")) { 
    x$SAMPLNO <- as.character(x$SAMPLNO) #Impingement data is all int
  }
  return(x)
})

dup <- c(FALSE,sapply(seq(2:length(flattened))+1, function(x) all(names(flattened[[x]]) == names(flattened[[x-1]]))))
chunkd <- split(which(dup), cumsum(c(1, diff(which(dup)) != 1)))
chunkd <- lapply(chunkd, function(x) c(x[1] - 1, x))
dups <- lapply(chunkd, function(x) flattened[x])
datanames <- unlist(lapply(dups, function(x) names(x)[1]))
dups <- purrr::map(dups, bind_rows)
names(dups) <- datanames
#dups <- lapply(dups, function(x) do.call(rbind, x))
chunkd <- as.integer(unlist(chunkd))
chunkd <- setdiff(min(chunkd):max(chunkd), chunkd) #indices of the tables with no consecutive tables with the same column names (i.e. missing from above)
nonconform <- flattened[chunkd] 
###### CHECK ALONG THE MERGED CHUNKS FOR FURTHER MERGING OF TABLES WITH THE SAME HEADERS
tbls <- lapply(seq_along(dups), function(x) { #which tables have all of the column names of each "nonconform" table 
  surveyx <- names(dups)[x]
  x <- dups[[x]]
  surveyx <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyx)
  which(
    unlist(
      lapply(seq_along(dups), function(y) {
        surveyy <- names(dups)[y]
        y <- dups[[y]]
        surveyy <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyy)
        comparetbls <- unlist(
          all((names(x)) == names(y))
        )
        return(c(comparetbls & surveyx==surveyy))
      }
      )
    )
  ) #[1]
}
)
#)

for (x in seq_along(tbls)) {
  if(length(tbls[[x]]) > 1) {
    tomerg <- tbls[[x]]
    if(x==tomerg[1]){
      combined <- bind_rows(dups[tomerg])
      dups[[x]] <- combined
      dups[tomerg[2:length(tomerg)]] <- NA
    }
  } 
}

dups <- dups[!is.na(dups)] 

##########OF THE SETS THAT MATCHED NOTHING SERIALLY (i.e. the gaps) DO THEY MATCH WITH JUST DIFFERENT CAPS?
tbls <- lapply(seq_along(nonconform), function(x) { #which tables have all of the column names of each "nonconform" table 
  surveyx <- names(nonconform)[x]
  x <- nonconform[[x]]
  surveyx <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyx)
  which(
    unlist(
      lapply(seq_along(dups), function(y) {
        surveyy <- names(dups)[y]
        y <- dups[[y]]
        surveyy <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyy)
        comparetbls <- unlist(
          all(tolower(names(x)) == tolower(names(y)))
        )
        return(c(comparetbls & surveyx==surveyy))
      }
      )
    )
  ) #[1]
}
)
#)

meld <- sapply(seq_along(tbls), function(x) {
  if(length(tbls[[x]]) > 0) {
    primary <- dups[[tbls[[x]]]]
    names(primary) <- tolower(names(primary)) 
    secondary <- nonconform[[x]]
    names(secondary) <- tolower(names(secondary)) 
    bind_rows(primary, secondary)
  }
})

dups[unlist(tbls)] <- meld[which(!sapply(meld,is.null))] #merged in tables to the main dataset that just needed a NA filled column
nonconform <- nonconform[is.na(tbls > 0)] #now, this is tables that haven't been merged in with anything else

#########OF THE SETS LEFT THAT DIDN'T MATCH SERIALLY, CAN WE COMBINE THEM WITH THE SAME DATASETS BY NA FILL?
tbls <- unlist(lapply(seq_along(nonconform), function(x) { #which tables have all of the column names of each "nonconform" table 
  surveyx <- names(nonconform)[x]
  x <- nonconform[[x]]
  surveyx <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyx)
  which(
    unlist(
    lapply(seq_along(dups), function(y) {
    surveyy <- names(dups)[y]
    y <- dups[[y]]
    surveyy <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyy)
    comparetbls <- unlist(
      all(names(x) %in% names(y))
      )
    return(c(comparetbls & surveyx==surveyy))
  }
  )
  )
  )[1]
  }
  ))

meld <- sapply(seq_along(tbls), function(x) {
  if(!is.na(tbls[x])) {
    bind_rows(dups[[tbls[x]]], nonconform[[x]])
  }
  })

dups[tbls[!is.na(tbls)]] <- meld[which(!sapply(meld,is.null))] #merged in tables to the main dataset that just needed a NA filled column
nonconform <- nonconform[is.na(tbls)] #now, this is tables that haven't been merged in with anything else

#####NA FILL THE OTHER DIRECTION (i.e. unmatched datasets used to add columns to the merged data)
tbls <- unlist(lapply(seq_along(nonconform), function(x) { #which tables have all of the column names of each "nonconform" table 
  surveyx <- names(nonconform)[x]
  x <- nonconform[[x]]
  surveyx <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyx)
  which(
    unlist(
      lapply(seq_along(dups), function(y) {
        surveyy <- names(dups)[y]
        y <- dups[[y]]
        surveyy <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyy)
        comparetbls <- unlist(
          all(names(y) %in% names(x))
        )
        return(c(comparetbls & surveyx==surveyy))
      }
      )
    )
  )[1]
}
))

meld <- sapply(seq_along(tbls), function(x) {
  if(!is.na(tbls[x])) {
    bind_rows(dups[[tbls[x]]], nonconform[[x]])
  }
})

dups[tbls[!is.na(tbls)]] <- meld[which(!sapply(meld,is.null))] #merged in tables to the main dataset that just needed a NA filled column
nonconform <- nonconform[is.na(tbls)] #now, this is tables that haven't been merged in with anything else

######OF DATASETS THAT DIDN'T MATCH, CAN THEY BE REASONABLY MERGED WITH EACH OTHER?
tbls <- lapply(seq_along(nonconform), function(x) { #which tables have all of the column names of each "nonconform" table 
  surveyx <- names(nonconform)[x]
  x <- nonconform[[x]]
  surveyx <- gsub('.*_([A-Za-z ]*) [0-9].*','\\1', surveyx)
  which(
    unlist(
      lapply(seq_along(nonconform), function(y) {
        surveyy <- names(nonconform)[y]
        y <- nonconform[[y]]
        surveyy <- gsub('.*_([A-Za-z ]*) [0-9].*','\\1', surveyy)
        comparetbls <- unlist(
          all(names(x) %in% names(y))
        )
        return(c(comparetbls & surveyx==surveyy))
      }
      )
    )
  )
}
)

for (x in seq_along(tbls)) {
  if(length(tbls[[x]]) > 1) {
    y <- tbls[[x]][which(tbls[[x]] != x)]
    combined <- bind_rows(nonconform[[x]], nonconform[[y]])
    if(y < x){
      nonconform[[y]] <- combined
      nonconform[[x]] <- NA
    } else {
      nonconform[[x]] <- combined
      nonconform[[y]] <- NA
      }
  } 
}

nonconform <- nonconform[!is.na(nonconform)] 
#these are now tables that have been merged with each other, as best as possible, based on the criterion that to merge in, all column names in one table
#must be present in another

#test <- names(dups) %>% 
#  str_detect('Level 1_Entrainment') %>%
#  keep(dups, .)

#lapply(test, names)[[2]]

####OF THE UNMATCHED DATASETS LEFT, FIND THE BEST MATCH TO MERGE THEM IN
tbls <- as.integer(unlist(
  lapply(seq_along(nonconform), function(x) { #which tables have all of the column names of each "nonconform" table 
  surveyx <- names(nonconform)[x]
  x <- nonconform[[x]]
  surveyx <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyx)
  #which(
    #unlist(
  best <- bind_rows(lapply(seq_along(dups), function(y) {
        surveyy <- names(dups)[y]
        y <- dups[[y]]
        surveyy <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyy)
        comparetbls <- unlist(
          length(which(names(x) %in% names(y)))
        )
        return(data.frame(comparetbls,surveyx==surveyy))
      }
      )
      )
  best <- best[best$surveyx....surveyy==T,]
  best <- rownames(best[order(best$comparetbls, decreasing=T),][1,])
    #)
  #)
}
)
))

meld <- sapply(seq_along(tbls), function(x) {
    bind_rows(dups[[tbls[x]]], nonconform[[x]])
  })

dups[tbls] <- meld #merged in tables to the main dataset that just needed a NA filled column
###Voila! "nonconform" is done...

#now, see which tables can be combined by all names within the names of another and same level/survey
tbls <- lapply(seq_along(dups), function(x) { #which tables have all of the column names of each "nonconform" table 
  surveyx <- names(dups)[x]
  x <- dups[[x]]
  surveyx <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyx)
  which(
    unlist(
      lapply(seq_along(dups), function(y) {
        surveyy <- names(dups)[y]
        y <- dups[[y]]
        surveyy <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyy)
        comparetbls <- unlist(
          all((names(x)) %in% names(y))
        )
        return(c(comparetbls & surveyx==surveyy))
      }
      )
    )
  ) #[1]
}
)

for (x in seq_along(tbls)) {
  if(length(tbls[[x]]) > 1) {
    z <- tbls[[x]]
    y <- z[which(z != x)]
    if(all(!is.na(dups[z]))) {
      combined <- bind_rows(dups[z])
      if (any(y < x)){
        dups[[min(y)]] <- combined
        dups[z[which(z > min(y))]] <- NA
      } else {
        dups[[x]] <- combined
        dups[y] <- NA
    }
  }
} 
}

dups <- dups[!is.na(dups)]

####OF THE UNMATCHED DATASETS LEFT, FIND THE BEST MATCH TO MERGE THEM IN
tbls <- as.integer(
  unlist(
  lapply(seq_along(dups), function(x) { #which tables have all of the column names of each "nonconform" table 
    surveyx <- names(dups)[x]
    x <- dups[[x]]
    surveyx <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyx)
    #which(
    #unlist(
    best <- bind_rows(lapply(seq_along(dups), function(y) {
      surveyy <- names(dups)[y]
      y <- dups[[y]]
      surveyy <- gsub('(Level [0-9]_[A-Za-z ]*) [0-9].*','\\1', surveyy)
      comparetbls <- unlist(
        length(which(names(x) %in% names(y)))
      )
      return(data.frame(comparetbls,surveyx==surveyy))
    }
    ))
    best <- best[best$surveyx....surveyy==T & best$comparetbls > 2,]
    best <- best[order(best$comparetbls, decreasing=T),]
    if (nrow(best) > 1) {
      best <- rownames(best[2,])
    } else {best <- NA}
  }
  )
))

for (x in seq_along(tbls)) {
  if(!is.na(tbls[[x]]) & any(!is.na(dups[[x]]))) {
    y <- tbls[[x]][which(tbls[[x]] != x)]
    combined <- bind_rows(dups[[x]], dups[[y]])
    if(y < x){
      dups[[y]] <- combined
      dups[[x]] <- NA
    } else {
      dups[[x]] <- combined
      dups[[y]] <- NA
    }
  } 
}

dups <- dups[!is.na(dups)]
