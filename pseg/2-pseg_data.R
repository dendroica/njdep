library(purrr)
library(dplyr)
library(tidyverse)
root <- Sys.getenv("VPATH")
#this is generated from function in accessdb.R
path <- file.path(root, "pseg/pseg_data.RData")
load(path)
tbls_list <- flatten(mydbs)
# tbls_list <- sort(tbls_list)
data_tbls <- lapply(seq_along(tbls_list), function(x) {
  y <- tbls_list[[x]][order(colnames(tbls_list[[x]]))]
  y$filename <- names(tbls_list)[x]
  return(y)
})

names(data_tbls) <- names(tbls_list)

data_tbls <- data_tbls[order(names(data_tbls))]

# samplesint <- lapply(data_tbls, function(x){
#  print(is.integer(x$SAMPLNO))
# })

data_tbls <- lapply(data_tbls, function(x) {
  if (any(names(x) == "GRID")) {
    x$GRID <- as.integer(x$GRID)
  }

  if (any(names(x) == "STRATA")) {
    x$STRATA <- as.integer(x$STRATA)
  }

  if (any(names(x) == "DUR")) { # is DUR correct with decimals e.g. data_tbls[[59]]? data_tbls[[33]] -1234
    x$DUR <- as.character(x$DUR)
  }

  if (any(names(x) == "SAMPLNO")) {
    x$SAMPLNO <- as.character(x$SAMPLNO) # Impingement data is all int
  }
  return(x)
})
data_tbls <- lapply(data_tbls, function(x) {
  colnames(x) <- tolower(colnames(x))
  return(x)})
#checking to see if consecutive tables have the same headers
same_names <- c(FALSE, sapply(seq(2:length(data_tbls)) + 1, function(x) all(names(data_tbls[[x]]) == names(data_tbls[[x - 1]]))))
same_data_chunked <- split(which(same_names), cumsum(c(1, diff(which(same_names)) != 1)))
same_data_chunks <- lapply(same_data_chunked, function(x) c(x[1] - 1, x))
can_be_merged_chunks <- lapply(same_data_chunks, function(x) data_tbls[x])
database_names <- unlist(lapply(can_be_merged_chunks, function(x) names(x)[1]))
merged_chunks <- purrr::map(can_be_merged_chunks, bind_rows)
names(merged_chunks) <- database_names
# dups <- lapply(dups, function(x) do.call(rbind, x))
flattened <- as.integer(unlist(same_data_chunks))
nonconforming <- setdiff(min(flattened):max(flattened), flattened) #where the breaks happen
unmatched <- data_tbls[nonconforming]

###### CHECK ALONG THE MERGED CHUNKS FOR FURTHER MERGING OF TABLES WITH THE SAME HEADERS
##i.e. now check to see if there are tables with the same headers, but they were not consecutive
tbls <- lapply(seq_along(merged_chunks), function(x) { # which tables have all of the column names of each "nonconform" table
  db_name <- names(merged_chunks)[x]
  merged_chunk <- merged_chunks[[x]]
  db_name <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", db_name)
  which( #return which data tables are the same level and gear, that also have the same exact column names
    unlist(
      lapply(seq_along(merged_chunks), function(y) {
        db_name_match <- names(merged_chunks)[y]
        y <- merged_chunks[[y]]
        surveyy <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", db_name_match)
        comparetbls <- unlist(
          all((names(merged_chunk)) == names(y))
        )
        return(c(comparetbls & db_name == surveyy))
      })
    )
  ) # [1]
})
# )

for (x in seq_along(tbls)) {
  if (length(tbls[[x]]) > 1) { # are there dfs that can be merged?
    to_merge <- tbls[[x]]
    if (x == to_merge[1]) { # are we on the 1st df in that list that others can be merged to?
      combined <- bind_rows(merged_chunks[to_merge]) # if so, merge them
      merged_chunks[[x]] <- combined # reassign merged chunk to the index we're on
      merged_chunks[to_merge[2:length(to_merge)]] <- NA # set the chunks that followed that have been merged in to NA
    }
  }
}

merged_chunks <- merged_chunks[!is.na(merged_chunks)]

########## OF THE SETS THAT MATCHED NOTHING SERIALLY (i.e. the gaps)...now check if they match anything in the merged chunks non-serially
tbls <- lapply(seq_along(unmatched), function(x) {
  surveyx <- names(unmatched)[x]
  x <- unmatched[[x]]
  surveyx <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyx)
  which(
    unlist(
      lapply(seq_along(merged_chunks), function(y) {
        surveyy <- names(merged_chunks)[y]
        y <- merged_chunks[[y]]
        surveyy <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyy)
        comparetbls <- unlist(
          all(names(x) == names(y))
        )
        return(c(comparetbls & surveyx == surveyy))
      })
    )
  ) # [1]
})
# )

meld <- sapply(seq_along(tbls), function(x) {
  if (length(tbls[[x]]) > 0) {
    primary <- merged_chunks[[tbls[[x]]]]
    names(primary) <- tolower(names(primary))
    secondary <- unmatched[[x]]
    names(secondary) <- tolower(names(secondary))
    bind_rows(primary, secondary)
  }
})

merged_chunks[unlist(tbls)] <- meld[which(!sapply(meld, is.null))] # merged in tables to the main dataset that just needed a NA filled column
nonconform <- unmatched[is.na(tbls > 0)] # now, this is tables that haven't been merged in with anything else

######### OF THE SETS LEFT THAT DIDN'T MATCH SERIALLY, CAN WE COMBINE THEM BY NA FILL?
tbls <- unlist(lapply(seq_along(nonconform), function(x) { # which tables have all of the column names of each "nonconform" table
  surveyx <- names(nonconform)[x]
  x <- nonconform[[x]]
  surveyx <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyx)
  which(
    unlist(
      lapply(seq_along(merged_chunks), function(y) {
        surveyy <- names(merged_chunks)[y]
        y <- merged_chunks[[y]]
        surveyy <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyy)
        comparetbls <- unlist(
          all(names(x) %in% names(y))
        )
        return(c(comparetbls & surveyx == surveyy))
      })
    )
  )[1]
}))

meld <- sapply(seq_along(tbls), function(x) {
  if (!is.na(tbls[x])) {
    bind_rows(merged_chunks[[tbls[x]]], nonconform[[x]])
  }
})

merged_chunks[tbls[!is.na(tbls)]] <- meld[which(!sapply(meld, is.null))] # merged in tables to the main dataset that just needed a NA filled column
nonconform <- nonconform[is.na(tbls)] # now, this is tables that haven't been merged in with anything else

##### NA FILL THE OTHER DIRECTION (i.e. unmatched datasets used to add columns to the merged data)
tbls <- unlist(lapply(seq_along(nonconform), function(x) { # which tables have all of the column names of each "nonconform" table
  surveyx <- names(nonconform)[x]
  x <- nonconform[[x]]
  surveyx <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyx)
  which(
    unlist(
      lapply(seq_along(merged_chunks), function(y) {
        surveyy <- names(merged_chunks)[y]
        y <- merged_chunks[[y]]
        surveyy <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyy)
        comparetbls <- unlist(
          all(names(y) %in% names(x))
        )
        return(c(comparetbls & surveyx == surveyy))
      })
    )
  )[1]
}))

meld <- sapply(seq_along(tbls), function(x) {
  if (!is.na(tbls[x])) {
    bind_rows(merged_chunks[[tbls[x]]], nonconform[[x]])
  }
})

merged_chunks[tbls[!is.na(tbls)]] <- meld[which(!sapply(meld, is.null))] # merged in tables to the main dataset that just needed a NA filled column
nonconform <- nonconform[is.na(tbls)] # now, this is tables that haven't been merged in with anything else

###### OF DATASETS THAT DIDN'T MATCH, CAN THEY BE REASONABLY MERGED WITH EACH OTHER?
tbls <- lapply(seq_along(nonconform), function(x) { # which tables have all of the column names of each "nonconform" table
  surveyx <- names(nonconform)[x]
  x <- nonconform[[x]]
  surveyx <- gsub(".*_([A-Za-z ]*) [0-9].*", "\\1", surveyx)
  which(
    unlist(
      lapply(seq_along(nonconform), function(y) {
        surveyy <- names(nonconform)[y]
        y <- nonconform[[y]]
        surveyy <- gsub(".*_([A-Za-z ]*) [0-9].*", "\\1", surveyy)
        comparetbls <- unlist(
          all(names(x) %in% names(y))
        )
        return(c(comparetbls & surveyx == surveyy))
      })
    )
  )
})

for (x in seq_along(tbls)) {
  if (length(tbls[[x]]) > 1) {
    y <- tbls[[x]][which(tbls[[x]] != x)]
    combined <- bind_rows(nonconform[[x]], nonconform[[y]])
    if (y < x) {
      nonconform[[y]] <- combined
      nonconform[[x]] <- NA
    } else {
      nonconform[[x]] <- combined
      nonconform[[y]] <- NA
    }
  }
}

nonconform <- nonconform[!is.na(nonconform)]
# these are now tables that have been merged with each other, as best as possible, based on the criterion that to merge in, all column names in one table
# must be present in another

# test <- names(dups) %>%
#  str_detect('Level 1_Entrainment') %>%
#  keep(dups, .)

# lapply(test, names)[[2]]

#### OF THE UNMATCHED DATASETS LEFT, FIND THE BEST MATCH TO MERGE THEM IN
tbls <- as.integer(unlist(
  lapply(seq_along(nonconform), function(x) { # which tables have all of the column names of each "nonconform" table
    surveyx <- names(nonconform)[x]
    x <- nonconform[[x]]
    surveyx <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyx)
    # which(
    # unlist(
    best <- bind_rows(lapply(seq_along(merged_chunks), function(y) {
      surveyy <- names(merged_chunks)[y]
      y <- merged_chunks[[y]]
      surveyy <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyy)
      comparetbls <- unlist(
        length(which(names(x) %in% names(y)))
      )
      return(data.frame(comparetbls, surveyx == surveyy))
    }))
    best <- best[best$surveyx....surveyy == T, ]
    best <- rownames(best[order(best$comparetbls, decreasing = T), ][1, ])
    # )
    # )
  })
))

meld <- sapply(seq_along(tbls), function(x) {
  bind_rows(merged_chunks[[tbls[x]]], nonconform[[x]])
})

merged_chunks[tbls] <- meld # merged in tables to the main dataset that just needed a NA filled column
### Voila! "nonconform" is done...

# now, see which tables can be combined by all names within the names of another and same level/survey
tbls <- lapply(seq_along(merged_chunks), function(x) { # which tables have all of the column names of each "nonconform" table
  surveyx <- names(merged_chunks)[x]
  x <- merged_chunks[[x]]
  surveyx <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyx)
  which(
    unlist(
      lapply(seq_along(merged_chunks), function(y) {
        surveyy <- names(merged_chunks)[y]
        y <- merged_chunks[[y]]
        surveyy <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyy)
        comparetbls <- unlist(
          all((names(x)) %in% names(y))
        )
        return(c(comparetbls & surveyx == surveyy))
      })
    )
  ) # [1]
})

for (x in seq_along(tbls)) {
  if (length(tbls[[x]]) > 1) {
    z <- tbls[[x]]
    y <- z[which(z != x)]
    if (all(!is.na(merged_chunks[z]))) {
      combined <- bind_rows(merged_chunks[z])
      if (any(y < x)) {
        merged_chunks[[min(y)]] <- combined
        merged_chunks[z[which(z > min(y))]] <- NA
      } else {
        merged_chunks[[x]] <- combined
        merged_chunks[y] <- NA
      }
    }
  }
}

dups <- merged_chunks[!is.na(merged_chunks)]

#### OF THE UNMATCHED DATASETS LEFT, FIND THE BEST MATCH TO MERGE THEM IN
tbls <- as.integer(
  unlist(
    lapply(seq_along(dups), function(x) { # which tables have all of the column names of each "nonconform" table
      surveyx <- names(dups)[x]
      x <- dups[[x]]
      surveyx <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyx)
      # which(
      # unlist(
      best <- bind_rows(lapply(seq_along(dups), function(y) {
        surveyy <- names(dups)[y]
        y <- dups[[y]]
        surveyy <- gsub("(Level [0-9]_[A-Za-z ]*) [0-9].*", "\\1", surveyy)
        comparetbls <- unlist(
          length(which(names(x) %in% names(y)))
        )
        return(data.frame(comparetbls, surveyx == surveyy))
      }))
      best <- best[best$surveyx....surveyy == T & best$comparetbls > 2, ]
      best <- best[order(best$comparetbls, decreasing = T), ]
      if (nrow(best) > 1) {
        best <- rownames(best[2, ])
      } else {
        best <- NA
      }
    })
  )
)

for (x in seq_along(tbls)) {
  if (!is.na(tbls[[x]]) & any(!is.na(dups[[x]]))) {
    y <- tbls[[x]][which(tbls[[x]] != x)]
    combined <- bind_rows(dups[[x]], dups[[y]])
    if (y < x) {
      dups[[y]] <- combined
      dups[[x]] <- NA
    } else {
      dups[[x]] <- combined
      dups[[y]] <- NA
    }
  }
}

dups <- dups[!is.na(dups)]
