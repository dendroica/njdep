library(purrr)
library(dplyr)
library(tidyverse)
load("~/data/pseg_data.RData")
flattendb <- flatten(dbs)

lvl2bottom <- flattendb[[140]]
lvl3bottom <- flattendb[[141]]

problem <- lvl2bottom[lvl2bottom$sample_id %in% c("000038335",
                                            "000038345",
                                            "000038299",
                                            "000038319",
                                            "000038396",
                                            "000038352",
                                            "000038194",
                                            "000038202",
                                            "000038389",
                                            "000038302",
                                            "000038200",
                                            "000038187",
                                            "000038185", "000038304") & lvl2bottom$common_name %in% "STRIPED BASS",]

problem2 <- lvl3bottom[lvl3bottom$sample_id %in% c("000038335",
                                                  "000038345",
                                                  "000038299",
                                                  "000038319",
                                                  "000038396",
                                                  "000038352",
                                                  "000038194",
                                                  "000038202",
                                                  "000038389",
                                                  "000038302",
                                                  "000038200",
                                                  "000038187",
                                                  "000038185", "000038304") & lvl3bottom$common_name %in% "STRIPED BASS",]
comparelvls <- merge(problem, problem2, all.y=T, by="sample_id")

#test2 <- x[x$sample_id=="000038304",]
#test2 <- test2[test2$common_name=="STRIPED BASS",]
#head(test2[order(test2$ind_fish_id),])
