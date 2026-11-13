library(ggplot2, quietly = TRUE, verbose=FALSE)
library(car)
library(MuMIn)
load(file.path(Sys.getenv("FILEPATH"),"data/weakrope/weakrope_data3.RData"))
haul$id <- paste(haul$Name, haul$stringid, sep="_")
string$id <- paste(string$Name, string$stringid, sep="_")
merged <- merge(haul, string, by="id")
merged <- merged[!is.na(merged$`Target Species`),]
merged$target <- "bluefish"
merged$target[merged$`Target Species`=="butterfish"] <- "butterfish"
merged$target[merged$`Target Species`=="croaker"] <- "croaker"
merged$target[merged$`Target Species` %in% c("dogfish",
                                             "small dogfish",
                                             "smooth dogfish",
                                             "smooth dogfish, bonito", 
                                             
                                             #"smooth dogfish, bluefish",
                                             #"skate, bluefish, smooth dogfish",
                                             "spiny dogfish")] <- "dogfish"
merged$target[merged$`Target Species` %in% c("menhaden")] <- "menhaden"
merged$target[merged$`Target Species` %in% c("monkfish", "bluefish, monkfish", "skate, monkfish")] <- "monkfish"
merged$target[merged$`Target Species` %in% c("shark", "spinner shark")] <- "shark"
merged$target[merged$`Target Species` %in% c("skate", "skt", "skw", "winter skate",
                                             "smooth dogfish, skate", "menhaden, skate")] <- "skate"
merged$target[merged$`Target Species` %in% c("spanish mackerel")] <- "spanish mackerel"
merged$target[merged$`Target Species` %in% c("spot")] <- "spot"
merged$Treatment[is.na(merged$Treatment)] <- "control"

testdata <- merged[,names(merged)[!names(merged) %in% c("VTR#", "Expected Soak Time", 
                                                        "Estimated Soak Duration", "VTR Data",
                                                        "nontarget", "Set", "Haul",
                                                        "Protected Species Interaction",
                                                        "panel", "buoy_buoy", "Headrope Buoyancy (lb)",
                                                        "footrope_buoy",
                                                        "hours", "minutes",
                                                        "Notes/design")]]

#lapply(split(df$Col_to_see, df$Col_group), unique)

#foot rope diameter was significant in all models, and was aliased with a number
#of different variables in the data set describing the net, which included tie
#down length, twine size, anchor weight, manufacturer of various net components,
#as well as experimental parameter of the type of weak link used

predictors <- names(testdata)[!names(testdata) %in% c("catch", "net", "Treatment",
                                                      "target", "Target Species")]
char_cols <- sapply(testdata, is.character)
char_cols[which(names(char_cols)=="Target Species")] <- FALSE
testdata[char_cols] <- lapply(testdata[char_cols], as.factor)

model_all <- lm(catch ~ #Name + 
                  #stringid + 
                  #Vessel +
                  wind_speed + 
                  #wind_direction +
                  `Wave Length (ft)` + 
                  #sst +
                  current +
                  Substrate +
                  `Set Depth (fa)` + lat + lon + `Wind Speed (knots)` +
                  #`Wind Direction...25` +
                  #`Wave Height (ft)` +
                  `Sea Surface (f)...27` + `Current (Knots)...28` + max_swell +
                  estimated_soak +
                  #`# Net Panels` +
                  `Net Length (ft)` +
                  #`Net Height (ft)` +
                  `Mesh Count (vertical)` +
                  #`Stretched Mesh Size (in)` +
                  `Leadline (Spool) Weight (lbs)` +
                  #`Net Color` +
                  `# Floats` + 
                  `# Weak Links` +
                  #`Buoy line Diameter (in)` +
                  # `Buoy line Length (ft)` + #`Headrope Length (ft)` +
                  #`Footrope MFG` +
                  `Footrope Diameter (in)` +
                  #`# Tie Downs` +
                  #`Tie Down Length (in)` +
                  #`Twine Size` +
                  #`Footrope Length (ft)` +
                  #`Anchor Weight (lbs)` + #`Buoy line MFG` +
                  #`Weak Link Type (if any)` +
                  `Headrope Diameter (in)`, #+ `Headrope MFG`,
                data=testdata)

#you have to remove these to de-alias the model:
#ld.vars <- attributes(alias(model_all)$Complete)$dimnames[[1]]
vif(model_all) #use this to get rid of collinear variables
#then when they're out, use what's left to extract the col names (below)
#names(vif(model_all)[,1])

clean_data <- testdata[,
                       which(names(testdata) %in% c("id", gsub("`","", 
                                                               names(vif(model_all)[,1])),
                                                    "net", "Treatment",
                                                    "Target Species", "target",
                                                    "catch", "Weak Links Total"))]
clean_data <- na.omit(clean_data)
clean_data$target <- gsub("\\b([a-z])", "\\U\\1", clean_data$target, perl = TRUE)
best_model_trt <- aov(catch ~ `# Floats` + `Footrope Diameter (in)` +
                        `Mesh Count (vertical)` + `Net Length (ft)` + `Sea Surface (f)...27` +
                        `Set Depth (fa)` + current + estimated_soak + lat + max_swell + wind_speed +
                        target*Treatment, data = clean_data, na.action=na.fail)
comp_means <- emmeans(best_model_trt, ~ target * Treatment)
pairwise_results <- pairs(comp_means, by="target")
summary(pairwise_results)
pairs_df <- as.data.frame(pairwise_results)
pairs_df <- pairs_df[pairs_df$p.value < 0.05 & !is.na(pairs_df$p.value),]
