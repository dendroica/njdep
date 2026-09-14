library(ggplot2, quietly = TRUE, verbose=FALSE)
library(leaps)
library(car)
library(MuMIn)
library(emmeans)
library(interactions)
load(file.path(Sys.getenv("FILEPATH"),"data/weakrope/weakrope_data.RData"))
merged <- merge(haul, string, by=c("Name","stringid"))
merged <- merged[!is.na(merged$`Target Species`),]
merged$target <- "bluefish"
merged$target[merged$`Target Species`=="butterfish"] <- "butterfish"
merged$target[merged$`Target Species`=="croaker"] <- "croaker"
merged$target[merged$`Target Species` %in% c("dogfish", "small dogfish",
                                             "smooth dogfish",
                                             "smooth dogfish, bonito", 
                                             "smooth dogfish, skate",
                                             "spiny dogfish")] <- "dogfish"
merged$target[merged$`Target Species` %in% c("menhaden", "menhaden, skate")] <- "menhaden"
merged$target[merged$`Target Species` %in% c("monkfish", "skate, monkfish", "bluefish, monkfish")] <- "monkfish"
merged$target[merged$`Target Species` %in% c("shark", "spinner shark")] <- "shark"
merged$target[merged$`Target Species` %in% c("skate", "skt", "skw", "winter skate")] <- "skate"
merged$target[merged$`Target Species` %in% c("spanish mackerel")] <- "spanish mackerel"
merged$target[merged$`Target Species` %in% c("spot")] <- "spot"
merged$Treatment[is.na(merged$Treatment)] <- "control"

testdata <- merged[,names(merged)[!names(merged) %in% c("VTR#", "Expected Soak Time", 
                                                        "Estimated Soak Duration", "VTR Data",
                                                        "nontarget", "Set", "Haul",
                                                        "Protected Species Interaction",
                                                        "panel", "buoy_buoy", "Headrope Buoyancy (lb)",
                                                        "footrope_buoy", "Target Species",
                                                        "hours", "minutes", "Notes/design")]]
predictors <- names(testdata)[!names(testdata) %in% c("catch", "net", "Treatment",
                                                      "target")]
char_cols <- sapply(testdata, is.character)
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

clean_data <- testdata[,which(names(testdata) %in% c("catch",
                                                     gsub("`",
                                                          "", 
                                                          names(vif(model_all)[,1])),
                                                     "target", "net", "Treatment"))]
clean_data <- na.omit(clean_data)
#save(clean_data, file="weakrope_data_analysis.RData")
model1 <- lm(as.formula(paste0("catch ~ ",
                               paste(names(vif(model_all)[,1]), collapse=" + "),
                               " + target*net")),
             data = clean_data, na.action=na.fail)
num_cores <- parallel::detectCores() - 1
fuck <- parallel::makeCluster(num_cores)
parallel::clusterExport(fuck, "clean_data")
subset_results <- dredge(model1, cluster=fuck) #, m.max=7
parallel::stopCluster(fuck)
#################
#save.image("big.RData")
#not needed here but if you tweak variables you might want to reconsider how much the data is pared down
#model_all <- lm(catch ~ `# Floats` + `# Weak Links` + `Footrope Diameter (in)` +
#  `Mesh Count (vertical)` + `Net Length (ft)` + `Sea Surface (f)...27` +
#  `Set Depth (fa)` + current + estimated_soak + lat + max_swell + wind_speed +
#  target*`# Weak Links`, data=testdata)

#clean_data <- testdata[,which(names(testdata) %in% c("catch",
#                                                     gsub("`",
#                                                          "", 
#                                                          names(vif(model_all)[,1])),
#                                                     "target", "net"))]
#clean_data <- na.omit(clean_data)
##################

#test <- merged[merged$`Target Species`== "menhaden",]
#aov(catch ~ estimated_soak + net, data = test)
#paste(predictors, collapse=" + ")

#oldbest_model <- aov(catch ~ `Anchor Weight (lbs)` +
#                    `Leadline (Spool) Weight (lbs)` + 
#                    `Sea Surface (f)...27` + `Set Depth (fa)` +
#                    `Wind Speed (knots)` + current + lat +
#                    estimated_soak + `# Weak Links` + target*net,
                    #target*`# Weak Links`,
#                 data = clean_data, na.action=na.fail)

#ggplot(data = clean_data, aes(x = `# Weak Links`, y = catch, color=target, group=target)) +
#  geom_point() +
#  geom_smooth(method = "lm")
