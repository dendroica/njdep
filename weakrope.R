library(ggplot2, quietly = TRUE, verbose=FALSE)
library(readxl, quietly = TRUE, verbose=FALSE)
library(stringr)
library(leaps)
library(car)
library(MuMIn)
library(emmeans)
library(interactions)
#in -JMG2 file version:
#I fixed a typo in the name of the sheet to correct it to "Protected species interactions"

#what's left to do...
#1. fix Haul where you can from estimated_soak

haul <- read_xlsx(path=file.path(Sys.getenv("FILEPATH"), "data/Weak Rope Survey-JMG4.xlsx"), sheet="Hauling Data")
haul[,c(5:6, 19:20, 22)] <- NULL
names(haul)[4] <- "stringid"
names(haul)[names(haul)=="Lattitude (DD.dddd)"] <- "lat"
names(haul)[names(haul)=="Longitude (DD.dddd)"] <- "lon"
names(haul)[names(haul)=="Sea Surface (f)...12"] <- "sst"
names(haul)[names(haul)=="Panal damage/loss"] <- "panel"
names(haul)[names(haul)=="Wind Direction...10"] <- "wind_direction"
names(haul)[names(haul)=="Current (Knots)...13"] <- "current"
names(haul)[names(haul)=="Wind Speed (Knots)"] <- "wind_speed"
names(haul)[names(haul)=="Max Swell Durin Soak (ft)"] <- "max_swell"
names(haul)[names(haul)=="Non-Target Species (bycatch) Caught (list species)"] <- "nontarget"
names(haul)[names(haul)=="Est. Target Catch (lbs)"] <- "catch"
#haul$`Wind Speed (knots)` <- NULL name for set/haul
#haul$`Wind Direction...25` <- NULL
#haul$`Sea Surface (f)...27` <- NULL
#haul$`Current (Knots)...28` <- NULL

#stand-in fixes...
haul$max_swell[haul$max_swell=="s"] <- NA
haul$`Wind Speed (knots)`[grep("w$", haul$`Wind Speed (knots)`)] <- NA
haul$`Estimated Soak Duration`[grep("E", haul$`Estimated Soak Duration`)] <- "1 hr 38 mins"
########
haul$stringid <- tolower(haul$stringid)
haul$stringid[haul$stringid=="monk cont" & !is.na(haul$stringid)] <- "monk control"
haul$stringid[haul$stringid=="blue control" & !is.na(haul$stringid)] <- "4 1/4 blue control"
haul$stringid[haul$stringid=="blue exp" & !is.na(haul$stringid)] <- "4 1/4 blue exp"
haul$stringid[haul$stringid=="3 monk" & !is.na(haul$stringid)] <- "monk 3"
haul$stringid[haul$stringid=="5 monk" & !is.na(haul$stringid)] <- "monk 5"
haul$stringid[haul$stringid=="4 monk" & !is.na(haul$stringid)] <- "monk 4"
haul$stringid[haul$stringid=="6 monk" & !is.na(haul$stringid)] <- "monk 6"
haul$Vessel <- tolower(haul$Vessel)
haul$Haul <- as.POSIXct(haul$Haul, format="%m/%d/%Y %I:%M %p")
haul$Set <- as.POSIXct(haul$Set, format="%m/%d/%Y %I:%M %p")
haul$`Expected Soak Time`[grep("[0-9]$",
                               haul$`Expected Soak Time`)] <- paste(haul$`Expected Soak Time`[grep("[0-9]$", haul$`Expected Soak Time`)], "hr")
haul$`Estimated Soak Duration`[grep("[0-9]$",
                               haul$`Estimated Soak Duration`)] <- paste(haul$`Estimated Soak Duration`[grep("[0-9]$",
                                                                                                             haul$`Estimated Soak Duration`)], "hr")
haul$`Estimated Soak Duration` <- gsub("hrs", "hr", haul$`Estimated Soak Duration`)
haul$`Estimated Soak Duration` <- gsub("mins", "min", haul$`Estimated Soak Duration`)
haul$`Estimated Soak Duration` <- gsub("minns", "min", haul$`Estimated Soak Duration`)
haul$hours   <- as.numeric(str_extract(haul$`Estimated Soak Duration`, "\\d+(?=\\s*hr)"))
haul$hours[is.na(haul$hours)] <- 0
haul$minutes <- as.numeric(str_extract(haul$`Estimated Soak Duration`, "\\d+(?=\\s*min)"))
haul$minutes[is.na(haul$minutes)] <- 0
haul$estimated_soak <- haul$hours + haul$minutes / 60
soak <- haul[is.na(haul$`Estimated Soak Duration`),"Haul"] - haul[is.na(haul$`Estimated Soak Duration`),"Set"]
haul$estimated_soak[is.na(haul$`Estimated Soak Duration`)] <-  as.numeric(soak$Haul, units = "hours")

#haul$`Expected Soak Time` still need to change value "1-2 hrs"
#wind_speed choose which side of range (min or max) to keep, change to 2 for max
haul$wind_speed <- as.numeric(unname(unlist(lapply(sapply(haul$wind_speed, function(x) str_split(x, "-")), "[[", 1))))
haul$`Wind Speed (knots)` <- as.numeric(unname(unlist(lapply(sapply(haul$`Wind Speed (knots)`, function(x) str_split(x, "-")), "[[", 1))))
haul$wind_direction <- toupper(haul$wind_direction)
haul$wind_direction <- gsub(",", "", haul$wind_direction)
haul$wind_direction[grep("[0-9]$", haul$wind_direction)] <- NA
haul$`Wave Length (ft)` <- as.numeric(unname(unlist(lapply(sapply(haul$`Wave Length (ft)`, function(x) str_split(x, "-")), "[[", 1))))
haul$current <- as.numeric(haul$current)
haul$Substrate <- toupper(haul$Substrate)
haul$`Target Species` <- tolower(haul$`Target Species`)
haul$`Target Species` <- gsub("dog fish", "dogfish", haul$`Target Species`)
haul$`Target Species` <- gsub(", blue", ", bluefish", haul$`Target Species`)
haul$`Target Species` <- gsub("blue,", "bluefish,", haul$`Target Species`)
haul$`Target Species` <- gsub("blues", ", bluefish,", haul$`Target Species`)
haul$`Target Species` <- gsub("skate ,", "skate,", haul$`Target Species`)
haul$`Target Species` <- gsub(", smooth", ", smooth dogfish", haul$`Target Species`)
haul$`Target Species` <- gsub("monk fish", "monkfish", haul$`Target Species`)
haul$`Target Species` <- gsub("/monk", ", monkfish", haul$`Target Species`)
haul$`Target Species` <- gsub("bluefish, monk", "bluefish, monkfish", haul$`Target Species`)
haul$`Target Species` <- gsub("mackeral", "mackerel", haul$`Target Species`)
haul$catch <- as.numeric(haul$catch)
haul$lat <- as.numeric(haul$lat)
haul$lon <- as.numeric(haul$lon)
haul$panel <- tolower(haul$panel)
haul$`Protected Species Interaction` <- tolower(haul$`Protected Species Interaction`)
haul$max_swell <- as.numeric(unname(unlist(lapply(sapply(haul$max_swell, function(x) str_split(x, "-")), "[[", 1))))
haul$nontarget <- tolower(haul$nontarget)
haul$nontarget <- gsub("n/a", NA, haul$nontarget)
haul$nontarget <- gsub("none", NA, haul$nontarget)
haul$nontarget <- gsub("mackeral", "mackerel", haul$nontarget)
haul$nontarget <- gsub("skatre", "skate", haul$nontarget)
haul$nontarget <- gsub("sog shark", "dogfish", haul$nontarget)
haul$nontarget <- gsub("threasher", "thresher", haul$nontarget)
haul$nontarget <- gsub("skaye", "skate", haul$nontarget)
haul$nontarget <- gsub("barndeer", "barndoor", haul$nontarget)
haul$nontarget <- gsub("horseshore", "horseshoe", haul$nontarget)
haul$nontarget <- gsub("sturheon", "sturgeon", haul$nontarget)
haul$nontarget <- gsub("bonita", "bonito", haul$nontarget)
haul$nontarget <- gsub("hickery", "hickory", haul$nontarget)
haul$nontarget <- gsub("stripped", "striped", haul$nontarget)
haul$nontarget <- gsub("smooth hound", "smooth-hound", haul$nontarget)
haul$nontarget <- gsub("spidercrab", "spider crab", haul$nontarget)
haul$nontarget <- gsub("striper", "striped bass", haul$nontarget)
haul$nontarget <- gsub("sun dial", "windowpane flounder", haul$nontarget)
haul$nontarget <- gsub("hsc", "horseshoe crab", haul$nontarget)
haul$nontarget <- gsub("bunker", "menhaden", haul$nontarget)
haul$nontarget <- gsub("monktail", "monkfish", haul$nontarget)
haul$nontarget <- gsub("hounds", "hound", haul$nontarget)
haul$nontarget <- gsub("dogs", "dogfish", haul$nontarget)
haul$nontarget <- gsub("robins", "robin", haul$nontarget)
haul$nontarget <- gsub("croakers", "croaker", haul$nontarget)
haul$nontarget <- gsub("crabs", "crab", haul$nontarget)
haul$nontarget <- gsub("blues", "bluefish", haul$nontarget)
haul$nontarget <- gsub("skates", "skate", haul$nontarget)
haul$nontarget <- gsub("butter fish", "butterfish", haul$nontarget)
haul$nontarget <- gsub("window pane", "windowpane", haul$nontarget)
haul$nontarget <- gsub("winterskate", "winter skate", haul$nontarget)
haul$nontarget <- gsub("menhaden ", "menhaden, ", haul$nontarget)
haul$nontarget <- gsub("loon ", "loon, ", haul$nontarget)
haul$nontarget <- gsub("2 ", "", haul$nontarget)
haul$nontarget <- gsub("small sturgeon", "sturgeon", haul$nontarget)
haul$nontarget <- gsub("small thresher", "thresher", haul$nontarget)
haul$nontarget <- gsub("small fluke", "fluke", haul$nontarget)
haul$nontarget <- gsub("spanish mackerel ", "spanish mackerel, ", haul$nontarget)
haul$nontarget <- gsub("bait ", "bait, ", haul$nontarget)
haul$nontarget <- gsub("fluke ", "fluke, ", haul$nontarget)
haul$nontarget <- gsub("crab ", "crab, ", haul$nontarget)
haul$nontarget <- gsub("skate ", "skate, ", haul$nontarget)
haul$nontarget <- gsub("croaker ", "croaker, ", haul$nontarget)
haul$nontarget <- gsub("shark ", "shark, ", haul$nontarget)
haul$nontarget <- gsub("robin ", "robin, ", haul$nontarget)
haul$nontarget <- gsub("dog fish", "dogfish", haul$nontarget)
haul$nontarget <- gsub("blue fish", "bluefish", haul$nontarget)
haul$nontarget <- gsub("blue,", "bluefish,", haul$nontarget)
haul$nontarget <- gsub("dog ", "dogfish, ", haul$nontarget)
haul$nontarget <- gsub("dogfish ", "dogfish, ", haul$nontarget)
haul$nontarget <- gsub("starfish ", "starfish, ", haul$nontarget)
haul$nontarget <- gsub("butterfish ", "butterfish, ", haul$nontarget)
haul$nontarget <- gsub("bluefish ", "bluefish, ", haul$nontarget)
haul$nontarget <- gsub("striped bass ", "striped bass, ", haul$nontarget)
haul$nontarget <- gsub("spiny dog,", "spiny dogfish,", haul$nontarget)
haul$nontarget <- gsub("smooth dog,", "smooth dogfish,", haul$nontarget)
haul$nontarget <- gsub("monk,", "monkfish,", haul$nontarget)
haul$nontarget <- gsub("h,b", "h, b", haul$nontarget)
haul$nontarget <- gsub("thresher,", "thresher shark,", haul$nontarget)
haul$nontarget <- gsub("sand tiger,", "sand tiger shark,", haul$nontarget)
haul$nontarget <- gsub(",  ", ", ", haul$nontarget)
haul$nontarget <- gsub(", muddy nets", "", haul$nontarget)
haul$nontarget <- gsub("black tip", "blacktip", haul$nontarget)
haul$nontarget <- gsub("spanish ,", "spanish mackerel,", haul$nontarget)
haul$nontarget <- gsub("thresher sand", "thresher shark, sand", haul$nontarget)
haul$nontarget <- gsub("thresher little", "thresher shark, little", haul$nontarget)
haul$nontarget <- gsub("sandbar cownose", "sandbar shark, cownose", haul$nontarget)

haul$nontarget <- gsub("cleanose", "clearnose", haul$nontarget)
haul$nontarget <- gsub("clearnose,", "clearnose skate,", haul$nontarget)
haul$nontarget <- gsub("clearnose spider crab", "clearnose skate, spider crab", haul$nontarget)
haul$nontarget <- gsub("clearnose sea", "clearnose skate, sea", haul$nontarget)
haul$nontarget <- gsub("sandbar clearnose", "sandbar shark, clearnose", haul$nontarget)
haul$nontarget <- gsub("spinner clearnose", "spinner shark, clearnose", haul$nontarget)
haul$nontarget <- gsub("clear nose", "clearnose", haul$nontarget)
haul$nontarget <- gsub("clearnose$", "clearnose skate", haul$nontarget)

haul$nontarget <- gsub("monk fish", "monkfish", haul$nontarget)
haul$nontarget <- gsub("monkfish ", "monkfish, ", haul$nontarget)
haul$nontarget <- gsub("monk ", "monkfish, ", haul$nontarget)
haul$nontarget <- gsub("monk. ", "monkfish, ", haul$nontarget)

haul$nontarget <- gsub("dog$", "dogfish", haul$nontarget)
haul$nontarget <- gsub("tiger$", "tiger shark", haul$nontarget)
haul$nontarget <- gsub("monk$", "monkfish", haul$nontarget)
haul$nontarget <- gsub("windowpane$", "windowpane flounder", haul$nontarget)
haul$nontarget <- gsub("sandbar$", "sandbar shark", haul$nontarget)
haul$nontarget <- gsub("spinner$", "spinner shark", haul$nontarget)
haul$nontarget <- gsub("thresher$", "thresher shark", haul$nontarget)
haul$nontarget <- gsub("butter$", "butterfish", haul$nontarget)
haul$nontarget <- gsub("mac$", "mackerel", haul$nontarget)
spp <- sapply(haul$nontarget, function(x) str_split(x, ", "))
spp <- lapply(spp, sort)
spp_char <- unname(unlist(lapply(spp, function(x) paste(x, collapse=", "))))
spp_char[which(spp_char=="")] <- NA
haul$nontarget <- spp_char

subset_df <- haul[grepl("weakfish", haul$nontarget), ]
haul$`Wind Direction...25` <- toupper(haul$`Wind Direction...25`)
haul$`Wind Direction...25` <- gsub(",", "", haul$`Wind Direction...25`)
haul$`Wind Direction...25` <- gsub(" ", "", haul$`Wind Direction...25`)
haul$`Wind Direction...25`[grep("[0-9]$", haul$`Wind Direction...25`)] <- NA
haul$`Wind Direction...25`[grep("A$", haul$`Wind Direction...25`)] <- NA

haul$`Wave Height (ft)` <- as.numeric(unname(unlist(lapply(sapply(haul$`Wave Height (ft)`, function(x) str_split(x, "-")), "[[", 1))))
haul$`Current (Knots)...28` <- as.numeric(haul$`Current (Knots)...28`)

string <- read_xlsx(path=file.path(Sys.getenv("FILEPATH"), "data/Weak Rope Survey-JMG4.xlsx"), sheet="String ID")
names(string)[names(string)=="String ID"] <- "stringid"
names(string)[names(string)=="Control or experimental"] <- "net"
#names(string)[names(string)=="Footrope MFG (manfacture)"] <- "footrope_mfg"
#names(string)[names(string)=="Footrope Bouyancy (lb)"] <- "footrope_buoy"
names(string)[names(string)=="Footrope Buoyancy  (lb)"] <- "footrope_buoy"
#names(string)[names(string)=="Headrope MFG (manafacture)"] <- "headrope_mfg"
#names(string)[names(string)=="Headrope Bouyancy (lb)"] <- "headrope_buoy"
#names(string)[names(string)=="Bouyline Length (ft)"] <- "buoy_l"
#names(string)[names(string)=="Bouyline Diameter (in)"] <- "buoy_d"
#names(string)[names(string)=="Bouyline MFG (manfacture)"] <- "buoy_mfg"
#names(string)[names(string)=="Bouyline Bouyancy (lb)"] <- "buoy_buoy"
names(string)[names(string)=="Buoy line Buoyancy  (lb)"] <- "buoy_buoy"

string$stringid <- tolower(string$stringid)
string$net <- tolower(string$net)
string[which(string$Treatment=="Weak End Line"),]$Treatment <- "Weak endline"
string[which(string$Name=="Tim Kriessmenn"),]$Name <- "Tim Kriegsmann"
string[which(string$Treatment=="N/A"),]$Treatment <- NA 
string$`Net Color` <- tolower(string$`Net Color`)
string[string$`Net Color`=="lt green",]$`Net Color` <- "light green"
string[string$`Net Color` %in% c("pink/grey", "pink/gray"),]$`Net Color` <- "pink grey"
string[string$`Anchor Weight (lbs)` %in% c("n/a", "na"),]$`Anchor Weight (lbs)` <- NA
string$`Anchor Weight (lbs)` <- as.integer(string$`Anchor Weight (lbs)`)
string[string$`# Tie Downs` %in% c("N/A", "N/a"),]$`# Tie Downs` <- NA
string[string$`Tie Down Length (in)` %in% c("N/A", "N/a", "n/a", "na"),]$`Tie Down Length (in)` <- NA
string$`Tie Down Length (in)` <- as.integer(string$`Tie Down Length (in)`)
string$`Footrope Diameter (in)` <- unname(sapply(string$`Footrope Diameter (in)`, function(x) eval(parse(text=x))))
#string$footrope_mfg <- toupper(string$footrope_mfg)
string$`Footrope MFG` <- toupper(string$`Footrope MFG`)
string$`Headrope Diameter (in)` <- unname(sapply(string$`Headrope Diameter (in)`, function(x) eval(parse(text=x))))
string$`Buoy line Diameter (in)` <- unname(sapply(string$`Headrope Diameter (in)`, function(x) eval(parse(text=x))))
string$`Twine Size` <- as.numeric(string$`Twine Size`)

protected <- read_xlsx(path=file.path(Sys.getenv("FILEPATH"), "data/Weak Rope Survey-JMG4.xlsx"), sheet="Protected species interactions")
names(protected)[names(protected)=="string id"] <- "stringid"
names(protected)[names(protected)=="net type"] <- "net"
names(protected)[names(protected)=="Haul date"] <- "Haul"
names(protected)[names(protected)=="where there any visible injuries or indentations from gear"] <- "injury"
names(protected)[names(protected)=="for sea turtle, was resuscitation requried?"] <- "sea_turtle_resuscitate"
names(protected)[names(protected)=="was the animal alive"] <- "fate"
names(protected)[names(protected)=="picture taken"] <- "pic"
names(protected)[names(protected)=="how did you interact with the animal"] <- "interaction"
names(protected)[names(protected)=="where did the entanglement occur on the panel"] <- "where"
names(protected)[names(protected)=="if alive, what was the state when released"] <- "release"
protected$fate <- tolower(protected$fate)
protected$net <- tolower(protected$net)
protected$`on the animal?` <- tolower(protected$`on the animal?`)
protected[protected$`on the animal?`=="head,gilled",]$`on the animal?` <- "head (gilled)"
#how did you interact with the animal: needs cleanup
#where did the entanglement occur on the panel: various values that mean the same thing?
#on the animal? various values that mean the same thing?
#initial state of the animal needs cleanup
#if alive, what was the state when released needs cleanup
#injury needs cleanup

panel <- read_xlsx(path=file.path(Sys.getenv("FILEPATH"), "data/Weak Rope Survey-JMG4.xlsx"), sheet="Panel Damage & Loss Information")
names(panel)[names(panel)=="haul date"] <- "Haul"
names(panel)[names(panel)=="string id"] <- "stringid"
names(panel)[names(panel)=="were pannels lost"] <- "panel_loss"
names(panel)[names(panel)=="describe damange"] <- "damage"
panel[panel$Name=="Chuck solan",]$Name <- "Charles Solan"
panel[panel$Name=="f/v webb",]$Name <- "F/v webo"

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
merged$target[merged$`Target Species` %in% c("monkfish", "skate, monkfish")] <- "monkfish"
merged$target[merged$`Target Species` %in% c("shark", "spinner shark")] <- "shark"
merged$target[merged$`Target Species` %in% c("skate", "skt", "skw", "winter skate")] <- "skate"
merged$target[merged$`Target Species` %in% c("spanish mackerel")] <- "spanish mackerel"
merged$target[merged$`Target Species` %in% c("spot")] <- "spot"

#test <- merged[merged$`Target Species`== "menhaden",]
#aov(catch ~ estimated_soak + net, data = test)
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
#paste(predictors, collapse=" + ")

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

#you have to remov]e these to de-alias the model:
#ld.vars <- attributes(alias(model_all)$Complete)$dimnames[[1]]
vif(model_all) #use this to get rid of collinear variables
#then when they're out, use what's left to extract the col names (below)
#names(vif(model_all)[,1])

clean_data <- testdata[,which(names(testdata) %in% c("catch",
                                                     gsub("`",
                                                          "", 
                                                          names(vif(model_all)[,1])),
                                                     "target", "net"))]
clean_data <- na.omit(clean_data)



model1 <- lm(as.formula(paste0("catch ~ ",
                               paste(names(vif(model_all)[,1]), collapse=" + "),
                               " + target*net")),
             data = clean_data, na.action=na.fail)

num_cores <- parallel::detectCores() - 1
fuck <- parallel::makeCluster(num_cores)
parallel::clusterExport(fuck, "clean_data")
subset_results <- dredge(model1, cluster=fuck)
parallel::stopCluster(fuck)

best_model <- aov(catch ~ `Anchor Weight (lbs)` +
                    `Leadline (Spool) Weight (lbs)` + 
                    `Sea Surface (f)...27` + `Set Depth (fa)` +
                    `Wind Speed (knots)` + current + lat +
                    estimated_soak + `# Weak Links` + target*net,
                    #target*`# Weak Links`,
                 data = clean_data, na.action=na.fail)
summary(best_model)
emtrends(best_model, ~ target, var="# Weak Links")
#comp_means <- emmeans(best_model, ~ `Target Species` * net)
#pairwise_results <- pairs(comp_means, by = "Target Species", adjust = "net")
#summary(pairwise_results)

#ggplot(data = clean_data, aes(x = `# Weak Links`, y = catch, color=target, group=target)) +
#  geom_point() +
#  geom_smooth(method = "lm")
