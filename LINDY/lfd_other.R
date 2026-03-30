#THIS DOES THE ADDITIONAL SUBSETS NO ONE SEEMS TO NEED
#if you need it, paste back into LFD_WGTED.R right under where FINALY
#is specified 

TEMPLATE <- read.dbf(file.path(mypath, "LTEMPLTE.dbf"))
TEMPLATE$YEAR <- as.numeric(as.character(TEMPLATE$YEAR))

SAMPLES <- ABUND %>%
  count(CRUCODE, STRATUM, AREA, name = "COUNT")

LENGA <-  bind_rows(TEMPLATE, LENG) %>%
  mutate(
    LENGA = as.character(LENGTH),
    FREQUENCY = FREQUENCY / MINOUT * 20
  )

LENG1 <- LENGA %>%
  group_by(CRUCODE, STRATUM, LENGA) %>%
  summarise(SUM = sum(FREQUENCY, na.rm = TRUE), .groups = 'drop')

SAMPLES$FACTOR <- unname(unlist(Map(FactorAssignment, SAMPLES$AREA, SAMPLES$STRATUM)))

ZEROS <- LENG1 %>% full_join(SAMPLES, by = c("CRUCODE", "STRATUM")) %>%
  filter(SUM > 0) %>% mutate(MEAN1 = FACTOR * (SUM / COUNT)) %>%
  group_by(CRUCODE, LENGA) %>%
  summarise(MEANS = sum(MEAN1, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = LENGA, values_from = MEANS) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

FINAL <- MEANS %>%
  full_join(ZEROS, by = "CRUCODE") %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
  filter(CRUCODE != 19011) %>%
  select(-CRUCODE, -CRUCODES)

STSAMPLESALL <- STABUNDALL %>% count(STRATUM, name="COUNT")

# Transpose the data
STTRANSY3ALL <- bind_rows(TEMPLATE, LENG) %>%
  mutate(STRATUM = 0, LENGA = as.character(LENGTH), FREQUENCY = FREQUENCY / MINOUT * 20) %>%
  group_by(STRATUM, LENGA) %>%
  summarise(SUM = sum(FREQUENCY, na.rm = TRUE), .groups = 'drop') %>%
  full_join(STSAMPLESALL, by = "STRATUM") %>%
  filter(SUM > 0) %>%
  mutate(FACTOR = 1, MEAN1 = FACTOR * (SUM / COUNT)) %>%
  group_by(STRATUM, LENGA) %>%
  summarise(MEANS = sum(MEAN1, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = LENGA, values_from = MEANS) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>% #matches!
  pivot_longer(cols = -STRATUM, names_to = "LENGTH", values_to = "MEANS") %>%
  pivot_wider(names_from = LENGTH, values_from = MEANS) #line 760

STFINALYALL <- STYMEANSALL %>%
  full_join(STTRANSY3ALL, by = "STRATUM") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

#BY STRATUM - NOT WEIGHTED BY STRATUM FACTORS*******************;
# Frequency table
STSAMPLES <- ABUND %>%
  group_by(STRATUM) %>%
  summarise(COUNT = n(), .groups = 'drop') %>%
  select(STRATUM, COUNT)

# Append data
STYTEMPLT <- TEMPLATE
# Sort and transpose again
STTRANSY3 <- bind_rows(STYTEMPLT, LENG) %>%
  mutate(LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
  group_by(STRATUM, LENGA) %>% summarise(SUM = sum(FREQUENCY),
                                         .groups = 'drop') %>% #MATCHES
  left_join(STSAMPLES, by = "STRATUM") %>% filter(SUM > 0) %>%
  mutate(FACTOR = 1, MEAN1 = FACTOR * (SUM / COUNT)) %>%
  group_by(STRATUM, LENGA) %>%
  summarise(MEANS = sum(MEAN1), .groups = 'drop') %>%
  pivot_wider(names_from = LENGA, values_from = MEANS) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  pivot_longer(-STRATUM, names_to = "LENGTH", values_to = "MEANS")  %>%
  pivot_wider(names_from = LENGTH, values_from = MEANS)

# Final merge and cleanup
STFINALY1 <- STAM4 %>%
  left_join(STTRANSY3, by = "STRATUM") %>%
  filter(STRATUM != 0) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Final data structure
STFINALY <- STFINALYALL %>%
  mutate(STRATUM = as.integer(STRATUM),
         SAMPLES = as.integer(SAMPLES),
         TOTALN = as.integer(TOTALN),
         NMEAN = as.integer(NMEAN),
         SNVE = as.integer(SNVE))

# Append final data
STFINALY <- bind_rows(STFINALY, STFINALY1)

#BY YEAR - WEIGHTED BY STRATUM FACTORS***STRAIGHT LFD, NOT CPUE****************;

# Create LFWSA
LFWSA <- ABUNDALL %>%
  count(STRATUM, AREA, name="COUNT") %>%
  ungroup() %>%
  mutate(PERCENT = COUNT / sum(COUNT) * 100)  %>%
  mutate(YEAR = "ALLC") %>%
  select(-PERCENT, -COUNT)
LFWSA$FACTOR <- unname(unlist(Map(FactorAssignment, LFWSA$AREA, LFWSA$STRATUM)))

# Replace NA with 0
LFWZEROSYALL <- bind_rows(TEMPLATE, LENG) %>%
  mutate(YEAR = "ALLC", LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
  group_by(YEAR, STRATUM, LENGA) %>%
  summarise(SUM = sum(FREQUENCY, na.rm = TRUE), .groups = 'drop') %>%
  inner_join(LFWSA, by = c("YEAR", "STRATUM")) %>%
  filter(SUM > 0) %>%
  mutate(WSUM = FACTOR * SUM) %>%
  group_by(YEAR, LENGA) %>%
  summarise(WSUMSALL = sum(WSUM, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = LENGA, values_from = WSUMSALL) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Final merge
LFWFINALYALL <- YMEANSALL %>% #MATCHES
  left_join(LFWZEROSYALL, by = "YEAR") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% select(-NMEAN, -SNVE)

# Frequency table
YSA <- ABUND %>%
  count(YEAR, STRATUM, AREA) %>%
  ungroup()

# Create YLSTTEMPLT
YLSTTEMPLT <- TEMPLATE
# Append data
YLSTTEMPLT$YEAR <- as.integer(YLSTTEMPLT$YEAR)

YLSTLENGY1 <- bind_rows(YLSTTEMPLT, LENG) %>%
  mutate(LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
  group_by(YEAR, STRATUM, LENGA) %>%
  summarise(SUM = sum(FREQUENCY, na.rm = TRUE), .groups = 'drop')

# Merge data
YLSTLENG2 <- left_join(YSA, YLSTLENGY1, by = c("YEAR", "STRATUM")) %>%
  filter(SUM > 0)
YLSTLENG2$FACTOR <- unname(unlist(Map(FactorAssignment, YLSTLENG2$AREA, YLSTLENG2$STRATUM)))

# Replace NA with 0
YLSTZEROSY <- YLSTLENG2 %>%
  mutate(STSUM = FACTOR * SUM) %>%
  group_by(YEAR, LENGA) %>%
  summarise(STSUMS = sum(STSUM, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = LENGA, values_from = STSUMS) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  filter(YEAR != "1901")

# Final merge
YLSTFINALY1 <- left_join(AM4, YLSTZEROSY, by = "YEAR") %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  select(-NMEAN, -SNVE)

# Final data structure
YLSTFINALY <- rbind(LFWFINALYALL, YLSTFINALY1) 

#BY YEAR - NOT WEIGHTED BY STRATUM FACTORS***STRAIGHT LFD, NOT CPUE****************;
#NOT TESTED
# Create LFALLTOTAL data frame
LFALLTOTAL <- ABUNDALL %>%
  summarise(N = n(), SUM = sum(NUMBER, na.rm = TRUE)) %>%
  mutate(YEAR = "ALLC")

# Replace NA with 0
LFZEROSYALL <- bind_rows(TEMPLATE, LENG) %>%
  mutate(YEAR = "ALLC", LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
  group_by(YEAR, LENGA) %>%
  summarise(SUM = sum(FREQUENCY, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = LENGA, values_from = SUM) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Merge LFALLTOTAL and LFZEROSYALL
LFFINALYALL <- full_join(LFALLTOTAL, LFZEROSYALL, by = "YEAR") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Create YLSAMPLES data frame
YLSAMPLES <- ABUND %>%
  count(YEAR)

# Append LENG to TEMPLATE
TEMPLATE$YEAR <- as.integer(TEMPLATE$YEAR)
# Sort and summarize by YEAR and LENGA
YLLENGY1 <- bind_rows(TEMPLATE, LENG) %>%
  mutate(LENGA = LENGTH, FREQUENCY = FREQUENCY / MINOUT * 20) %>%
  group_by(YEAR, LENGA) %>%
  summarise(SUM = sum(FREQUENCY, na.rm = TRUE)) %>%
  ungroup()

# Merge YLLENGY1 and YLSAMPLES
YLLENGY2 <- inner_join(YLLENGY1, YLSAMPLES, by = "YEAR") %>%
  filter(!is.na(SUM))

df <- data.frame(
  YEAR = 1901,
  SUM = 0, 
  n = 0
)
df$LENGA <- list(c(1:160))
expanded_df <- unnest(df, LENGA)

# Replace NA with 0 and remove specific year
YLZEROSY <- rbind(expanded_df, YLLENGY2) %>%
  complete(YEAR, LENGA, fill = list(SUM = 0, n = 0)) %>%
  pivot_wider(names_from = LENGA, values_from = SUM) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  filter(YEAR != "1901")

# Merge AM4 and YLZEROSY
YLFINALY1 <- full_join(AM4, YLZEROSY, by = "YEAR") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  select(-NMEAN, -SNVE, -n)

# Create YLFINALY data frame
YLFINALY <- LFFINALYALL %>%
  mutate(YEAR = as.character(YEAR), TOTALN = SUM, SAMPLES = N) %>%
  select(-N, -SUM)

# Append YLFINALY1 to YLFINALY
YLFINALY <- rbind(YLFINALY, YLFINALY1)

#return STFINALY, FINAL, YLFINALY, YLSTFINALY