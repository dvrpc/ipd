## SETUP
# Dependencies
require(tidycensus); require(tidyverse); require(here)

# Functions
source("functions.R")

## DOWNLOADS
# API Calls
# For counts (universes and estimates)
# EXCEPTION 1: API does not allow redundant downloads; universes are duplicated after download
# EXCEPTION 2: Desired RM_CE = RM_UE - RM_CE; computed after download. Makes estimate correct but MOE wrong
s_counts <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                    variables = c(LI_U = "S1701_C01_001",
                                  LI_C = "S1701_C01_042",
                                  F_U = "S0101_C01_001",
                                  F_C = "S0101_C05_001",
                                  D_U = "S1810_C01_001",
                                  D_C = "S1810_C02_001",
                                  OA_U = "S1601_C01_001",
                                  OA_C = "S0101_C01_030",
                                  # LEP_U = "S1601_C01_001", # Redundant download
                                  LEP_C = "S1601_C05_001")) %>%
  select(-NAME) %>% mutate(LEP_UE = OA_UE, LEP_UM = OA_UM)
d_counts <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                    variables = c(EM_U = "B03002_001",
                                  EM_C = "B03002_012",
                                  # Y_U = "B03002_001", # Redundant download
                                  Y_C = "B09001_001",
                                  FB_U = "B05012_001",
                                  FB_C = "B05012_003",
                                  RM_U = "B02001_001",
                                  RM_C = "B02001_002")) %>%
  mutate(Y_UE = EM_UE, Y_UM = EM_UM, x = RM_UE - RM_CE) %>%
  select(-NAME, -RM_CE) %>% 
  rename(RM_CE = x)

# For available percentages
s_percs <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                   variables = c(D_P = "S1810_C03_001",
                                 OA_P = "S0101_C02_030",
                                 LEP_P = "S1601_C06_001")) %>% select(-NAME)
dp_percs <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                    variables = c(F_P = "DP05_0003PE")) %>%
  rename(F_PE = F_P, F_PM = DP05_0003PM) %>% select(-NAME)

# Combine downloads into merged files
# Subset for DVRPC region
keep_cty <- c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")
dl_counts <- left_join(s_counts, d_counts) %>%
  filter(str_sub(GEOID, 1, 5) %in% keep_cty)
dl_percs <- left_join(s_percs, dp_percs) %>%
  filter(str_sub(GEOID, 1, 5) %in% keep_cty)

## CALCULATIONS
# Split `dl_counts` into list for processing
# Sort column names for consistency
# `comp` = "component parts"
comp <- list()
comp[[1]] <- dl_counts %>% select(ends_with("UE")) %>% select(sort(current_vars()))
comp[[2]] <- dl_counts %>% select(ends_with("UM")) %>% select(sort(current_vars()))
comp[[3]] <- dl_counts %>% select(ends_with("CE")) %>% select(sort(current_vars()))
comp[[4]] <- dl_counts %>% select(ends_with("CM")) %>% select(sort(current_vars()))

# Compute percentages and associated MOEs
pct_matrix <- NULL
pct_moe_matrix <- NULL
for (m in 1:length(comp[[1]])){
  pct <- unlist(comp[[3]][,m] / comp[[1]][,m] * 100)
  pct <- round(pct, digits = 3)
  pct_matrix <- cbind(pct_matrix, pct)
  moe <- NULL
  for (l in 1:length(comp[[1]]$LI_UE)){
    moe_indiv <- as.numeric(moe_prop(comp[[3]][l,m], comp[[1]][l,m], comp[[4]][l,m], comp[[2]][l,m])) * 100
    moe_indiv <- round(moe_indiv, digits = 3)
    moe <- append(moe, moe_indiv)
  }
  pct_moe_matrix <- cbind(pct_moe_matrix, moe)
}

# Result: `pct` and `pct_moe` have percentages and associated MOEs
pct <- as_tibble(pct_matrix)
names(pct) <- str_replace(names(comp[[1]]), "_UE", "_PctEst")
pct_moe <- as_tibble(pct_moe_matrix)
names(pct_moe) <- str_replace(names(comp[[1]]), "_UE", "_PctMOE")

# EXCEPTION 1: If estimated percentage == 0 & MOE == 0; MOE = 0.1
# This is matrix math. Only overwrite MOE where pct_matrix + pct_moe_matrix == 0
overwrite_locations <- which(pct_matrix + pct_moe_matrix == 0, arr.ind = TRUE)
pct_moe[overwrite_locations] <- 0

# EXCEPTION 2: Substitute percentages and associated MOEs when available from AFF
pct <- pct %>% mutate(D_PctEst = dl_percs$D_PE,
                      OA_PctEst = dl_percs$OA_PE,
                      LEP_PctEst = dl_percs$LEP_PE,
                      F_PctEst = dl_percs$F_PE)
pct_moe <- pct_moe %>% mutate(D_PctMOE = dl_percs$D_PM,
                              OA_PctMOE = dl_percs$OA_PM,
                              LEP_PctMOE = dl_percs$LEP_PM,
                              F_PctMOE = dl_percs$F_PM)

# (FUTURE) EXCEPTION 3: Use variance replicates to compute RM_CntMOE and RM_PctMOE

# Compute percentile
# Add percentages to `comp`; sort column names for consistency
comp[[5]] <- pct %>% select(sort(current_vars()))

percentile_matrix <- NULL
for (m in 1:length(comp[[1]])){
  p <- unlist(comp[[5]][,m])
  rank <- ecdf(p)(p) * 100
  rank <- round(rank, digits = 3)
  percentile_matrix <- cbind(percentile_matrix, rank)
}

# Result: `percentile` has rank
percentile <- as_tibble(percentile_matrix)
names(percentile) <- str_replace(names(comp[[1]]), "_UE", "_Rank")

# Compute IPD score and classification
# EXCEPTION BURIED IN LOOP: If pct estimate = 0 and falls in bin #1, move to bin #0
score_matrix <- NULL
class_matrix <- NULL
for (m in 1:length(comp[[1]])){
  p <- unlist(comp[[5]][,m])
  breaks <- st_dev_breaks(p, 5, na.rm = TRUE)
  score <- (cut(p, labels = FALSE, breaks = breaks,
                include.lowest = TRUE, right = TRUE)) - 1
  overwrite_locations <- which(score == 0 & p == 0)
  score[overwrite_locations] <- 0
  class <- case_when(score == 0 ~ "Well Below Average",
                     score == 1 ~ "Below Average",
                     score == 2 ~ "Average",
                     score == 3 ~ "Above Average",
                     score == 4 ~ "Well Above Average")
  score_matrix <- cbind(score_matrix, score)
  class_matrix <- cbind(class_matrix, class)
}

# Result: `score` and `class` have IPD scores and associated descriptions
score <- as_tibble(score_matrix)
names(score) <- str_replace(names(comp[[1]]), "_UE", "_Score")
class <- as_tibble(class_matrix)
names(class) <- str_replace(names(comp[[1]]), "_UE", "_Class")

# Compute total IPD score
score <- score %>% mutate(IPD_Score = rowSums(.))

## CLEANING
# Merge all information into a single df
df <- bind_cols(dl_counts, pct) %>%
  bind_cols(., pct_moe) %>%
  bind_cols(., percentile) %>%
  bind_cols(., score) %>%
  bind_cols(., class)

# Rename columns
names(df) <- str_replace(names(df), "_CE", "_CntEst")
names(df) <- str_replace(names(df), "_CM", "_CntMOE")
df <- df %>% mutate(U_TPopEst = F_UE, U_TPopMOE = F_UM, U_Pop6Est = LEP_UE,
                    U_Pop6MOE = LEP_UM, U_PPovEst = LI_UE, U_PPovMOE = LI_UM,
                    U_PNICEst = D_UE, U_PNICMOE = D_UM) %>%
  select(-ends_with("UE"), -ends_with("UM"))

# Reorder columns
df <- df %>% select(GEOID, sort(current_vars())) %>%
  select(move_last(., c("IPD_Score", "U_TPopEst", "U_TPopMOE", "U_Pop6Est",
                        "U_Pop6MOE", "U_PPovEst", "U_PPovMOE", "U_PNICEst", "U_PNICMOE")))

# Replace NA with NoData if character and -99999 if numeric
df <- df %>% mutate_if(is.character, funs(ifelse(is.na(.), "NoData", .))) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), -99999, .)))

# Slice unwanted tracts
slicer <- c("42045980000", "42017980000", "42101980800",
            "42101980300", "42101980500", "42101980400",
            "42101980900", "42101980700", "42101980600",
            "42101005000")
df <- df %>% filter(!(GEOID %in% slicer))

# Export result
write_csv(df, here("outputs", "ipd.csv"))
