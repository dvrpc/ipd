## SETUP
# Dependencies
require(tidycensus); require(tidyverse); require(here)

# Functions
source("functions.R")

# Fields
youth_universe                       <- "B03002_001"
youth_count                          <- "B09001_001"
youth_percent                        <- NULL
older_adults_universe                <- "S0101_C01_001"
older_adults_count                   <- "S0101_C01_030"
older_adults_percent                 <- "S0101_C02_030"
female_universe                      <- "S0101_C01_001"
female_count                         <- "S0101_C05_001"
female_percent                       <- "DP05_0003PE"
racial_minority_universe             <- "B02001_001"
racial_minority_count                <- "B02001_002"
racial_minority_percent              <- NULL
ethnic_minority_universe             <- "B03002_001"
ethnic_minority_count                <- "B03002_012"
ethnic_minority_percent              <- NULL
foreign_born_universe                <- "B05012_001"
foreign_born_count                   <- "B05012_003"
foreign_born_percent                 <- NULL
limited_english_proficiency_universe <- "S1601_C01_001"
limited_english_proficiency_count    <- "S1601_C05_001"
limited_english_proficiency_percent  <- "S1601_C06_001"
disabled_universe                    <- "S1810_C01_001"
disabled_count                       <- "S1810_C02_001"
disabled_percent                     <- "S1810_C03_001"
low_income_universe                  <- "S1701_C01_001"
low_income_count                     <- "S1701_C01_042"
low_income_percent                   <- NULL

## DOWNLOADS
# API Calls
# For counts (universes and estimates)
# EXCEPTION 1: API does not allow redundant downloads; universes are duplicated after download
# EXCEPTION 2: Desired RM_CE = RM_UE - RM_CE; computed after download. Makes estimate correct but MOE wrong
s_counts <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                    variables = c(LI_U = low_income_universe,
                                  LI_C = low_income_count,
                                  F_U = female_universe,
                                  F_C = female_count,
                                  D_U = disabled_universe,
                                  D_C = disabled_count,
                                  # OA_U = older_adults_universe, # Redundant download
                                  OA_C = older_adults_count,
                                  LEP_U = limited_english_proficiency_universe,
                                  LEP_C = limited_english_proficiency_count)) %>%
  select(-NAME) %>% mutate(OA_UE = F_UE, OA_UM = F_UM)
d_counts <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                    variables = c(EM_U = ethnic_minority_universe,
                                  EM_C = ethnic_minority_count,
                                  # Y_U = youth_universe, # Redundant download
                                  Y_C = youth_count,
                                  FB_U = foreign_born_universe,
                                  FB_C = foreign_born_count,
                                  RM_U = racial_minority_universe,
                                  RM_C = racial_minority_count)) %>%
  mutate(Y_UE = EM_UE, Y_UM = EM_UM, x = RM_UE - RM_CE) %>%
  select(-NAME, -RM_CE) %>% 
  rename(RM_CE = x)

# For available percentages
s_percs <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                   variables = c(D_P = disabled_percent,
                                 OA_P = older_adults_percent,
                                 LEP_P = limited_english_proficiency_percent)) %>% select(-NAME)
dp_percs <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                    variables = c(F_P = female_percent)) %>%
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
comp$uni_est <- dl_counts %>% select(ends_with("UE")) %>% select(sort(current_vars()))
comp$uni_moe <- dl_counts %>% select(ends_with("UM")) %>% select(sort(current_vars()))
comp$count_est <- dl_counts %>% select(ends_with("CE")) %>% select(sort(current_vars()))
comp$count_moe <- dl_counts %>% select(ends_with("CM")) %>% select(sort(current_vars()))

# Compute percentages and associated MOEs
pct_matrix <- NULL
pct_moe_matrix <- NULL
for (c in 1:length(comp$uni_est)){
  pct <- unlist(comp$count_est[,c] / comp$uni_est[,c]) * 100
  pct_matrix <- cbind(pct_matrix, pct)
  moe <- NULL
  for (r in 1:length(comp$uni_est$LI_UE)){
    moe_indiv <- as.numeric(moe_prop(comp$count_est[r,c], comp$uni_est[r,c], comp$count_moe[r,c], comp$uni_moe[r,c])) * 100
    moe <- append(moe, moe_indiv)
  }
  pct_moe_matrix <- cbind(pct_moe_matrix, moe)
}

# Result: `pct` and `pct_moe` have percentages and associated MOEs
pct <- as_tibble(pct_matrix) %>% mutate_all(round, 1)
names(pct) <- str_replace(names(comp$uni_est), "_UE", "_PctEst")
pct_moe <- as_tibble(pct_moe_matrix) %>% mutate_all(round, 1)
names(pct_moe) <- str_replace(names(comp$uni_est), "_UE", "_PctMOE")

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

# EXCEPTION 3: Use variance replicates to compute RM_CntMOE and RM_PctMOE
if(TRUE %in% (list.files(here("outputs")) == "rm_moe.csv")){
  rm_moe <- read_csv(here("outputs", "rm_moe.csv")) %>%
    mutate_at(vars(GEOID), as.character)
  # Sort in GEOID processing order as above
  re_sort <- left_join(dl_counts, rm_moe) %>% select(RM_PctMOE) %>% pull(.)
  pct_moe <- pct_moe %>% mutate(RM_PctMOE = re_sort)
}

# Compute percentile
# Add percentages to `comp`; sort column names for consistency
comp$pct_est <- pct %>% select(sort(current_vars()))

percentile_matrix <- NULL
for (c in 1:length(comp$uni_est)){
  p <- unlist(comp$pct_est[,c])
  rank <- ecdf(p)(p)
  percentile_matrix <- cbind(percentile_matrix, rank)
}

# Result: `percentile` has rank
percentile <- as_tibble(percentile_matrix) %>% mutate_all(round, 2)
names(percentile) <- str_replace(names(comp$uni_est), "_UE", "_Rank")

# Compute IPD score and classification
# EXCEPTION BURIED IN LOOP: If pct estimate = 0 and falls in bin #1, move to bin #0
score_matrix <- NULL
class_matrix <- NULL
for (c in 1:length(comp$uni_est)){
  p <- unlist(comp$pct_est[,c])
  breaks <- st_dev_breaks(p, 5, na.rm = TRUE)
  score <- (cut(p, labels = FALSE, breaks = breaks,
                include.lowest = TRUE, right = TRUE)) - 1
  overwrite_locations <- which(score == 1 & p == 0)
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
names(score) <- str_replace(names(comp$uni_est), "_UE", "_Score")
class <- as_tibble(class_matrix)
names(class) <- str_replace(names(comp$uni_est), "_UE", "_Class")

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
