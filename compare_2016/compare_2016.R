# SECTION 1: Compute 2016 IPD just as you would compute 2017
# SECTION 2: Compare to the 2016 online data
# SECTION 3: Compare MOEs
#-------------------------------------------------------------------------------------------
# SECTION 1: Compute 2016 IPD just as you would compute 2017
require(tidycensus); require(tidyverse); require(here)
source("functions.R")
s_counts <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                    year = 2016,
                    variables = c(LI_U = "S1701_C01_001",
                                  LI_C = "S1701_C01_042",
                                  F_U = "S0101_C01_001",
                                  F_C = "S0101_C03_001",
                                  D_U = "S1810_C01_001",
                                  D_C = "S1810_C02_001",
                                  # OA_U = "S0101_C01_001", # Redundant download
                                  LEP_U = "S1601_C01_001",
                                  LEP_C = "S1601_C05_001")) %>%
  select(-NAME) %>% mutate(OA_UE = F_UE, OA_UM = F_UM)
d_counts <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                    year = 2016,
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
dp_counts <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                     year = 2016,
                     variables = c(OA_CE = "DP05_0025E")) %>%
  rename(OA_CM = DP05_0025M) %>% select(-NAME)
s_percs <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                   year = 2016,
                   variables = c(D_P = "S1810_C03_001",
                                 OA_P = "S0101_C01_028",
                                 LEP_P = "S1601_C06_001")) %>% select(-NAME)
dp_percs <- get_acs(geography = "tract", state = c(34,42), output = "wide",
                    year = 2016,
                    variables = c(F_P = "DP05_0003PE")) %>%
  rename(F_PE = F_P, F_PM = DP05_0003PM) %>% select(-NAME)
keep_cty <- c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")
dl_counts <- left_join(s_counts, d_counts) %>%
  left_join(., dp_counts) %>%
  filter(str_sub(GEOID, 1, 5) %in% keep_cty)
dl_percs <- left_join(s_percs, dp_percs) %>%
  filter(str_sub(GEOID, 1, 5) %in% keep_cty) %>%
  mutate_if(is.numeric, funs(. / 100))
if(TRUE %in% (list.files(here("compare_2016")) == "rm_moe.csv")){
  rm_moe <- read_csv(here("compare_2016", "rm_moe.csv")) %>%
    mutate_at(vars(GEOID), as.character)
  dl_counts <- dl_counts %>% left_join(., rm_moe) %>%
    select(-RM_CM) %>%
    rename(RM_CM = RM_CntMOE)
}
slicer <- c("42045980000", "42017980000", "42101980800",
            "42101980300", "42101980500", "42101980400",
            "42101980900", "42101980700", "42101980600",
            "42101005000")
dl_counts <- dl_counts %>% filter(!(GEOID %in% slicer))
dl_percs <- dl_percs %>% filter(!(GEOID %in% slicer))
comp <- list()
comp$uni_est <- dl_counts %>% select(ends_with("UE")) %>% select(sort(current_vars()))
comp$uni_moe <- dl_counts %>% select(ends_with("UM")) %>% select(sort(current_vars()))
comp$count_est <- dl_counts %>% select(ends_with("CE")) %>% select(sort(current_vars()))
comp$count_moe <- dl_counts %>% select(ends_with("CM")) %>% select(sort(current_vars()))
pct_matrix <- NULL
pct_moe_matrix <- NULL
for (c in 1:length(comp$uni_est)){
  pct <- unlist(comp$count_est[,c] / comp$uni_est[,c])
  pct_matrix <- cbind(pct_matrix, pct)
  moe <- NULL
  for (r in 1:length(comp$uni_est$LI_UE)){
    moe_indiv <- as.numeric(moe_prop(comp$count_est[r,c], comp$uni_est[r,c], comp$count_moe[r,c], comp$uni_moe[r,c]))
    moe <- append(moe, moe_indiv)
  }
  pct_moe_matrix <- cbind(pct_moe_matrix, moe)
}
pct <- as_tibble(pct_matrix) %>% mutate_all(round, 3)
names(pct) <- str_replace(names(comp$uni_est), "_UE", "_PctEst")
pct_moe <- as_tibble(pct_moe_matrix) %>% mutate_all(round, 3)
names(pct_moe) <- str_replace(names(comp$uni_est), "_UE", "_PctMOE")
overwrite_locations <- which(pct_matrix + pct_moe_matrix == 0, arr.ind = TRUE)
pct_moe[overwrite_locations] <- 0
pct <- pct %>% mutate(D_PctEst = dl_percs$D_PE,
                      OA_PctEst = dl_percs$OA_PE,
                      LEP_PctEst = dl_percs$LEP_PE,
                      F_PctEst = dl_percs$F_PE)
pct_moe <- pct_moe %>% mutate(D_PctMOE = dl_percs$D_PM,
                              OA_PctMOE = dl_percs$OA_PM,
                              LEP_PctMOE = dl_percs$LEP_PM,
                              F_PctMOE = dl_percs$F_PM)
comp$pct_est <- pct %>% select(sort(current_vars()))
percentile_matrix <- NULL
for (c in 1:length(comp$uni_est)){
  p <- unlist(comp$pct_est[,c])
  rank <- ecdf(p)(p)
  percentile_matrix <- cbind(percentile_matrix, rank)
}
percentile <- as_tibble(percentile_matrix) %>% mutate_all(round, 2)
names(percentile) <- str_replace(names(comp$uni_est), "_UE", "_Rank")
score_matrix <- NULL
class_matrix <- NULL
break_storage <- matrix(nrow = 9, ncol = 6)
for (c in 1:length(comp$uni_est)){
  p <- unlist(comp$pct_est[,c])
  breaks <- st_dev_breaks(p, 5, na.rm = TRUE)
  break_storage[c,] <- breaks
  score <- case_when(p < breaks[2] ~ 0,
                     p >= breaks[2] & p < breaks[3] ~ 1,
                     p >= breaks[3] & p < breaks[4] ~ 2,
                     p >= breaks[4] & p < breaks[5] ~ 3,
                     p >= breaks[5] ~ 4)
  class <- case_when(score == 0 ~ "Well Below Average",
                     score == 1 ~ "Below Average",
                     score == 2 ~ "Average",
                     score == 3 ~ "Above Average",
                     score == 4 ~ "Well Above Average")
  score_matrix <- cbind(score_matrix, score)
  class_matrix <- cbind(class_matrix, class)
}
score <- as_tibble(score_matrix)
names(score) <- str_replace(names(comp$uni_est), "_UE", "_Score")
class <- as_tibble(class_matrix)
names(class) <- str_replace(names(comp$uni_est), "_UE", "_Class")
score <- score %>% mutate(IPD_Score = rowSums(.))
df <- bind_cols(dl_counts, pct) %>%
  bind_cols(., pct_moe) %>%
  bind_cols(., percentile) %>%
  bind_cols(., score) %>%
  bind_cols(., class)
names(df) <- str_replace(names(df), "_CE", "_CntEst")
names(df) <- str_replace(names(df), "_CM", "_CntMOE")
df <- df %>% mutate(U_TPopEst = F_UE, U_TPopMOE = F_UM, U_Pop6Est = LEP_UE,
                    U_Pop6MOE = LEP_UM, U_PPovEst = LI_UE, U_PPovMOE = LI_UM,
                    U_PNICEst = D_UE, U_PNICMOE = D_UM) %>%
  select(-ends_with("UE"), -ends_with("UM"))

df <- df %>% select(GEOID, sort(current_vars())) %>%
  select(move_last(., c("IPD_Score", "U_TPopEst", "U_TPopMOE", "U_Pop6Est",
                        "U_Pop6MOE", "U_PPovEst", "U_PPovMOE", "U_PNICEst", "U_PNICMOE")))
df <- df %>% mutate_if(is.character, funs(ifelse(is.na(.), "NoData", .))) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), -99999, .)))
write_csv(df, here("compare_2016", "ipd_2016.csv"))

# Compare PctEst and PctMOE to 2016 webmap download
web <- read_csv(here("compare_2016", "DVRPC_2016_Indicators_of_Potential_Disadvantage.csv")) %>%
  mutate_at(vars("GEOID10"), as.character)
new_data <- df
names(new_data) <- paste(names(new_data), "n", sep = "_")
compare <- left_join(web, new_data, by = c("GEOID10" = "GEOID_n")) %>%
  select(GEOID10, sort(current_vars()))
compare_estimates <- compare %>% select(GEOID10, contains("PctEst"))
compare_counts <- compare %>% select(GEOID10, contains("CntEst"))
compare_moes <- compare %>% select(GEOID10, contains("PctMOE"))
compare_cnt_moes <- compare %>% select(GEOID10, contains("CntMOE"))
compare_pctile <- compare %>% select(GEOID10, contains("Pctile"), contains("Rank"))
compare_score <- compare %>% select(GEOID10, contains("Score"))

write_csv(compare_estimates, here("compare_2016", "test_compare_2016_pct.csv"))
write_csv(compare_counts, here("compare_2016", "test_compare_2016_count.csv"))
write_csv(compare_moes, here("compare_2016", "test_compare_2016_pctMOE.csv"))
write_csv(compare_cnt_moes, here("compare_2016", "test_compare_2016_cntMOE.csv"))
write_csv(compare_pctile, here("compare_2016", "test_compare_2016_pctile.csv"))
write_csv(compare_score, here("compare_2016", "test_compare_2016_score.csv"))
write_csv(as_tibble(break_storage), here("compare_2016", "test_compare_2016_breaks.csv"))
