require(tidycensus); require(tidyverse); require(sf); require(here); require(stringr)

st_dev_breaks <- function(x, i){
  half_st_dev_count <- c(-1 * rev(seq(1, i, by = 2)),
                      seq(1, i, by = 2))
  if((i %% 2) == 1) {
    half_st_dev_breaks <- unlist(lapply(half_st_dev_count,
                                     function(i) (0.5 * i * sd(x)) + mean(x)))
    half_st_dev_breaks[[1]] <- ifelse(min(x) < half_st_dev_breaks[[1]],
                                   min(x), half_st_dev_breaks[[1]])
    half_st_dev_breaks[[i + 1]] <- ifelse(max(x) > half_st_dev_breaks[[i + 1]],
                                       max(x), half_st_dev_breaks[[i + 1]])
  } else {
    half_st_dev_breaks <- NA
  }
  return(half_st_dev_breaks)
}

summary_tables <- get_acs(geography = "tract", state = "DC",
                          variables = c(LI_U = "S1701_C01_001",
                                        LI_C = "S1701_C01_042",
                                        F_U = "S0101_C01_001",
                                        F_C = "S0101_C03_001",
                                        D_U = "S1810_C01_001",
                                        D_C = "S1810_C02_001",
                                        OA_U = "S1601_C01_001",
                                        # LEP_U = "S1601_C01_001", # Redundant downloads not allowed
                                        LEP_C = "S1601_C05_001"),
                          output = "wide") %>%
  select(-NAME) %>%
  mutate(LEP_UE = OA_UE, LEP_UM = OA_UM)
detailed_tables <- get_acs(geography = "tract", state = "DC",
                           variables = c(EM_U = "B03002_001",
                                         EM_C = "B03002_012",
                                         # Y_U = "B03002_001", # Redundant downloads not allowed
                                         Y_C = "B09001_001",
                                         FB_U = "B05012_001",
                                         FB_C = "B05012_003"),
                           output = "wide") %>%
  select(-NAME) %>%
  mutate(Y_UE = EM_UE, Y_UM = EM_UM)
data_profiles <- get_acs(geography = "tract", state = "DC",
                         variables = c(OA_CE = "DP05_0025E"),
                         output = "wide") %>%
  rename(OA_CM = DP05_0025M) %>%
  select(-NAME)

dl <- left_join(summary_tables, detailed_tables) %>%
  left_join(., data_profiles)

# Throw vars in list so we can apply same functions in one go
sub <- list()
sub[[1]] <- dl %>% select(ends_with("UE"))
sub[[2]] <- dl %>% select(ends_with("UM"))
sub[[3]] <- dl %>% select(ends_with("CE"))
sub[[4]] <- dl %>% select(ends_with("CM"))

# Compute percentages and pct moes for all variables
pct_matrix <- NULL
pct_moe_matrix <- NULL
for (m in 1:8){
  pct <- unlist(sub[[3]][,m] / sub[[1]][,m])
  pct_matrix <- cbind(pct_matrix, pct)
  moe <- NULL
  for (l in 1:length(sub[[1]]$LI_UE)){
    moe_indiv <- as.numeric(moe_prop(sub[[3]][l,m], sub[[1]][l,m], sub[[4]][l,m], sub[[2]][l,m]))
    moe <- append(moe, moe_indiv)
  }
  pct_moe_matrix <- cbind(pct_moe_matrix, moe)
}
# pct has estimated percentages and pct_moe has associated MOEs for all variables
pct <- as_tibble(pct_matrix)
names(pct) <- str_replace(names(sub[[1]]), "_UE", "_PctEst")
pct_moe <- as_tibble(pct_moe_matrix)
names(pct_moe) <- str_replace(names(sub[[1]]), "_UE", "_PctMOE")

# append to original df
dl <- bind_cols(dl, pct) %>%
  bind_cols(., pct_moe) %>%
  select(-ends_with("UM"), -ends_with("UE"))

# BEFORE ASSIGN RANK, must substitute pcts and pct MOEs available from AFF.

# Compute rank
sub[[5]] <- dl %>% select(ends_with("PctEst"))

percentile_matrix <- NULL
for (m in 1:8){
  pct <- unlist(sub[[5]][,m])
  rank <- ecdf(pct)(pct)
  percentile_matrix <- cbind(percentile_matrix, rank)
}
percentile <- as_tibble(percentile_matrix)
names(percentile) <- str_replace(names(sub[[1]]), "_UE", "_Rank")

# append to original df
dl <- bind_cols(dl, percentile)

# Compute IPD score and classification
# Manual adjustments required:
# If pct estimate = 0 and falls in bin #1, move to bin #0
# For 0/almost 0 pop tracts, apply value of -99999 to numeric fields, "no data" to text fields
score_matrix <- NULL
class_matrix <- NULL
for (m in 1:8){
  pct <- unlist(sub[[5]][,m])
  breaks <- st_dev_breaks(pct, 5)
  score <- (cut(pct, labels = FALSE, breaks = breaks,
                include.lowest = TRUE, right = TRUE)) - 1
  class <- case_when(score == 0 ~ "Well Below Average",
                     score == 1 ~ "Below Average",
                     score == 2 ~ "Average",
                     score == 3 ~ "Above Average",
                     score == 4 ~ "Well Above Average")
  score_matrix <- cbind(score_matrix, score)
  class_matrix <- cbind(class_matrix, class)
}
score <- as_tibble(score_matrix)
names(score) <- str_replace(names(sub[[1]]), "_UE", "_Score")
class <- as_tibble(class_matrix)
names(class) <- str_replace(names(sub[[1]]), "_UE", "_Class")

# append to original df
dl <- bind_cols(dl, score) %>%
  bind_cols(., class)

# Rename columns
names(dl) <- str_replace(names(dl), "_CE", "_CntEst")
names(dl) <- str_replace(names(dl), "_CM", "_CntMOE")

# Reorder, alphabetical
final <- dl %>% 
  select(GEOID, sort(current_vars()))

# Tabulate sum scores
totals <- final %>% select(ends_with("Score")) %>%
  mutate(IPD_Score = rowSums(.)) %>%
  select(IPD_Score)

final <- bind_cols(final, totals)

# Export
write_csv(final, here("outputs", "ipd.csv"))

# W I S H  L I S T
# We appear to still be missing a variable
# Universes actually appear at *end* of "dream schema," oops--don't drop 'em
# Variance replicate tables? B02001
# Adjustments for bin placement and flagging zeroes noted above. Has to do with bounds of bin 0 entirely negative
# Summary tables. For each indicator:
#     All bin breaks
#     Count of tracts that fall in each bin
#     5 number summary, sd, half-sd
#     County means

# if estimated percentage == 0 & MOE == 0; make MOE = 0.1?

# Communicating statistically significant differences between census tracts (NOT one at a time)

# Current nodata tracts?
# 42045980000
# 42017980000
# 42101980800
# 42101980300
# 42101980500
# 42101980400
# 42101980900
# 42101980700
# 42101980600
# 42101005000
