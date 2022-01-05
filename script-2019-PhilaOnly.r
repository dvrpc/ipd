# For 2014-2019 data.
#Tract 42091206702 returns NoData for the IPD classification it was corrected by hard coding "Well Below Average"
#For 2020 ACS check this tract of issues it may present. Other tracts with 0 population for variables are coded correctly and low population tracts are excluded.
## SETUP

# Dependencies
library(plyr); library(here); library(sf); library(summarytools); library(purrr);
library(tidycensus); library(tidyverse); library(tigris); library(dplyr); library(descr)

# Census API Key

census_api_key("", overwrite = TRUE)

# Fields

# See https://www.census.gov/data/developers/data-sets/acs-5year.html
# for the variables for Detailed Tables (B), Subject Tables (S), and Data Profiles (DP)

disabled_universe                    <- "S1810_C01_001"  
disabled_count                       <- "S1810_C02_001"  
disabled_percent                     <- "S1810_C03_001"  
ethnic_minority_universe             <- "B03002_001"
ethnic_minority_count                <- "B03002_012"
ethnic_minority_percent              <- NA
female_universe                      <- "S0101_C01_001"
female_count                         <- "S0101_C05_001"
female_percent                       <- "DP05_0003PE"
foreign_born_universe                <- "B05012_001"
foreign_born_count                   <- "B05012_003"
foreign_born_percent                 <- NA
limited_english_proficiency_universe <- "S1601_C01_001"
limited_english_proficiency_count    <- "S1601_C05_001"
limited_english_proficiency_percent  <- "S1601_C06_001"
low_income_universe                  <- "S1701_C01_001"
low_income_count                     <- "S1701_C01_042"
low_income_percent                   <- NA
older_adults_universe                <- "S0101_C01_001"
older_adults_count                   <- "S0101_C01_030"
older_adults_percent                 <- "S0101_C02_030"
racial_minority_universe             <- "B02001_001"
racial_minority_count                <- "B02001_002"
racial_minority_percent              <- NA
youth_universe                       <- "B03002_001"
youth_count                          <- "B09001_001"
youth_percent                        <- NA

ipd_year <- 2019
ipd_states <- c("NJ", "PA", "NY", "DE")
ipd_counties <- c("34009",
                  "10005",
                  "34029",
                  "36055",
                  "36095",
                  "42001",
                  "36047",
                  "42095",
                  "36113",
                  "42011",
                  "42067",
                  "36099",
                  "36039",
                  "34035",
                  "34011",
                  "36121",
                  "34019",
                  "42119",
                  "36071",
                  "42015",
                  "42111",
                  "34013",
                  "42045",
                  "42055",
                  "42079",
                  "34031",
                  "36031",
                  "10003",
                  "42069",
                  "42087",
                  "42117",
                  "42047",
                  "42077",
                  "36079",
                  "36073",
                  "36061",
                  "42017",
                  "42103",
                  "36045",
                  "42093",
                  "36037",
                  "42105",
                  "42099",
                  "42057",
                  "36081",
                  "36063",
                  "42003",
                  "42115",
                  "36025",
                  "42071",
                  "36003",
                  "42059",
                  "36043",
                  "42035",
                  "42113",
                  "36105",
                  "42125",
                  "42065",
                  "36059",
                  "36083",
                  "34027",
                  "34003",
                  "42039",
                  "34001",
                  "34039",
                  "36027",
                  "36023",
                  "42029",
                  "36085",
                  "42075",
                  "10001",
                  "42107",
                  "42021",
                  "36065",
                  "34015",
                  "42063",
                  "42013",
                  "36057",
                  "36089",
                  "42073",
                  "34025",
                  "36067",
                  "42027",
                  "42043",
                  "36111",
                  "42133",
                  "42049",
                  "42123",
                  "42053",
                  "36049",
                  "36007",
                  "42007",
                  "36109",
                  "36119",
                  "42083",
                  "36051",
                  "36093",
                  "36097",
                  "42109",
                  "42037",
                  "42023",
                  "42101",
                  "36019",
                  "42131",
                  "36029",
                  "42041",
                  "36013",
                  "42005",
                  "36101",
                  "34023",
                  "36115",
                  "36015",
                  "34037",
                  "36091",
                  "36041",
                  "36123",
                  "36107",
                  "34017",
                  "36009",
                  "36033",
                  "36087",
                  "42121",
                  "34021",
                  "36103",
                  "36001",
                  "42061",
                  "42009",
                  "42097",
                  "36117",
                  "42033",
                  "42085",
                  "34041",
                  "34033",
                  "36005",
                  "34007",
                  "42019",
                  "36011",
                  "36069",
                  "42025",
                  "42051",
                  "36021",
                  "36035",
                  "36075",
                  "36077",
                  "42089",
                  "42129",
                  "42127",
                  "42031",
                  "34005",
                  "36053",
                  "42091",
                  "36017",
                  "42081")

# Functions

min <- function(i, ..., na.rm = TRUE) {
  base::min(i, ..., na.rm = na.rm)
}

mean <- function(i, ..., na.rm = TRUE) {
  base::mean(i, ..., na.rm = na.rm)
}

sd <- function(i, ..., na.rm = TRUE) {
  stats::sd(i, ..., na.rm = na.rm)
}

max <- function(i, ..., na.rm = TRUE) {
  base::max(i, ..., na.rm = na.rm)
}

st_dev_breaks <- function(x, i, na.rm = TRUE){
  half_st_dev_count <- c(-1 * rev(seq(1, i, by = 2)),
                         seq(1, i, by = 2))
  if((i %% 2) == 1) {
    half_st_dev_breaks <- sapply(half_st_dev_count,
                                 function(i) (0.5 * i * sd(x)) + mean(x))
    half_st_dev_breaks[[1]] <- 0
    half_st_dev_breaks[[2]] <- ifelse(half_st_dev_breaks[[2]] < 0,
                                      0.1,
                                      half_st_dev_breaks[[2]])
    half_st_dev_breaks[[i + 1]] <- ifelse(max(x) > half_st_dev_breaks[[i + 1]],
                                          max(x), half_st_dev_breaks[[i + 1]])
  } else {
    half_st_dev_breaks <- NA
  }
  return(half_st_dev_breaks)
}

move_last <- function(df, last_col) {
  match(c(setdiff(names(df), last_col), last_col), names(df))
}

description <- function(i) {
  des <- as.numeric(summarytools::descr(i, na.rm = TRUE,
                                        stats = c("min", "med", "mean", "sd", "max")))
  des <- c(des[1:4], des[4] / 2, des[5])
  return(des)
}

round_0 <- function(i) round(i, 0)
round_1 <- function(i) round(i, 1)
round_2 <- function(i) round(i, 2)
mult_100 <- function(i) i * 100

## VARIANCE REPLICATES

ipd_states_numeric <- fips_codes %>%
  filter(state %in% ipd_states) %>%
  select(state_code) %>% distinct(.) %>% pull(.)
var_rep <- NULL

##########changed .gz to .zip and r bind to dplyr::bind_rows
for (i in 1:length(ipd_states)){
  url <- paste0("https://www2.census.gov/programs-surveys/acs/replicate_estimates/",
                ipd_year,
                "/data/5-year/140/B02001_",
                ipd_states_numeric[i],
                ".csv.zip")
  temp <- tempfile()
  download.file(url, temp)
  var_rep_i <- read_csv(unzip(temp))
  var_rep <- dplyr::bind_rows(var_rep, var_rep_i)
} 

var_rep <- var_rep %>%
  mutate_at(vars(GEOID), ~(str_sub(., 8, 18))) %>%
  filter(str_sub(GEOID, 1, 5) %in% ipd_counties) %>%
  select(-TBLID, -NAME, -ORDER, -MOE, -CME, -SE) %>%
  filter(TITLE %in% c("Black or African American alone",
                      "American Indian and Alaska Native alone",
                      "Asian alone",
                      "Native Hawaiian and Other Pacific Islander alone",
                      "Some other race alone",
                      "Two or more races:"))
num <- var_rep %>% 
  group_by(GEOID) %>%
  summarize_if(is.numeric, ~ sum(.)) %>%
  select(-GEOID)
estim <- num %>% select(ESTIMATE)
individual_replicate <- num %>% select(-ESTIMATE)
id <- var_rep %>% select(GEOID) %>% distinct(.) %>% pull(.)
sqdiff_fun <- function(v, e) (v - e) ^ 2
sqdiff <- mapply(sqdiff_fun, individual_replicate, estim) 
sum_sqdiff <- rowSums(sqdiff)
variance <- 0.05 * sum_sqdiff
moe <- round(sqrt(variance) * 1.645, 0)
rm_moe <- cbind(id, moe) %>%
  as_tibble(.) %>%
  rename(GEOID10 = id, RM_CntMOE = moe) %>%
  mutate_at(vars(RM_CntMOE), as.numeric)

## DOWNLOADS

# Counts and universes
counts <- c(disabled_count, disabled_universe,
            ethnic_minority_count, ethnic_minority_universe,
            female_count, female_universe,
            foreign_born_count, foreign_born_universe,
            limited_english_proficiency_count, limited_english_proficiency_universe,
            low_income_count, low_income_universe,
            older_adults_count, older_adults_universe,
            racial_minority_count, racial_minority_universe,
            youth_count, youth_universe)
counts_ids <- c("D_C", "D_U", 
                "EM_C", "EM_U",
                "F_C", "F_U",
                "FB_C", "FB_U",
                "LEP_C", "LEP_U", 
                "LI_C", "LI_U",
                "OA_C", "OA_U", 
                "RM_C", "RM_U", 
                "Y_C", "Y_U")

# Zip count API variables and their appropriate abbreviations together
counts_calls <- tibble(id = counts_ids, api = counts) %>%
  drop_na(.)

# Separate into different types of API requests
s_calls <- counts_calls %>%
  filter(str_sub(api, 1, 1) == "S")  # Summary Tables
d_calls <- counts_calls %>%
  filter(str_sub(api, 1, 1) == "B")  # Detailed Tables
dp_calls <- counts_calls %>%
  filter(str_sub(api, 1, 1) == "D")  # Data Profile

# Make requests; if variables exist for this type, dl and append
dl_counts <- NULL

if(length(s_calls$id > 0)){
  s_counts <- get_acs(geography = "tract",
                      state = ipd_states,
                      output = "wide",
                      year = ipd_year,
                      variables = s_calls$api) %>%
    select(-NAME)
  dl_counts <- bind_cols(dl_counts, s_counts)
}

if(length(d_calls$id > 0)){
  d_counts <- get_acs(geography = "tract",
                      state = ipd_states,
                      output = "wide",
                      year = ipd_year,
                      variables = d_calls$api) %>%
    select(-NAME)
  dl_counts <- left_join(dl_counts, d_counts)
}

if(length(dp_calls$id > 0)){
  dp_counts <- get_acs(geography = "tract",
                       state = ipd_states,
                       output = "wide",
                       year = ipd_year,
                       variables = dp_calls$api) %>%
    select(-NAME)
  dl_counts <- left_join(dl_counts, dp_counts)
}

dl_counts <- dl_counts %>%
  rename(GEOID10 = GEOID)

# For DP downloads, make sure counts_calls and dl_counts match

counts_calls$api <- str_replace(counts_calls$api, "E$", "")

for(i in 1:length(counts_calls$id)){
  names(dl_counts) <- str_replace(names(dl_counts),
                                  counts_calls$api[i],
                                  counts_calls$id[i])
}

# Identify duplicate API columns and create if missing

duplicate_cols <- counts_calls %>% 
  group_by(api) %>% 
  filter(n()>1) %>%
  summarize(orig = id[1],
            duplicator = id[2])

e_paste <- function(i) paste0(i, "E")
m_paste <- function(i) paste0(i, "M")
e_rows <- apply(duplicate_cols, 2, e_paste)
m_rows <- apply(duplicate_cols, 2, m_paste)

combined_rows <- as_tibble(rbind(e_rows, m_rows)) %>%
  mutate_all(as.character)

for(i in 1:length(combined_rows$api)){
  dl_counts[combined_rows$duplicator[i]] <- dl_counts[combined_rows$orig[i]]
}

# Percentages

percs <- c(disabled_percent,
           ethnic_minority_percent,
           female_percent,
           foreign_born_percent,
           limited_english_proficiency_percent,
           low_income_percent,
           older_adults_percent,
           racial_minority_percent,
           youth_percent)

percs_ids <- c("D_P", "EM_P", "F_P", "FB_P", "LEP_P",
               "LI_P", "OA_P", "RM_P", "Y_P")

percs_calls <- tibble(id = percs_ids, api = percs) %>%
  drop_na(.)

s_calls <- percs_calls %>%
  filter(str_sub(api, 1, 1) == "S")

d_calls <- percs_calls %>%
  filter(str_sub(api, 1, 1) == "B")

dp_calls <- percs_calls %>%
  filter(str_sub(api, 1, 1) == "D")

dl_percs <- NULL

if(length(s_calls$id > 0)){
  s_percs <- get_acs(geography = "tract",
                     state = ipd_states,
                     output = "wide",
                     year = ipd_year,
                     variables = s_calls$api) %>%
    select(-NAME)
  dl_percs <- bind_cols(dl_percs, s_percs)
}

if(length(d_calls$id > 0)){
  d_percs <- get_acs(geography = "tract",
                     state = ipd_states,
                     output = "wide",
                     year = ipd_year,
                     variables = d_calls$api) %>%
    select(-NAME)
  dl_percs <- left_join(dl_percs, d_percs)
}

if(length(dp_calls$id > 0)){
  dp_percs <- get_acs(geography = "tract",
                      state = ipd_states,
                      output = "wide",
                      year = ipd_year,
                      variables = dp_calls$api) %>%
    select(-NAME)
  dl_percs <- left_join(dl_percs, dp_percs)
}

dl_percs <- dl_percs %>%
  rename(GEOID10 = GEOID)

# For DP downloads, make sure percs_calls and dl_percs match

percs_calls$api <- str_replace(percs_calls$api, "PE", "")
names(dl_percs) <- str_replace(names(dl_percs), "PE", "E")
names(dl_percs) <- str_replace(names(dl_percs), "PM", "M")

for(i in 1:length(percs_calls$id)){
  names(dl_percs) <- str_replace(names(dl_percs),
                                 percs_calls$api[i],
                                 percs_calls$id[i])
}

# Subset for DVRPC region

# Desired RM_CE = RM_UE - RM_CE
dl_counts <- dl_counts %>%
  filter(str_sub(GEOID10, 1, 5) %in% ipd_counties)
dl_percs <- dl_percs %>%
  filter(str_sub(GEOID10, 1, 5) %in% ipd_counties)


## CALCULATIONS

# Exception 1: RM_CE = RM_UE - RM_CE

dl_counts <- dl_counts %>% mutate(x = RM_UE - RM_CE) %>%
  select(-RM_CE) %>%
  rename(RM_CE = x)

# Exception 2: Substitute in RM_CntMOE

if(exists("rm_moe")){
  dl_counts <- dl_counts %>%
    select(-RM_CM) %>%
    left_join(., rm_moe) %>%
    rename(RM_CM = RM_CntMOE) %>%
    mutate_at(vars(RM_CM), as.numeric)
}

# Exception 3: Slice low-population tracts

slicer <- c("34001990000",
            "34009990100",
            "34011990000",
            "34017980100",
            "34025990000",
            "34029980000",
            "34029980100",
            "34029990000",
            "34033990000",
            "42003980000",
            "42003980100",
            "42003980300",
            "42003980400",
            "42003980700",
            "42003980800",
            "42003981000",
            "42003981100",
            "42003981200",
            "42017980000",
            "42045980000",
            "42049990000",
            "42079980100",
            "42101005000",
            "42101980300",
            "42101980400",
            "42101980500",
            "42101980600",
            "42101980700",
            "42101980800",
            "42101980900",
            "36005011000",
            "36005016300",
            "36005017100",
            "36005024900",
            "36005050400",
            "36009940200",
            "36011990200",
            "36013990000",
            "36029990000",
            "36045990001",
            "36047008600",
            "36047017500",
            "36047017700",
            "36047040700",
            "36047066600",
            "36047070203",
            "36047096000",
            "36047118000",
            "36047990100",
            "36055980000",
            "36055990000",
            "36059990100",
            "36059990200",
            "36059990301",
            "36059990302",
            "36059990400",
            "36061000100",
            "36061000500",
            "36061008602",
            "36061031100",
            "36061031900",
            "36063940100",
            "36063990000",
            "36065980002",
            "36065980003",
            "36073990000",
            "36075990000",
            "36081003700",
            "36081009900",
            "36081010701",
            "36081021900",
            "36081022900",
            "36081024600",
            "36081029900",
            "36081033100",
            "36081038301",
            "36081038302",
            "36081061302",
            "36081062400",
            "36081064102",
            "36081065501",
            "36081071600",
            "36081079300",
            "36081091602",
            "36081099900",
            "36081107202",
            "36081121100",
            "36081128300",
            "36081990100",
            "36085015400",
            "36085990100",
            "36103990100",
            "36117990100",
            "36119005600",
            "10001990000",
            "10003980100",
            "10003990100",
            "10005990000",
            "34021002400" 
)
dl_counts <- dl_counts %>% filter(!(GEOID10 %in% slicer))
dl_percs <- dl_percs %>% filter(!(GEOID10 %in% slicer))

# Split `dl_counts` into list for processing

# Sort column names for consistency
# `comp` = "component parts"
#has deprecated change to tidyselect::peek_vars()
comp <- list()
comp$uni_est <- dl_counts %>% select(ends_with("UE")) %>% select(sort(current_vars()))
comp$uni_moe <- dl_counts %>% select(ends_with("UM")) %>% select(sort(current_vars()))
comp$count_est <- dl_counts %>% select(ends_with("CE")) %>% select(sort(current_vars()))
comp$count_moe <- dl_counts %>% select(ends_with("CM")) %>% select(sort(current_vars()))

# Compute percentages and associated MOEs
pct_matrix <- NULL
pct_moe_matrix <- NULL

for (c in 1:length(comp$uni_est)){
  pct <- unlist(comp$count_est[,c] / comp$uni_est[,c])
  pct_matrix <- cbind(pct_matrix, pct)
  moe <- NULL
  for (r in 1:length(comp$uni_est$LI_UE)){
    moe_indiv <- as.numeric(moe_prop(comp$count_est[r,c],
                                     comp$uni_est[r,c],
                                     comp$count_moe[r,c],
                                     comp$uni_moe[r,c]))
    moe <- append(moe, moe_indiv)
  }
  pct_moe_matrix <- cbind(pct_moe_matrix, moe)
}

# Result: `pct` and `pct_moe` have percentages and associated MOEs
pct <- as_tibble(pct_matrix) %>% mutate_all(~ . * 100) %>% mutate_all(round_1)
names(pct) <- str_replace(names(comp$uni_est), "_UE", "_PctEst")
pct_moe <- as_tibble(pct_moe_matrix) %>% mutate_all(~ . * 100) %>% mutate_all(round_1)
names(pct_moe) <- str_replace(names(comp$uni_est), "_UE", "_PctMOE")

# Exception 4: If MOE == 0; MOE = 0.1

pct_moe <- pct_moe %>% replace(., . == 0, 0.1)

# Exception 5: Substitute percentages and associated MOEs when available from AFF

pct <- pct %>% mutate(D_PctEst = dl_percs$D_PE,
                      OA_PctEst = dl_percs$OA_PE,
                      LEP_PctEst = dl_percs$LEP_PE,
                      F_PctEst = dl_percs$F_PE)
pct_moe <- pct_moe %>% mutate(D_PctMOE = dl_percs$D_PM,
                              OA_PctMOE = dl_percs$OA_PM,
                              LEP_PctMOE = dl_percs$LEP_PM,
                              F_PctMOE = dl_percs$F_PM)

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
percentile <- as_tibble(percentile_matrix) %>% mutate_all(round_2)
names(percentile) <- str_replace(names(comp$uni_est), "_UE", "_Pctile")

# Compute IPD score and classification

score_matrix <- NULL
class_matrix <- NULL

for (c in 1:length(comp$uni_est)){
  p <- unlist(comp$pct_est[,c])
  breaks <- st_dev_breaks(p, 5, na.rm = TRUE)
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

# Result: `score` and `class` have IPD scores and associated descriptions
score <- as_tibble(score_matrix)
names(score) <- str_replace(names(comp$uni_est), "_UE", "_Score")
class <- as_tibble(class_matrix)
names(class) <- str_replace(names(comp$uni_est), "_UE", "_Class")

# Compute total IPD score
#score <- score %>% mutate(IPD_Score = rowSums(.))
score <- score %>%
      mutate(IPD_Score = reduce(., `+`))

## CLEANING

# Merge all information into a single df
ipd <- bind_cols(dl_counts, pct) %>%
  bind_cols(., pct_moe) %>%
  bind_cols(., percentile) %>%
  bind_cols(., score) %>%
  bind_cols(., class)

# Rename columns
names(ipd) <- str_replace(names(ipd), "_CE", "_CntEst")
names(ipd) <- str_replace(names(ipd), "_CM", "_CntMOE")
ipd <- ipd %>% mutate(STATEFP10 = str_sub(GEOID10, 1, 2),
                      COUNTYFP10 = str_sub(GEOID10, 3, 5),
                      NAME10 = str_sub(GEOID10, 6, 11),
                      U_TPopEst = F_UE,
                      U_TPopMOE = F_UM,
                      U_Pop6Est = LEP_UE,
                      U_Pop6MOE = LEP_UM,
                      U_PPovEst = LI_UE,
                      U_PPovMOE = LI_UM,
                      U_PNICEst = D_UE,
                      U_PNICMOE = D_UM) %>%
  select(-ends_with("UE"), -ends_with("UM"))

# Reorder columns
ipd <- ipd %>% select(GEOID10, STATEFP10, COUNTYFP10, NAME10, sort(current_vars())) %>%
  select(move_last(., c("IPD_Score", "U_TPopEst", "U_TPopMOE",
                        "U_Pop6Est", "U_Pop6MOE", "U_PPovEst",
                        "U_PPovMOE", "U_PNICEst", "U_PNICMOE")))

#slice the ipd scores off
ipdScore <- subset(ipd, select = IPD_Score)
ipd$IPD_Score <-as.numeric(ipd$IPD_Score)
ipd <- subset(ipd, select = -c(IPD_Score))

#this breaks the IPD score
# Replace NA with NoData if character and 0 if numeric
ipd <- ipd %>% mutate_if(is.character, ~(ifelse(is.na(.), "NoData", .))) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .)))

#add back in IPD_Score
ipd <- cbind(ipd, ipdScore)

# Append low-population tracts back onto dataset
slicer <- enframe(slicer, name = NULL, value = "GEOID10")
ipd <- plyr::rbind.fill(ipd, slicer)


## SUMMARY TABLES

# Replace 0 with NA for our purposes
ipd_summary <- ipd
ipd_summary[ipd_summary == 0]

# Count of tracts that fall in each bin
counts <- ipd_summary %>% select(ends_with("Class"))
export_counts <- apply(counts, 2, function(i) plyr::count(i))
for(i in 1:length(export_counts)){
  export_counts[[i]]$var <- names(export_counts)[i]
}
export_counts <- map_dfr(export_counts, `[`, c("var", "x", "freq"))

# Format export
counts <- ipd_summary %>% select(ends_with("Class"))
export_counts <- apply(counts, 2, function(i) plyr::count(i))
for(i in 1:length(export_counts)){
  export_counts[[i]]$var <- names(export_counts)[i]
}
export_counts <- map_dfr(export_counts, `[`, c("var", "x", "freq"))
colnames(export_counts) <- c("Variable", "Classification", "Count")
export_counts$Classification <- factor(export_counts$Classification,
                                       levels = c("Well Below Average",
                                                  "Below Average",
                                                  "Average",
                                                  "Above Average",
                                                  "Well Above Average",
                                                  "NoData"))
export_counts <- arrange(export_counts, Variable, Classification)
export_counts <- export_counts %>%
  spread(Classification, Count) %>%
  mutate_all(~(replace_na(., 0))) %>%
  mutate(TOTAL = rowSums(.[2:7], na.rm = TRUE))

# Bin break points
breaks <- ipd_summary %>% select(ends_with("PctEst"))
export_breaks <- round(mapply(st_dev_breaks, x = breaks, i = 5, na.rm = TRUE), digits = 3)
export_breaks <- as_tibble(export_breaks) %>%
  mutate(Class = c("Min", "1", "2", "3", "4", "Max")) %>%
  select(Class, current_vars())

# Minimum, median, mean, standard deviation, maximum
pcts <- ipd_summary %>% select(ends_with("PctEst"))

summary_data <- apply(pcts, MARGIN=2, description)
export_summary <- as_tibble(summary_data) %>%
  mutate_all(round_2) %>%
  mutate(Statistic = c("Minimum", "Median", "Mean", "SD", "Half-SD", "Maximum")) %>%
  select(Statistic, current_vars())

# Population-weighted county means for each indicator
export_means <- dl_counts %>% select(GEOID10, ends_with("UE"), ends_with("CE")) %>%
  select(GEOID10, sort(current_vars())) %>%
  mutate(County = str_sub(GEOID10, 1, 5)) %>%
  select(-GEOID10) %>%
  group_by(County) %>%
  summarize(D_PctEst = sum(D_CE) / sum(D_UE),
            EM_PctEst = sum(EM_CE) / sum(EM_UE),
            F_PctEst = sum(F_CE) / sum(F_UE),
            FB_PctEst = sum(FB_CE) / sum(FB_UE),
            LEP_PctEst = sum(LEP_CE) / sum(LEP_UE),
            LI_PctEst = sum(LI_CE) / sum(LI_UE),
            OA_PctEst = sum(OA_CE) / sum(OA_UE),
            RM_PctEst = sum(RM_CE) / sum(RM_UE),
            Y_PctEst = sum(Y_CE) / sum(Y_UE)) %>%
  mutate_if(is.numeric, ~ . * 100) %>%
  mutate_if(is.numeric, round_1)

# Replace NA with NoData if character and -99999 if numeric
#moved from line 352 so tract 42091206702 doesn't mess up breaks and means by indicator
ipd <- ipd %>% mutate_if(is.character, ~(ifelse(is.na(.), "NoData", .))) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), -99999, .)))
ipd_summary[ipd_summary == -99999] <- NA

## EXPORT

options(tigris_use_cache = TRUE, tigris_class = "sf")
st <- str_sub(ipd_counties, 1, 2)
cty <- str_sub(ipd_counties, 3, 5)
trct <- map2(st, cty, ~{tracts(state = .x,
                               county = .y,
                               #cb = TRUE,
                               year = ipd_year)}) %>%
  rbind_tigris() %>%
  st_transform(., 26918) %>%
  select(GEOID) %>%
  left_join(., ipd, by = c("GEOID" = "GEOID10")) %>%
  rename(GEOID10 = GEOID)

st_write(trct, here("outputs", "ipd4state.shp"), delete_dsn = TRUE, quiet = TRUE)
write_csv(ipd, here("outputs", "ipd4state.csv"))
write_csv(export_counts, here("outputs", "counts_by_indicator4state.csv"))
write_csv(export_breaks, here("outputs", "breaks_by_indicator4state.csv"))
write_csv(export_summary, here("outputs", "summary_by_indicator4state.csv"))
write_csv(export_means, here("outputs", "mean_by_county4state.csv"))