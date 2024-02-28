## SETUP
# Dependencies
library(plyr); library(here); library(sf); library(summarytools);
library(tidycensus); library(tidyverse); library(tigris); library(dplyr); library(descr)

# Set wd
setwd("C:/Users/bcarney/Documents/GitHub/ipd")

# Census API Key
census_api_key <- Sys.getenv("CENSUS_API_KEY")

# Fields
# See https://www.census.gov/data/developers/data-sets/acs-5year.html
# for the variables for Detailed Tables (B), Subject Tables (S), and Data Profiles (DP)

acs5_dt_list <- c(
  tot_pop = "B01003_001", # Total Population
  eth_uni = "B03002_001", # Ethnic Minority
  eth_est = "B03002_012",
  fbo_uni = "B05012_001", # Foreign-born
  fbo_est = "B05012_003",
  rac_uni = "B02001_001", # Racial minority
  wht_est = "B02001_002", # White Alone
  you_est = "B09001_001" # Youth
  )

acs5_st_list <- c(
  lep_uni = "S1601_C01_001", # Limited English Proficiency
  lep_est = "S1601_C05_001",
  dis_uni = "S1810_C01_001",  # Disabled
  dis_est = "S1810_C02_001",  
  dis_pct = "S1810_C03_001", 
  fem_uni = "S0101_C01_001",
  fem_est = "S0101_C05_001", # Female
  inc_uni = "S1701_C01_001", # Low Income
  inc_est = "S1701_C01_042",
  old_uni = "S0101_C01_001", # Older Population
  old_est = "S0101_C01_030",
  old_pct = "S0101_C02_030"
)

ipd_year <- 2021
ipd_states <- c("NJ", "PA")
ipd_fips <- c('34005|34007|34015|34021|42017|42029|42045|42091|42101')

raw_dt_data <- get_acs(geography = "tract",
                    variables = acs5_dt_list,
                    year = ipd_year,
                    state = ipd_states,
                    survey = "acs5",
                    output = "wide"
                    ) %>%
  mutate(year = ipd_year) %>%
  filter(str_starts(GEOID, '^34005|^34007|^34015|^34021|^42017|^42029|^42045|^42091|^42101')) %>%
  dplyr::select(-NAME) %>%
  'colnames<-'(str_replace(colnames(.), "M$", "_M")) %>%
  'colnames<-'(str_replace(colnames(.), "E$", ""))
    
raw_st_data <- get_acs(geography = "tract",
                       variables = acs5_st_list,
                       year = ipd_year,
                       state = ipd_states,
                       survey = "acs5",
                       output = "wide"
) %>%
  mutate(year = ipd_year) %>%
  filter(str_starts(GEOID, '^34005|^34007|^34015|^34021|^42017|^42029|^42045|^42091|^42101')) %>%
  dplyr::select(-NAME) %>%
  'colnames<-'(str_replace(colnames(.), "M$", "_M")) %>%
  'colnames<-'(str_replace(colnames(.), "E$", ""))
              
               
# Combine tables
raw_data_combined <- raw_dt_data %>%
  inner_join(raw_st_data)


# Calculate percentages
estimates_table <- raw_data_combined %>%
  mutate(lep_pct = round(100 * (lep_est/lep_uni), digits = 1)) %>%
  mutate(rac_est = tot_pop - wht_est) %>% # Racial minority calculation
  mutate(rac_pct = round(100 * (rac_est/rac_uni), digits = 1)) %>%
  mutate(fem_pct = round(100 * (fem_est/fem_uni), digits = 1)) %>%
  mutate(eth_pct = round(100 * (eth_est/eth_uni), digits = 1)) %>%
  mutate(fbo_pct = round(100 * (fbo_est/fbo_uni), digits = 1)) %>%
  mutate(inc_pct = round(100 * (inc_est/inc_uni), digits = 1)) %>%
  mutate(you_pct = round(100 * (you_est/tot_pop), digits = 1)) %>%
  select(GEOID, -wht_est, -ends_with("M"), ends_with("pct"))

# Drop Low Population Tracts
low_pop_tracts <- c("34005981802","34005982200","34021980000","42017980000",
                    "42045980300","42045980000","42045980200","42091980100",
                    "42091980000","42091980200","42091980300","42101036901",
                    "42101980001","42101980002","42101980003","42101980300",
                    "42101980701","42101980702","42101980800","42101980100",
                    "42101980200", "42101980400","42101980500","42101980600",
                    "42101980901","42101980902","42101980903","42101980904",
                    "42101980905","42101980906", "42101989100","42101989200",
                    "42101989300")

estimates_table_clean <- estimates_table %>%
  filter(!GEOID %in% low_pop_tracts)

# Define Test Table
test_table <- estimates_table_clean

# Variables
vars <- list("lep_pct", "dis_pct", "old_pct", "rac_pct", "fem_pct", "eth_pct", "fbo_pct", "inc_pct", "you_pct")


# Function to calculate score
calculate_score <- function(data, var) {
  means <- mean(data[[var]], na.rm = TRUE)
  stdev <- sd(data[[var]], na.rm = TRUE)
  score_col <- paste0(var, "_score")
  data <- data %>%
    mutate(!!score_col := case_when(
      data[[var]] < ifelse(means - (1.5 * stdev) < 0, 0.1, means - (1.5 * stdev)) ~ 0,
      data[[var]] >= means - (1.5 * stdev) & data[[var]] < means - (0.5 * stdev) ~ 1,
      data[[var]] >= means - (0.5 * stdev) & data[[var]] < means + (0.5 * stdev) ~ 2,
      data[[var]] >= means + (0.5 * stdev) & data[[var]] < means + (1.5 * stdev) ~ 3,
      data[[var]] >= means + (1.5 * stdev) ~ 4
    ))
  return(data)
}

# Applying the function to each variable
for (var in vars) {
  test_table <- calculate_score(test_table, var)
}

# Add the total score column
test_table$ipd_score <- rowSums(select(test_table, ends_with("_score")), na.rm = TRUE)

# Export CSV
write.csv(test_table, "G:\\My Drive\\ipd_review\\test_table.csv")