## SETUP
# Dependencies
library(plyr); library(here); library(sf); library(summarytools);
library(tidycensus); library(tidyverse); library(tigris); library(dplyr); library(descr)

# Census API Key
census_api_key <- Sys.getenv("CENSUS_API_KEY")

# Fields
# See https://www.census.gov/data/developers/data-sets/acs-5year.html
# for the variables for Detailed Tables (B), Subject Tables (S), and Data Profiles (DP)

acs5_dt_list <- c(
  tot_pop = "B01003_001", # Total Population
  em_uni = "B03002_001", # Ethnic Minority
  em_est = "B03002_012",
  fb_uni = "B05012_001", # Foreign-born
  fb_est = "B05012_003",
  rm_uni = "B02001_001", # Racial minority
  wht_est = "B02001_002", # White Alone
  blk_est = "B02001_003", # Black or African American alone
  aia_est = "B02001_004", # American Indian and Alaska Native alone
  asn_est = "B02001_005", # Asian alone
  hpi_est = "B02001_006", # Native Hawaiian and Other Pacific Islander alone
  oth_est = "B02001_007", # Some other race alone
  two_est = "B02001_008", # Two or more races
  y_est = "B09001_001" # Youth
)

acs5_st_list <- c(
  lep_uni = "S1601_C01_001", # Limited English Proficiency
  lep_est = "S1601_C05_001",
  lep_pct = "S1601_C06_001",
  d_uni = "S1810_C01_001",  # Disabled
  d_est = "S1810_C02_001",  
  d_pct = "S1810_C03_001", 
  f_uni = "S0101_C01_001",
  f_est = "S0101_C05_001", # Female
  li_uni = "S1701_C01_001", # Low Income
  li_est = "S1701_C01_042",
  oa_uni = "S0101_C01_001", # Older Population
  oa_est = "S0101_C01_030",
  oa_pct = "S0101_C02_030"
)

acs5_dp_list <- c(
  f_pct = "DP05_0003P"
)


ipd_year <- 2021
ipd_states <- c("NJ", "PA")
dvrpc_counties <- c('^34005|^34007|^34015|^34021|^42017|^42029|^42045|^42091|^42101')
ipd_counties <- c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")

raw_dt_data <- get_acs(geography = "tract",
                       variables = acs5_dt_list,
                       year = ipd_year,
                       state = ipd_states,
                       survey = "acs5",
                       output = "wide"
) %>%
  mutate(year = ipd_year) %>%
  filter(str_detect(GEOID, dvrpc_counties)) %>%
  dplyr::select(-NAME) %>%
  'colnames<-'(str_replace(colnames(.), "E$", "")) %>%
  'colnames<-'(str_replace(colnames(.), "M$", "_MOE"))
  
raw_st_data <- get_acs(geography = "tract",
                       variables = acs5_st_list,
                       year = ipd_year,
                       state = ipd_states,
                       survey = "acs5",
                       output = "wide"
) %>%
  mutate(year = ipd_year) %>%
  filter(str_detect(GEOID, dvrpc_counties)) %>%
  dplyr::select(-NAME) %>%
  'colnames<-'(str_replace(colnames(.), "E$", "")) %>%
  'colnames<-'(str_replace(colnames(.), "M$", "_MOE"))

raw_dp_data <- get_acs(geography = "tract",
                       variables = acs5_dp_list,
                       year = ipd_year,
                       state = ipd_states,
                       survey = "acs5",
                       output = "wide"
) %>%
  mutate(year = ipd_year) %>%
  filter(str_detect(GEOID, dvrpc_counties)) %>%
  dplyr::select(-NAME) %>%
  'colnames<-'(str_replace(colnames(.), "E$", "")) %>%
  'colnames<-'(str_replace(colnames(.), "M$", "_MOE"))

# Combine tables
raw_data_combined <- raw_dt_data %>%
  inner_join(raw_st_data) %>%
  inner_join(raw_dp_data)

# Calculate percentages and MOEs, Drop Unnecessesary MOEs
estimates_table <- raw_data_combined %>%
  mutate(rac_est = blk_est + aia_est + asn_est + hpi_est + oth_est + two_est) %>% # Racial minority calculation
  mutate(rm_est_MOE = round(sqrt(blk_est_MOE^2 + aia_est_MOE^2 + asn_est_MOE^2 + hpi_est_MOE^2 + oth_est_MOE^2 + two_est_MOE^2), 0)) %>%
  select(-blk_est, -aia_est, -asn_est, -hpi_est, -oth_est, -two_est, -blk_est_MOE, -aia_est_MOE, -asn_est_MOE, -hpi_est_MOE, -oth_est_MOE, -two_est_MOE) %>%
  mutate(rm_pct = round(100 * (rm_est/rm_uni), digits = 1)) %>%
  mutate(em_pct = round(100 * (em_est/em_uni), digits = 1)) %>%
  mutate(fb_pct = round(100 * (fb_est/fb_uni), digits = 1)) %>%
  mutate(li_pct = round(100 * (li_est/li_uni), digits = 1)) %>%
  mutate(y_pct = round(100 * (y_est/tot_pop), digits = 1)) %>%
  select(-wht_est, -wht_est_MOE) %>%
  mutate(rm_pct_MOE = round((sqrt(rm_est_MOE^2 + (rm_pct^2 * tot_pop_MOE^2))/rm_uni), 1)) %>%
  mutate(y_pct_MOE = round((sqrt(y_est_MOE^2 + (y_pct^2 * tot_pop_MOE^2)))/tot_pop, 1)) %>%
  mutate(em_pct_MOE = round((sqrt(em_est_MOE^2 + (em_pct^2 * em_uni_MOE^2)))/em_uni, 1)) %>%
  mutate(fb_pct_MOE = round((sqrt(fb_est_MOE^2 + (fb_pct^2 * fb_uni_MOE^2)))/fb_uni, 1)) %>%
  mutate(li_pct_MOE = round((sqrt(li_est_MOE^2 + (li_pct^2 * li_uni_MOE^2)))/li_uni, 1)) %>%
  select(-matches("uni"))
  

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


# IPD Score ----

# Define Test Table
test_table <- estimates_table_clean


# Variables
vars <- list("lep_pct", "d_pct", "oa_pct", "rm_pct", "f_pct", "em_pct", "fb_pct", "li_pct", "y_pct")


# Function to calculate indicator percentile and score
calculate_score <- function(data, var) {
  means <- mean(data[[var]], na.rm = TRUE)
  stdev <- sd(data[[var]], na.rm = TRUE)
  score_col <- paste0(var, "_score")
  cat_col <- paste0(var, "_cat")
  pctile_col <- paste0(var, "_pctile")
  data <- data %>%
    mutate(!!score_col := case_when(
      data[[var]] < ifelse(means - (1.5 * stdev) < 0, 0.1, means - (1.5 * stdev)) ~ 0,
      data[[var]] >= means - (1.5 * stdev) & data[[var]] < means - (0.5 * stdev) ~ 1,
      data[[var]] >= means - (0.5 * stdev) & data[[var]] < means + (0.5 * stdev) ~ 2,
      data[[var]] >= means + (0.5 * stdev) & data[[var]] < means + (1.5 * stdev) ~ 3,
      data[[var]] >= means + (1.5 * stdev) ~ 4
    )) %>%
    mutate(!!cat_col := case_when(
      data[[var]] < ifelse(means - (1.5 * stdev) < 0, 0.1, means - (1.5 * stdev)) ~ "Well Below Average",
      data[[var]] >= means - (1.5 * stdev) & data[[var]] < means - (0.5 * stdev) ~ "Below Average",
      data[[var]] >= means - (0.5 * stdev) & data[[var]] < means + (0.5 * stdev) ~ "Average",
      data[[var]] >= means + (0.5 * stdev) & data[[var]] < means + (1.5 * stdev) ~ "Above Average",
      data[[var]] >= means + (1.5 * stdev) ~ "Well Above Average"
    )) %>%
    mutate(!!pctile_col := round(percent_rank(data[[var]]), 2))
  return(data)
}

# Applying the function to each variable
for (var in vars) {
  test_table <- calculate_score(test_table, var)
}

# Calculate Total IPD Score
test_table$ipd_score <- rowSums(select(test_table, ends_with("_score")), na.rm = TRUE)


# Join table with all census tracts
tracts <- estimates_table %>%
  select(GEOID)

ipd_table <- tracts %>%
  left_join(test_table) %>%
  'colnames<-'(str_replace(colnames(.), "pct_score", "score"))

# Geography columns
ipd_table <- ipd_table %>%
  rename(GEOID20 = GEOID) %>%
  mutate(STATEFP20 = str_sub(GEOID20, 1, 2)) %>%
  mutate(COUNTYFP20 = str_sub(GEOID20, 3, 5)) %>%
  mutate(NAME20 = str_sub(GEOID20, 6, 11)) %>%
  mutate(namelsad = paste(substr(GEOID20, 6, 9), substr(GEOID20, 10, 11), sep = "."))

# Spatial
pa_tracts <- tracts("42", c("017", "029", "045", "091", "101"))
nj_tracts <- tracts("34", c("005", "007", "015", "021"))

region_tracts <- rbind(pa_tracts, nj_tracts) %>%
  st_transform(., 26918)

ipd_shapefile <- region_tracts %>%
  left_join(ipd_table, by=c("GEOID"="GEOID20"))

# Import Tract to MCD Lookup
tract_mcd_lookup <- st_read("U:\\_OngoingProjects\\Census\\_Geographies\\Census_Boundaries_2020.gdb", layer="TractToMCD_Lookup20") %>%
  select(geoid20, mun1, mun2, mun3, mcdgeo1, mcdgeo2, mcdgeo3)

# Join IPD table with Lookup
ipd_shapefile <- ipd_shapefile %>%
  left_join(tract_mcd_lookup, by=c("GEOID"="geoid20"))


# Summary Tables ----
