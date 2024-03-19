## SETUP
# Dependencies
library(plyr); library(here); library(sf); library(summarytools);
library(tidycensus); library(tidyverse); library(tigris); library(dplyr); library(descr)

# Census API Key
readRenviron(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/.Renviron"))
census_api_key <- Sys.getenv("CENSUS_API_KEY")

# Inputs and settings
ipd_year <- 2022
ipd_states <- c("NJ", "PA")
dvrpc_counties <- c('^34005|^34007|^34015|^34021|^42017|^42029|^42045|^42091|^42101')
ipd_counties <- c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")
output_dir <- "data\\"

# Fields
# See https://www.census.gov/data/developers/data-sets/acs-5year.html
# for the variables for Detailed Tables (B), Subject Tables (S), and Data Profiles (DP)

acs5_dt_list <- c(
  tot_pp = "B01003_001", # Total Population
  em_uni = "B03002_001", # Ethnic Minority
  em_est = "B03002_012",
  fb_uni = "B05012_001", # Foreign-born
  fb_est = "B05012_003",
  rm_uni = "B02001_001", # Racial minority
  blk_est = "B02001_003", # Black or African American alone
  aia_est = "B02001_004", # American Indian and Alaska Native alone
  asn_est = "B02001_005", # Asian alone
  hpi_est = "B02001_006", # Native Hawaiian and Other Pacific Islander alone
  oth_est = "B02001_007", # Some other race alone
  two_est = "B02001_008", # Two or more races
  y_est = "B09001_001" # Youth
)

acs5_st_list <- c(
  le_uni = "S1601_C01_001", # Limited English Proficiency
  le_est = "S1601_C05_001",
  le_pct = "S1601_C06_001",
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
  'colnames<-'(str_replace(colnames(.), "M$", "_moe"))

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
  'colnames<-'(str_replace(colnames(.), "M$", "_moe"))

raw_dp_data <- get_acs(geography = "tract",
                       variables = acs5_dp_list,
                       year = ipd_year,
                       state = ipd_states,
                       survey = "acs5",
                       output = "wide"
) %>%
  filter(str_detect(GEOID, dvrpc_counties)) %>%
  dplyr::select(-NAME) %>%
  'colnames<-'(str_replace(colnames(.), "E$", "")) %>%
  'colnames<-'(str_replace(colnames(.), "M$", "_moe"))

# Combine tables
raw_data_combined <- raw_dt_data %>%
  inner_join(raw_st_data) %>%
  inner_join(raw_dp_data)

# Calculate percentages and MOEs, Drop Unnecessesary MOEs
estimates_table <- raw_data_combined %>%
  mutate(rm_est = blk_est + aia_est + asn_est + hpi_est + oth_est + two_est) %>% # Racial minority calculation
  select(-blk_est, -aia_est, -asn_est, -hpi_est, -oth_est, -two_est, -blk_est_moe, -aia_est_moe, -asn_est_moe, -hpi_est_moe, -oth_est_moe, -two_est_moe) %>%
  mutate(rm_pct = round(100 * (rm_est/rm_uni), digits = 1)) %>%
  mutate(em_pct = round(100 * (em_est/em_uni), digits = 1)) %>%
  mutate(fb_pct = round(100 * (fb_est/fb_uni), digits = 1)) %>%
  mutate(li_pct = round(100 * (li_est/li_uni), digits = 1)) %>%
  mutate(y_pct = round(100 * (y_est/tot_pp), digits = 1)) %>%
  mutate(em_pct_moe = round(moe_prop(em_est,em_uni,em_est_moe,em_uni_moe) * 100,1)) %>%
  mutate(fb_pct_moe = round(moe_prop(fb_est,fb_uni,fb_est_moe,fb_uni_moe) * 100,1)) %>%
  mutate(li_pct_moe = round(moe_prop(li_est,li_uni,li_est_moe,li_uni_moe) * 100,1)) %>%
  mutate(y_pct_moe = round(moe_prop(y_est,tot_pp,y_est_moe,tot_pp_moe) * 100,1))

# Use variance replicates to calc MOE for RM indicator
ipd_states_numeric <- fips_codes %>%
  filter(state %in% ipd_states) %>%
  select(state_code) %>% distinct(.) %>% pull(.)
var_rep <- NULL

for (i in 1:length(ipd_states)){
  url <- paste0("https://www2.census.gov/programs-surveys/acs/replicate_estimates/",
                ipd_year,
                "/data/5-year/140/B02001_",
                ipd_states_numeric[i],
                ".csv.zip")
  temp <- tempfile()
  download.file(url, temp)
  var_rep_i <- read.csv(unzip(temp))
  var_rep <- dplyr::bind_rows(var_rep, var_rep_i)
} 

# function to calculate sqdiff
sqdiff_fn <- function(v, e) (v - e) ^ 2

var_rep <- var_rep %>%
  mutate_at(vars(GEOID), ~(str_sub(., 10, 20))) %>%
  filter(str_sub(GEOID, 1, 5) %in% ipd_counties) %>%
  select(-TBLID, -NAME, -ORDER, -MOE, -CME, -SE) %>%
  filter(TITLE %in% c("Black or African American alone",
                      "American Indian and Alaska Native alone",
                      "Asian alone",
                      "Native Hawaiian and Other Pacific Islander alone",
                      "Some other race alone",
                      "Two or more races:")) %>%
  group_by(GEOID) %>%
  summarize_if(is.numeric, ~ sum(.))

ids <- var_rep %>% select(GEOID) %>% pull(.)
rep_estimates <- var_rep %>% select(ESTIMATE)
replicates <- var_rep %>% select(-GEOID, -ESTIMATE)

sqdiff <- mapply(sqdiff_fn, replicates, rep_estimates)
sum_sqdiff <- rowSums(sqdiff, dims=1)
moe <- round(sqrt(0.05 * sum_sqdiff) * 1.645, 0) #sqrt(variance) * 1.645
rm_moe <- cbind(ids, moe) %>%
  as_tibble(.) %>%
  rename(GEOID = ids, rm_est_moe = moe) %>%
  mutate_at(vars(rm_est_moe), as.numeric)

estimates_table <- estimates_table %>%
  left_join(., rm_moe) %>%
  mutate(rm_pct_moe = round(moe_prop(rm_est,rm_uni,rm_est_moe,rm_uni_moe) * 100,1))


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
  select(-matches("_uni")) %>%
  filter(!GEOID %in% low_pop_tracts)


# IPD Score ----

# Define Test Table
test_table <- estimates_table_clean


# Variables
vars <- list("le_pct", "d_pct", "oa_pct", "rm_pct", "f_pct", "em_pct", "fb_pct", "li_pct", "y_pct")


# Function to calculate indicator percentile and score
calculate_score <- function(data, var) {
  means <- mean(data[[var]], na.rm = TRUE)
  stdev <- sd(data[[var]], na.rm = TRUE)
  score_col <- paste0(var, "_score")
  class_col <- paste0(var, "_class")
  pctile_col <- paste0(var, "_pctile")
  data <- data %>%
    mutate(!!score_col := case_when(
      data[[var]] < ifelse(means - (1.5 * stdev) < 0, 0.1, means - (1.5 * stdev)) ~ 0,
      data[[var]] >= means - (1.5 * stdev) & data[[var]] < means - (0.5 * stdev) ~ 1,
      data[[var]] >= means - (0.5 * stdev) & data[[var]] < means + (0.5 * stdev) ~ 2,
      data[[var]] >= means + (0.5 * stdev) & data[[var]] < means + (1.5 * stdev) ~ 3,
      data[[var]] >= means + (1.5 * stdev) ~ 4
    )) %>%
    mutate(!!class_col := case_when(
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
  'colnames<-'(str_replace(colnames(.), "pct_score", "score")) %>%
  'colnames<-'(str_replace(colnames(.), "pct_class", "class")) %>%
  'colnames<-'(str_replace(colnames(.), "pct_pctile", "pctile")) %>%
  select(GEOID, sort(colnames(.))) %>%
  relocate(ipd_score, .after=y_score)


# Spatial Data ----
# Geography columns
pa_tracts <- tracts("42", c("017", "029", "045", "091", "101"))
nj_tracts <- tracts("34", c("005", "007", "015", "021"))

region_tracts <- rbind(pa_tracts, nj_tracts) %>%
  st_transform(., 26918)

ipd_shapefile <- region_tracts %>%
  left_join(ipd_table, by=c("GEOID"="GEOID")) %>%
  select(-MTFCC, -FUNCSTAT, -ALAND, -AWATER, -INTPTLAT, -INTPTLON) %>%
  rename(geoid20 = GEOID) %>%
  'colnames<-'(tolower(colnames(.))) %>%
  mutate(year = ipd_year) %>%
  select(year, geoid20, everything())

# Import Tract to MCD Lookup
tract_mcd_lookup <- st_read("U:\\_OngoingProjects\\Census\\_Geographies\\Census_Boundaries_2020.gdb", layer="TractToMCD_Lookup20") %>%
  select(geoid20, mun1, mun2, mun3, mcdgeo1, mcdgeo2, mcdgeo3)

# Join IPD table with Lookup
ipd_shapefile <- ipd_shapefile %>%
  left_join(tract_mcd_lookup)

ipd_table <- st_drop_geometry(ipd_shapefile) 


# Summary Tables ----
# Counts
counts <- ipd_table %>% select(ends_with("_class"))

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
                                                  "NA"))

export_counts <- arrange(export_counts, Variable, Classification)

counts_table <- export_counts %>%
  spread(Classification, Count) %>%
  mutate(TOTAL = rowSums(.[2:7], na.rm = TRUE))

# Breaks
breaks_table <- ipd_table  %>%
  select(ends_with("_pct"))

calculate_class_breaks <- function(input_df) {
  breaks_df <- data.frame(matrix(NA, nrow = 6, ncol = ncol(input_df) + 1))
  colnames(breaks_df) <- c("Break", colnames(input_df))
  
  breaks_df$Break <- c("Min", "1", "2", "3", "4", "Max")
  
  for (i in 1:ncol(input_df)) {
    x <- input_df[[i]]
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
    
    min_break <- 0
    b1 <- round(mean_x - (1.5 * sd_x), 1)
    if (b1 < 0) {
      b1 <- 0.1
    }    
    b2 <- round(mean_x - (0.5 * sd_x), 1)
    b3 <- round(mean_x + (0.5 * sd_x), 1)
    b4 <- round(mean_x + (1.5 * sd_x), 1)
    max_break <- round(max(x, na.rm = TRUE), 1)
    
    breaks_df[, i + 1] <- c(min_break, b1, b2, b3, b4, max_break)
  }
  
  return(breaks_df)
}


class_breaks_table <- calculate_class_breaks(breaks_table)

# Summary Statistics
description <- function(i) {
  des <- as.numeric(summarytools::descr(i, na.rm = TRUE,
                                        stats = c("min", "med", "mean", "sd", "max")))
  des <- c(des[1:4], des[4] / 2, des[5])
  return(des)
}

pcts <- ipd_table %>% select(ends_with("_pct"))

round_1 <- function(i) round(i, 1)
round_2 <- function(i) round(i, 2)

summary_data <- apply(pcts, MARGIN=2, description)

summary_table <- as_tibble(summary_data) %>%
  mutate_all(round_2) %>%
  mutate(Statistic = c("Minimum", "Median", "Mean", "SD", "Half-SD", "Maximum")) %>%
  select(Statistic, tidyselect::peek_vars())

# County-level Means
means_table <- estimates_table %>%
  mutate(county_fips = str_sub(GEOID, 1, 5)) %>%
  select(-GEOID, tot_pp, ends_with("_est"), ends_with("_uni"), -matches("moe"), -year) %>%
  group_by(county_fips) %>%
  summarise(
    d_pctest = sum(d_est)/sum(d_uni),
    em_pctest = sum(em_est)/sum(em_uni),
    f_pctest = sum(f_est)/sum(f_uni),
    fb_pctest = sum(fb_est)/sum(fb_uni),
    le_pctest = sum(le_est)/sum(le_uni),
    li_pctest = sum(li_est)/sum(li_uni),
    oa_pctest = sum(oa_est)/sum(tot_pp),
    rm_pctest = sum(rm_est)/sum(rm_uni),
    y_pctest = sum(y_est)/sum(tot_pp)
  ) %>%
  mutate_if(is.numeric, ~ . * 100) %>%
  mutate_if(is.numeric, round_1)

# Export Data ----
## Tract-Level IPD Outputs

write.csv(ipd_table, paste(output_dir,"ipd_", ipd_year, ".csv", sep=""))
st_write(ipd_shapefile, paste(output_dir,"ipd_", ipd_year, ".shp", sep="")) 

## Summary Tables

write.csv(counts_table, paste(output_dir,"counts_by_indicator_", ipd_year, ".csv", sep=""))
write.csv(class_breaks_table, paste(output_dir,"breaks_by_indicator_", ipd_year, ".csv", sep=""))
write.csv(summary_table, paste(output_dir,"summary_by_indicator_", ipd_year, ".csv", sep=""))
write.csv(means_table, paste(output_dir,"means_by_county_", ipd_year, ".csv", sep=""))

