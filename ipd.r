## ---------------------------------------------------------------------------
## SETUP
## ---------------------------------------------------------------------------

# Dependencies
library(plyr)
library(here)
library(sf)
library(summarytools)
library(tidycensus)
library(tidyverse)
library(tigris)
library(dplyr)
library(descr)
library(terra)
library(gpkg)

# Load Census API Key
readRenviron(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/.Renviron"))
census_api_key <- Sys.getenv("CENSUS_API_KEY")

# Inputs and settings
ipd_year <- 2024
ipd_states <- c("NJ", "PA")
dvrpc_counties <- c("^34005|^34007|^34015|^34021|^42017|^42029|^42045|^42091|^42101")
ipd_counties <- c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")

county_names <- data.frame(
  ipd_counties,
  co_name = c(
    "Burlington", "Camden", "Gloucester", "Mercer",
    "Bucks", "Chester", "Delaware", "Montgomery", "Philadelphia"
  ),
  state = c("NJ", "NJ", "NJ", "NJ", "PA", "PA", "PA", "PA", "PA")
)

output_dir <- "data"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

## ---------------------------------------------------------------------------
## VARIABLES
## ---------------------------------------------------------------------------

# See: https://www.census.gov/data/developers/data-sets/acs-5year.html

acs5_dt_list <- c(
  tot_pop = "B01003_001",  # Total Population
  em_uni = "B03002_001",  # Ethnic Minority
  em_est = "B03002_012",
  fb_uni = "B05012_001",  # Foreign-born
  fb_est = "B05012_003",
  rm_uni = "B02001_001",  # Racial minority universe
  blk_est = "B02001_003",
  aia_est = "B02001_004",
  asn_est = "B02001_005",
  hpi_est = "B02001_006",
  oth_est = "B02001_007",
  two_est = "B02001_008",
  y_est  = "B09001_001"   # Youth
)

acs5_st_list <- c(
  le_uni = "S1601_C01_001", le_est = "S1601_C05_001", le_pct = "S1601_C06_001",  # Limited English
  d_uni  = "S1810_C01_001", d_est = "S1810_C02_001", d_pct = "S1810_C03_001",   # Disabled
  f_uni  = "S0101_C01_001", f_est = "S0101_C05_001",                            # Female
  li_uni = "S1701_C01_001", li_est = "S1701_C01_042",                           # Low Income
  oa_uni = "S0101_C01_001", oa_est = "S0101_C01_030", oa_pct = "S0101_C02_030"  # Older Adult
)

acs5_dp_list <- c(f_pct = "DP05_0003P")

## ---------------------------------------------------------------------------
## DOWNLOAD + CLEAN ACS DATA
## ---------------------------------------------------------------------------

get_clean_acs <- function(vars) {
  get_acs(
    geography = "tract",
    variables = vars,
    year = ipd_year,
    state = ipd_states,
    survey = "acs5",
    output = "wide"
  ) %>%
    mutate(year = ipd_year) %>%
    filter(str_detect(GEOID, dvrpc_counties)) %>%
    select(-NAME) %>%
    setNames(str_replace(names(.), "E$", "")) %>%
    setNames(str_replace(names(.), "M$", "_moe"))
}

raw_dt_data <- get_clean_acs(acs5_dt_list)
raw_st_data <- get_clean_acs(acs5_st_list)
raw_dp_data <- get_clean_acs(acs5_dp_list)

## ---------------------------------------------------------------------------
## COMBINE & CALCULATE PERCENTAGES
## ---------------------------------------------------------------------------

raw_data_combined <- raw_dt_data %>%
  inner_join(raw_st_data) %>%
  inner_join(raw_dp_data)


estimates_table <- raw_data_combined %>%
  mutate(
    rm_est = blk_est + aia_est + asn_est + hpi_est + oth_est + two_est
  ) %>%
  rowwise() %>%
  mutate(
    rm_est_moe = moe_sum(
      c_across(c(
        blk_est_moe,
        aia_est_moe,
        asn_est_moe,
        hpi_est_moe,
        oth_est_moe,
        two_est_moe
      )),
      estimate = c_across(c(
        blk_est,
        aia_est,
        asn_est,
        hpi_est,
        oth_est,
        two_est
      ))
    )
  ) %>%
  ungroup() %>%
  select(-blk_est, -aia_est, -asn_est, -hpi_est, -oth_est, -two_est, 
         -blk_est_moe, -aia_est_moe, -asn_est_moe, -hpi_est_moe, -oth_est_moe, -two_est_moe) %>%
  mutate(
    rm_pct = round(100 * (rm_est / rm_uni), digits = 1),
    em_pct = round(100 * (em_est / em_uni), digits = 1),
    fb_pct = round(100 * (fb_est / fb_uni), digits = 1),
    li_pct = round(100 * (li_est / li_uni), digits = 1),
    y_pct = round(100 * (y_est / tot_pop), digits = 1),
    em_pct_moe = round(moe_prop(em_est, em_uni, em_est_moe, em_uni_moe) * 100, 1),
    fb_pct_moe = round(moe_prop(fb_est, fb_uni, fb_est_moe, fb_uni_moe) * 100, 1),
    li_pct_moe = round(moe_prop(li_est, li_uni, li_est_moe, li_uni_moe) * 100, 1),
    y_pct_moe = round(moe_prop(y_est, tot_pop, y_est_moe, tot_pop_moe) * 100, 1)
  )

## ---------------------------------------------------------------------------
## VARIANCE REPLICATES FOR RM
## ---------------------------------------------------------------------------

ipd_states_numeric <- fips_codes %>%
  filter(state %in% ipd_states) %>%
  distinct(state_code) %>%
  pull()

var_rep <- NULL

for (i in seq_along(ipd_states)) {
  url <- paste0(
    "https://www2.census.gov/programs-surveys/acs/replicate_estimates/",
    ipd_year, "/data/5-year/140/B02001_",
    ipd_states_numeric[i], ".csv.zip"
  )

  temp <- tempfile()
  download.file(url, temp, quiet = TRUE)

  if (file.exists(temp)) {
    var_rep_i <- tryCatch(
      {
        read.csv(unzip(temp))
      },
      error = function(e) NULL  # Skip if file cannot be read
    )

    if (!is.null(var_rep_i)) {
      var_rep <- dplyr::bind_rows(var_rep, var_rep_i)
    }
  }
}

if (!is.null(var_rep)) {
  # Function to calculate squared differences
  sqdiff_fn <- function(v, e) (v - e)^2

  var_rep <- var_rep %>%
    mutate(GEOID = str_sub(GEOID, 10, 20)) %>%
    filter(str_sub(GEOID, 1, 5) %in% ipd_counties) %>%
    select(-TBLID, -NAME, -ORDER, -MOE, -CME, -SE) %>%
    filter(TITLE %in% c(
      "Black or African American alone",
      "American Indian and Alaska Native alone",
      "Asian alone",
      "Native Hawaiian and Other Pacific Islander alone",
      "Some other race alone",
      "Two or more races:"
    )) %>%
    group_by(GEOID) %>%
    summarize(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

  ids <- var_rep$GEOID
  rep_estimates <- var_rep$ESTIMATE
  replicates <- var_rep %>% select(-GEOID, -ESTIMATE)

  # Compute squared differences across all replicates
  sqdiff <- mapply(sqdiff_fn, replicates, rep_estimates)
  sum_sqdiff <- rowSums(sqdiff, dims = 1)

  # Compute MOE using replicate variance method
  rm_moe <- tibble(
    GEOID = ids,
    rm_est_moe_adv = round(sqrt(0.05 * sum_sqdiff) * 1.645, 0)
  ) %>%
    mutate(across(rm_est_moe_adv, as.numeric))

  estimates_table <- estimates_table %>%
    left_join(rm_moe, by = "GEOID") %>%
    mutate(
      rm_est_moe = coalesce(rm_est_moe_adv, rm_est_moe)
    ) %>%
    select(-rm_est_moe_adv)
}

# Compute RM percentage MOE
estimates_table <- estimates_table %>%
  mutate(
    rm_pct_moe = round(
      moe_prop(rm_est, rm_uni, rm_est_moe, rm_uni_moe) * 100, 1
    )
  )


## ---------------------------------------------------------------------------
## FILTER LOW POPULATION TRACTS
## ---------------------------------------------------------------------------

low_pop_tracts <- c(
  "34005981802", "34005982200", "34021980000", "42017980000",
  "42045980300", "42045980000", "42045980200", "42091980100",
  "42091980000", "42091980200", "42091980300", "42101036901",
  "42101980001", "42101980002", "42101980003", "42101980300",
  "42101980701", "42101980702", "42101980800", "42101980100",
  "42101980200", "42101980400", "42101980500", "42101980600",
  "42101980901", "42101980902", "42101980903", "42101980904",
  "42101980905", "42101980906", "42101989100", "42101989200",
  "42101989300"
)

estimates_table_clean <- estimates_table %>%
  select(-matches("_uni")) %>%
  filter(!GEOID %in% low_pop_tracts)

## ---------------------------------------------------------------------------
## SCORE CALCULATIONS
## ---------------------------------------------------------------------------

calculate_score <- function(data, var) {
  mean_val <- mean(data[[var]], na.rm = TRUE)
  sd_val <- sd(data[[var]], na.rm = TRUE)

  data %>%
    mutate(
      !!paste0(var, "_score") := case_when(
        data[[var]] < ifelse(mean_val - (1.5 * sd_val) < 0, 0.1, mean_val - (1.5 * sd_val)) ~ 0,
        data[[var]] < mean_val - (0.5 * sd_val) ~ 1,
        data[[var]] < mean_val + (0.5 * sd_val) ~ 2,
        data[[var]] < mean_val + (1.5 * sd_val) ~ 3,
        TRUE ~ 4
      ),
      !!paste0(var, "_class") := case_when(
        data[[var]] < ifelse(mean_val - (1.5 * sd_val) < 0, 0.1, mean_val - (1.5 * sd_val)) ~ "Well Below Average",
        data[[var]] < mean_val - (0.5 * sd_val) ~ "Below Average",
        data[[var]] < mean_val + (0.5 * sd_val) ~ "Average",
        data[[var]] < mean_val + (1.5 * sd_val) ~ "Above Average",
        TRUE ~ "Well Above Average"
      ),
      !!paste0(var, "_pctile") := round(percent_rank(data[[var]]), 2)
    )
}

vars <- c("le_pct", "d_pct", "oa_pct", "rm_pct", "f_pct", "em_pct", "fb_pct", "li_pct", "y_pct")

test_table <- estimates_table_clean
for (v in vars) {
  test_table <- calculate_score(test_table, v)
}

test_table <- test_table %>%
  mutate(
    ipd_score = rowSums(select(., ends_with("_score")), na.rm = TRUE),
    t6_score  = pmax(rm_pct_score, em_pct_score, na.rm = TRUE),
    t6_class  = case_when(
      t6_score == 0 ~ "Well Below Average",
      t6_score == 1 ~ "Below Average",
      t6_score == 2 ~ "Average",
      t6_score == 3 ~ "Above Average",
      t6_score == 4 ~ "Well Above Average",
      TRUE ~ NA_character_
    )
  )

## ---------------------------------------------------------------------------
## JOIN TABLES & GEOMETRY
## ---------------------------------------------------------------------------

tracts <- estimates_table %>% select(GEOID)

ipd_table <- tracts %>%
  mutate(county_fips = str_sub(GEOID, 1, 5)) %>%
  left_join(county_names, by = c("county_fips" = "ipd_counties")) %>%
  left_join(test_table, by = "GEOID") %>%
  rename_with(~ str_replace(., "pct_score", "score")) %>%
  rename_with(~ str_replace(., "pct_class", "class")) %>%
  rename_with(~ str_replace(., "pct_pctile", "pctile")) %>%
  select(GEOID, sort(names(.))) %>%
  relocate(ipd_score, .after = y_score)

# Spatial data
pa_tracts <- tracts("42", c("017", "029", "045", "091", "101"))
nj_tracts <- tracts("34", c("005", "007", "015", "021"))

region_tracts <- bind_rows(pa_tracts, nj_tracts) %>% st_transform(26918)

ipd_shapefile <- region_tracts %>%
  left_join(ipd_table, by = "GEOID") %>%
  select(-STATEFP, -COUNTYFP, -TRACTCE, -NAMELSAD, -MTFCC, -FUNCSTAT,
         -ALAND, -AWATER, -INTPTLAT, -INTPTLON) %>%
  rename(geoid20 = GEOID) %>%
  rename_with(tolower) %>%
  mutate(year = ipd_year) %>%
  select(year, geoid20, everything())

# Tract-to-MCD lookup
lookup_url <- "https://arcgis.dvrpc.org/portal/rest/services/demographics/tracttomcd_lookup/FeatureServer/0/query?where=1=1&outfields=*&f=json"
lookup_data <- jsonlite::fromJSON(lookup_url)
tract_mcd_lookup <- as.data.frame(lookup_data$features$attributes) %>%
  select(geoid20, mun1, mun2, mun3, mcdgeo1, mcdgeo2, mcdgeo3)

ipd_shapefile <- left_join(ipd_shapefile, tract_mcd_lookup, by = "geoid20")
ipd_table <- st_drop_geometry(ipd_shapefile)

## ---------------------------------------------------------------------------
## SUMMARY TABLES
## ---------------------------------------------------------------------------

# 1) Counts by classification
counts_table <- ipd_table %>%
  select(ends_with("_class")) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Classification") %>%
  mutate(Classification = replace_na(Classification, "NA")) %>%
  mutate(Classification = factor(
    Classification,
    levels = c(
      "Well Below Average", "Below Average", "Average",
      "Above Average", "Well Above Average", "NA"
    )
  )) %>%
  count(Variable, Classification, name = "Count") %>%
  arrange(Variable, Classification) %>%
  pivot_wider(names_from = Classification, values_from = Count, values_fill = 0) %>%
  mutate(TOTAL = rowSums(across(where(is.numeric)), na.rm = TRUE))

# 2) Breaks (class boundaries)
breaks_table_input <- ipd_table %>% select(ends_with("_pct"))

calculate_class_breaks <- function(input_df) {
  breaks_df <- data.frame(matrix(NA, nrow = 6, ncol = ncol(input_df) + 1))
  colnames(breaks_df) <- c("Break", colnames(input_df))
  breaks_df$Break <- c("Min", "1", "2", "3", "4", "Max")

  for (i in seq_len(ncol(input_df))) {
    x <- input_df[[i]]
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)

    min_break <- 0
    b1 <- round(mean_x - (1.5 * sd_x), 1); if (b1 < 0) b1 <- 0.1
    b2 <- round(mean_x - (0.5 * sd_x), 1)
    b3 <- round(mean_x + (0.5 * sd_x), 1)
    b4 <- round(mean_x + (1.5 * sd_x), 1)
    max_break <- round(max(x, na.rm = TRUE), 1)

    breaks_df[, i + 1] <- c(min_break, b1, b2, b3, b4, max_break)
  }
  breaks_df
}

class_breaks_table <- calculate_class_breaks(breaks_table_input)

# 3) Summary statistics (per indicator pct)
description <- function(i) {
  des <- as.numeric(summarytools::descr(i, na.rm = TRUE, stats = c("min", "med", "mean", "sd", "max")))
  des <- c(des[1:4], des[4] / 2, des[5])  # add Half-SD between SD and Max
  des
}

pcts <- ipd_table %>% select(ends_with("_pct"))

round_1 <- function(i) round(i, 1)
round_2 <- function(i) round(i, 2)

summary_data <- apply(pcts, MARGIN = 2, description)

summary_table <- as_tibble(summary_data) %>%
  mutate(across(everything(), round_2)) %>%
  mutate(Statistic = c("Minimum", "Median", "Mean", "SD", "Half-SD", "Maximum")) %>%
  select(Statistic, everything())

# 4) County-level means (population-weighted where appropriate)
means_table <- estimates_table %>%
  mutate(county_fips = str_sub(GEOID, 1, 5)) %>%
  select(-GEOID, tot_pop, ends_with("_est"), ends_with("_uni"), -matches("moe"), -year) %>%
  group_by(county_fips) %>%
  summarise(
    d_pctest  = sum(d_est)  / sum(d_uni),
    em_pctest = sum(em_est) / sum(em_uni),
    f_pctest  = sum(f_est)  / sum(f_uni),
    fb_pctest = sum(fb_est) / sum(fb_uni),
    le_pctest = sum(le_est) / sum(le_uni),
    li_pctest = sum(li_est) / sum(li_uni),
    oa_pctest = sum(oa_est) / sum(tot_pop),
    rm_pctest = sum(rm_est) / sum(rm_uni),
    y_pctest  = sum(y_est)  / sum(tot_pop),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x * 100, 1)))

## ---------------------------------------------------------------------------
## EXPORTS
## ---------------------------------------------------------------------------

ipd_table[is.na(ipd_table)] <- ""

# Tract-level outputs
write.csv(ipd_table, file.path(output_dir, paste0("ipd_", ipd_year, ".csv")), row.names = FALSE)

vect_shapefile <- vect(ipd_shapefile)
writeVector(
  vect_shapefile,
  file.path(output_dir, paste0("ipd_", ipd_year, ".shp")),
  filetype = "ESRI Shapefile",
  overwrite = TRUE
)

ipd_gpkg <- st_write(ipd_shapefile, dsn = file.path(output_dir, paste0("ipd_", ipd_year, ".gpkg")), layer = paste0("ipd_", ipd_year))

# Summary tables
write.csv(counts_table,        file.path(output_dir, paste0("counts_by_indicator_",  ipd_year, ".csv")), row.names = FALSE)
write.csv(class_breaks_table,  file.path(output_dir, paste0("breaks_by_indicator_",  ipd_year, ".csv")), row.names = FALSE)
write.csv(summary_table,       file.path(output_dir, paste0("summary_by_indicator_", ipd_year, ".csv")), row.names = FALSE)
write.csv(means_table,         file.path(output_dir, paste0("means_by_county_",      ipd_year, ".csv")), row.names = FALSE)
