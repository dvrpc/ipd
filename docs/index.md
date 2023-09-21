### Technical Reference

# Outline

1. [About](#about)
   - 1a. [Getting started](#one_a)
   - [Output abbreviations](#one_b)
   - [Project structure](#one_c)
2. [Setup](#setup)
   - [Dependencies](#two_a)
   - [Fields](#two_b)
   - [Year](#two_c)
   - [States](#two_d)
   - [Counties](#two_e)
   - [Census API key](#two_f)
   - [Functions](#two_g) 
      1. [Override `base` and `stats` function defaults](#two_g_i) 
      2. [Create custom half-standard deviation breaks](#two_g_ii) 
      3. [_Exception_](#two_g_iii) 
      4. [Move column or vector of columns to last position](#two_g_iv) 
      5. [Summarize data](#two_g_v)
3. [Variance replicate table download](#variance_replicate_table_download)
   - [Download variance replicates from Census website](#three_a)
   - [Combine and format downloads](#three_b)
4. [Variance replicate table processing](#variance_replicate_table_processing)
   - [Compute racial minority count MOE](#four_a)
   - [Save results](#four_b)
5. [ACS estimates download](#acs_estimates_download)
   - [Fields](#five_a)
   - [Download counts and universes from Census API](#five_b) 1. [_Exception_](#five_b_i)
   - [Download percentages from Census API](#five_c)
   - [Format downloads](#five_d) 1. [_Exception_](#five_d_i) 2. [_Exception_](#five_d_ii) 3. [_Exception_](#five_d_iii)
6. [ACS estimates calculations](#acs_estimates_calculations)
   - [Percentages and percentage MOEs](#six_a) 1. [Calculation](#six_a_i) 2. [Result](#six_a_ii) 3. [_Exception_](#six_a_iii) 4. [_Exception_](#six_a_iv)
   - [Percentile](#six_b) 1. [Calculation](#six_b_i) 2. [Result](#six_b_ii)
   - [IPD score and classification](#six_c) 1. [Calculation](#six_c_i) 2. [Result](#six_c_ii)
   - [Composite IPD score](#six_d) 
      1. [Calculation](#six_d_i)
7. [ACS estimates cleaning](#acs_estimates_cleaning)
8. [Summary tables](#summary_tables)
   - [Counts by indicator](#eight_a)
   - [Breaks by indicator](#eight_b)
   - [Summary by indicator](#eight_c)
   - [County means by indicator](#eight_d)
   - [Replacing NA with NoData if character and -99999 if numeric](#eight_e)
9. [Export](#export)
   - [Append to TIGER/LINE file](#nine_a)
   - [Export files](#nine_b)
10. [Metadata table with sources](#metadata)

# 1. About {#about}

DVRPC's IPD analysis identifies populations of interest under Title VI of the Civil Rights Act and the Executive Order on Environmental Justice (#12898) using 2016–2020 American Community Survey (ACS) five-year estimates from the U.S. Census Bureau. IPD analysis assists both DVRPC and outside organizations in equity work by identifying populations of interest, including youth, older adults, female, racial minority, ethnic minority, foreign-born, limited English proficiency, disabled, and low-income populations at the census tract level in DVRPC's nine-county region.

There are many ways of identifying these populations of interest. This document discusses DVRPC's process, which is automated in an `R` script.

## 1a. Getting started {#one_a}

For guidance on software prerequisites and how to run this script, see the [README.md](https://github.com/dvrpc/ipd)

## 1b. Output abbreviations {#one_b}

Components of field names that you'll see in `outputs` and throughout the script.

| Component | Equivalent                  |
| :-------- | :-------------------------- |
| D         | Disabled                    |
| EM        | Ethnic Minority             |
| F         | Female                      |
| FB        | Foreign-Born                |
| LEP       | Limited English Proficiency |
| LI        | Low-Income                  |
| OA        | Older Adults                |
| RM        | Racial Minority             |
| Y         | Youth                       |
| CntEst    | Count Estimate              |
| CntMOE    | Count MOE                   |
| PctEst    | Percentage Estimate         |
| PctMOE    | Percentage MOE              |
| Pctile    | Percentile                  |
| Score     | Score                       |
| Class     | Classification              |

<br>

Abbreviations of field names that you'll see in `outputs` *not* comprised of the above components.

| Abbreviation | Equivalent                                     |
| :----------- | :--------------------------------------------- |
| GEOID        | Census Tract Identifier                        |
| STATEFP      | State FIPS Code                                |
| COUNTYFP     | County FIPS Code                               |
| NAME         | Census Tract FIPS Code                         |
| IPD_Score    | Composite IPD Score                            |
| U_TPopEst    | Total Population Estimate                      |
| U_TPopMOE    | Total Population MOE                           |
| U_Pop5Est    | Population 5+ Estimate                         |
| U_Pop5MOE    | Population 5+ MOE                              |
| U_PPovEst    | Poverty Status Population Estimate             |
| U_PPovMOE    | Poverty Status Population MOE                  |
| U_PNICEst    | Non-Institutional Civilian Population Estimate |
| U_PNICMOE    | Non-Institutional Civilian Population MOE      |

## 1c. Project structure {#one_c}

This script uses relative file paths based off the location of `ipd.Rproj`. As long as you download the entire repository, the script should have no trouble locating the correct subfolders. All of the subsequent years files are based on the same architecture. The project is structured as follows:

```r
ipd
ipd.Rproj
  script.R
    documentation
      discussion.pdf
      getting_started.pdf
      script_reference.pdf
      script_reference.Rmd
      variables.csv
    outputs
      breaks_by_indicator.csv
      counts_by_indicator.csv
      ipd.csv
      ipd.dbf
      ipd.prj
      ipd.shp
      ipd.shx
      mean_by_county.csv
      summary_by_indicator.csv
```

# 2. Setup {#setup}

## 2a. Dependencies {#two_a}

The following packages are required to run this script. If you don't have the packages, you'll get the warning `Error in library (<name of package>) : there is no package called '<name of package>'`, in which case you'll need to install the package before proceeding.

```r
library(plyr); library(here); library(sf); library(summarytools);
library(tidycensus); library(tidyverse); library(tigris); library(dplyr); library(descr);
```

## 2b. Fields {#two_b}

The base information we need for IPD analysis are universes, counts, and percentages for nine indicators at the census tract level. For each indicator, the table below shows the indicator name, its abbreviation used in the script, its universe, its count, and its percentage field if applicable. Find a list of histoic variables below:

| Field                         | year_2015         | year_2016         | year_2017         | year_2018         | year_2019         | year_2020         | year_2021         |
| :------------------------------ | :-----------------: | :-----------------: | :-----------------: | :-----------------: | :-----------------: | :-----------------: | -----------------: |
| disabled_universe             | S1810_C01_001     | S1810_C01_001     | S1810_C01_001     | S1810_C01_001     | S1810_C01_001     | S1810_C01_001     | S1810_C01_001     |
| disabled_count                | S1810_C02_001     | S1810_C02_001     | S1810_C02_001     | S1810_C02_001     | S1810_C02_001     | S1810_C02_001     | S1810_C02_001     |
| disabled_percent              | S1810_C03_001     | S1810_C03_001     | S1810_C03_001     | S1810_C03_001     | S1810_C03_001     | S1810_C03_001     | S1810_C03_001     |
| ethnic_minority_universe     | B03002_001        | B03002_001        | B03002_001        | B03002_001        | B03002_001        | B03002_001        | B03002_001        |
| ethnic_minority_count        | B03002_012        | B03002_012        | B03002_012        | B03002_012        | B03002_012        | B03002_012        | B03002_012        |
| ethnic_minority_percent      | NA                | NA                | NA                | NA                | NA                | NA                | NA                |
| female_universe              | S0101_C01_001     | S0101_C01_001     | S0101_C01_001     | S0101_C01_001     | S0101_C01_001     | S0101_C01_001     | S0101_C01_001     |
| female_count                 | S0101_C03_001     | S0101_C03_001     | S0101_C05_001     | S0101_C05_001     | S0101_C05_001     | S0101_C05_001     | S0101_C05_001     |
| female_percent               | DP05_0003PE       | DP05_0003PE       | DP05_0003PE       | DP05_0003PE       | DP05_0003PE       | DP05_0003PE       | DP05_0003PE       |
| foreign_born_universe        | B05012_001        | B05012_001        | B05012_001        | B05012_001        | B05012_001        | B05012_001        | B05012_001        |
| foreign_born_count           | B05012_003        | B05012_003        | B05012_003        | B05012_003        | B05012_003        | B05012_003        | B05012_003        |
| foreign_born_percent         | NA                | NA                | NA                | NA                | NA                | NA                | NA                |
| limited_english_proficiency_universe | S1601_C01_001 | S1601_C01_001 | S1601_C01_001 | S1601_C01_001 | S1601_C01_001 | S1601_C01_001 | S1601_C01_001 |
| limited_english_proficiency_count    | S1601_C05_001 | S1601_C05_001 | S1601_C05_001 | S1601_C05_001 | S1601_C05_001 | S1601_C05_001 | S1601_C05_001 |
| limited_english_proficiency_percent  | S1601_C06_001 | S1601_C06_001 | S1601_C06_001 | S1601_C06_001 | S1601_C06_001 | S1601_C06_001 | S1601_C06_001 |
| low_income_universe          | S1701_C01_001     | S1701_C01_001     | S1701_C01_001     | S1701_C01_001     | S1701_C01_001     | S1701_C01_001     | S1701_C01_001     |
| low_income_count             | S1701_C01_042     | S1701_C01_042     | S1701_C01_042     | S1701_C01_042     | S1701_C01_042     | S1701_C01_042     | S1701_C01_042     |
| low_income_percent           | NA                | NA                | NA                | NA                | NA                | NA                | NA                |
| older_adults_universe       | S0101_C01_001     | S0101_C01_001     | S0101_C01_001     | S0101_C01_001     | S0101_C01_001     | S0101_C01_001     | S0101_C01_001     |
| older_adults_count          | DP05_0025E        | DP05_0025E        | DP05_0025E        | S0101_C01_030     | S0101_C01_030     | S0101_C01_030     | S0101_C01_030     |
| older_adults_percent        | S0101_C02_028     | S0101_C02_028     | S0101_C02_030     | S0101_C02_030     | S0101_C02_030     | S0101_C02_030     | S0101_C02_030     |
| racial_minority_universe    | B02001_001        | B02001_001        | B02001_001        | B02001_001        | B02001_001        | B02001_001        | B02001_001        |
| racial_minority_count       | B02001_002        | B02001_002        | B02001_002        | B02001_002        | B02001_002        | B02001_002        | B02001_002        |
| racial_minority_percent     | NA                | NA                | NA                | NA                | NA                | NA                | NA                |
| youth_universe              | B03002_001        | B03002_001        | B03002_001        | B03002_001 | B03002_001        | B03002_001        |       


Some percentage fields are empty. This is okay: we will compute the percentages when they are not directly available from the ACS.

Note that variable B02001_002 ("Estimate; Total: - White alone") is listed as the count for Racial Minority. This is a mathematical shortcut: otherwise, we would need to add several subfields to compute the same estimate. The desired count is B02001_001 (Universe) $-$ B02001_002 ("Estimate; Total: - White alone"). The subtraction is computed after download in Section 5d.i., making a correct estimate and an incorrect MOE. The correct MOE for the count, as calculated in Section 4, will be appended later.

| Indicator                   | Abbreviation |   Universe    |     Count     |  Percentage   |
| :-------------------------- | :----------: | :-----------: | :-----------: | :-----------: |
| Disabled                    |      D       | S1810_C01_001 | S1810_C02_001 | S1810_C03_001 |
| Ethnic Minority             |      EM      |  B03002_001   |  B03002_012   |      N/A      |
| Female                      |      F       | S0101_C01_001 | S0101_C05_001 |  DP05_0003PE  |
| Foreign-Born                |      FB      |  B05012_001   |  B05012_003   |      N/A      |
| Limited English Proficiency |     LEP      | S1601_C01_001 | S1601_C05_001 | S1601_C06_001 |
| Low-Income                  |      LI      | S1701_C01_001 | S1701_C01_042 |      N/A      |
| Older Adults                |      OA      | S0101_C01_001 | S0101_C01_030 | S0101_C02_030 |
| Racial Minority             |      RM      |  B02001_001   |  B02001_002   |      N/A      |
| Youth                       |      Y       |  B03002_001   |  B09001_001   |      N/A      |

<br>

While it's quicker to embed the names of the desired columns into the code, fields are explicitly spelled out in this script. This is a purposeful design choice. The user should check that the field names point to the correct API request with every IPD update. The best way to check the field names is to visit [Census Developers](https://www.census.gov/developers/) and select the corresponding API. For a history of the ACS variables used in IPD 2015, 2016, and 2017, see `variables.csv` in the `documentation` folder or in the table above.

<br>

## 2c. Year {#two_c}

The data download year.

```r
ipd_year <- 2017
```

<br>

## 2d. States {#two_d}

The data download state or states. Use the two-character text abbreviation.
<br>

```r
ipd_states <- c("NJ", "PA")
```

<br>

## 2e. Counties {#two_e}

The counties in your study area. Use five-digit characters concatenating the two-digit state and three-digit county GEOID's.
<br>

```r
ipd_counties <- c("34005", "34007", "34015", "34021",
                  "42017", "42029", "42045", "42091", "42101")
```

<br>

## 2f. Census API Key {#two_f}

Placeholder if you have never installed an API key before. If this is your first time accessing the Census API using `R`, see [README.md](https://github.com/dvrpc/ipd/#readme)
<br>

```r
# Census API Key
census_api_key("YOUR API KEY GOES HERE", install = TRUE)
```

# _THE TYPICAL USER SHOULD NOT HAVE TO EDIT ANYTHING BELOW THIS POINT._

## 2g. Functions {#two_g}

Load custom functions.

### 2g.i. Override `base` and `stats` function defaults {#two_g_i}

A time-saver so that it's not required to call `na.rm = TRUE` every time common functions are called.
<br>

```r
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
```

### 2g.ii. Create custom half-standard deviation breaks {#two_g_ii}

For a given vector of numbers `x` and a number of bins `i`, `st_dev_breaks` computes the bin breaks starting at `$-0.5 \cdot st dev$` and `$0.5 \cdot st dev$`. For the purposes of IPD analysis, `i = 5`, and `st_dev_breaks` calculates the minimum, `$-1.5 \cdot st dev$`, `$-0.5 \cdot st dev$`, `$0.5 \cdot st dev$`, `$1.5 \cdot st dev$`, and maximum values. These values are later used to slice the vector into five bins.

### 2g.iii. _Exception_ {#two_g_iii}

All minima are coerced to equal zero. If the first bin break (`$-1.5 \cdot st dev$`) is negative, as happens when the data has a large spread and therefore a large standard deviation, then this bin break is coerced to equal 0.1. In these cases, only estimates of 0 percent will be placed in the bottom bin.
<br>

```r
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
```

### 2g.iv. Move column or vector of columns to last position {#two_g_iv}

The requested schema for IPD data export renames and places all relevant universes in the final columns of the dataset. `move_last` moves a column or vector of column names to the last position(s) in a data frame. Further reordering is done in the final web application.
<br>

```r
move_last <- function(df, last_col) {
  match(c(setdiff(names(df), last_col), last_col), names(df))
}
```

### 2g.v. Summarize data {#two_g_v}

`description` tailors the exports from `summarytools::descr` to create summary tables with the requested fields. `$0.5 \cdot st dev$` is returned after `$stdev$`.
<br>

```r
description <- function(i) {
  des <- as.numeric(summarytools::descr(i, na.rm = TRUE,
                                        stats = c("min", "med", "mean", "sd", "max")))
  des <- c(des[1:4], des[4] / 2, des[5])
  return(des)
}
```

# 3. Variance replicate table download {#variance_replicate_table_download}

The racial minority indicator is created by summing up several subgroups in ACS Table `B03002`. This means that the MOE for the count has to be computed. While the ACS has issued guidance on computing the MOE by aggregating subgroups, using the approximation formula can artificially deflate the derived MOE. Variance replicate tables are used instead to account for covariance and compute a more accurate MOE. The MOE computed from variance replicates is substituted in for the racial minority count MOE in Section 5d.ii.

See the Census Bureau's [Variance Replicate Tables Documentation](https://www.census.gov/programs-surveys/acs/technical-documentation/variance-tables.html) for additional guidance on working with variance replicates.

## 3a. Download variance replicates from Census website {#three_a}

Download, unzip, and read variance replicate tables for Table `B02001`. Results are combined into a single table called `var_rep`.
<br>

```r
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
```

## 3b. Combine and format downloads {#three_b}

Subset `var_rep` for the study area defined in `ipd_counties` and extract the necessary subgroups.
<br>

```r
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
```

# 4. Variance replicate table processing {#variance_replicate_table_processing}

## 4a. Compute racial minority count MOE {#four_a}

Add up the racial minority counts into a single count per census tract for the estimate and 80 variance replicates. Separate the resulting data frame into estimates and variance replicates.
<br>

```r
num <- var_rep %>% 
  group_by(GEOID) %>%
  summarize_if(is.numeric, ~ sum(.)) %>%
  select(-GEOID)
estim <- num %>% select(ESTIMATE)
individual_replicate <- num %>% select(-ESTIMATE)
```

Compute the variance replicate for the count. GEOIDs are stored as `id` to be re-appended to the MOEs after they are calculated.
<br>

```r
id <- var_rep %>% select(GEOID) %>% distinct(.) %>% pull(.)
sqdiff_fun <- function(v, e) (v - e) ^ 2
sqdiff <- mapply(sqdiff_fun, individual_replicate, estim) 
sum_sqdiff <- sapply(sqdiff, sum) 
variance <- 0.05 * sum_sqdiff
moe <- round(sqrt(variance) * 1.645, 0)
```

## 4b. Save results {#four_b}

Save the racial minority MOE.
<br>

```r
rm_moe <- cbind(id, moe) %>%
  as_tibble(.) %>%
  rename(GEOID20 = id, RM_CntMOE = moe) %>%
  mutate_at(vars(RM_CntMOE), as.numeric)
```

# 5. ACS estimates download {#acs_estimates_download}

## 5a. Fields {#five_a}

Fields for downloads from the ACS API were discussed in Section 2b.

## 5b. Download counts and universes from Census API {#five_b}

Download counts and percentages for each of IPD's nine indicators. Note that the download is for all census tracts in `ipd_states`.

Input data for IPD comes from ACS Subject Tables, Detailed Tables, and Data Profiles. While one can request all the fields for Subject Tables in one batch, mixing requests for two or more different types of tables will result in failure. For this reason, the counts and universe fields supplied by the user in Section 2b are evaluated for their contents and split into three batches: `s_counts` for Subject Tables, `d_counts` for Detailed Tables, and `dp_counts` for Data Profiles.

The chunk below zips the user-defined calls from the API with the output abbreviations into a data frame called `counts_calls` and separates the calls into three batches.
<br>

```r
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
```

API calls are made separately for ACS Subject Tables, Detailed Tables, and Data Profiles and appended to `dl_counts`. Sometimes there are no requests for an ACS table type; in these situations, the script bypasses a download attempt. Then, information from `counts_calls` is used to rename the downloads to the appropriate abbreviation.
Please note that ACS surveys 2019 and before make use of the GEOID10 variable.
<br>

```r
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
  rename(GEOID20 = GEOID)
```

### 5b.i. _Exception_ {#five_b_i}

The API does not allow redundant downloads, so universes for Older Adults and Youth are duplicated after download. `duplicate_cols` identifies duplicate API calls, and `combined_rows` serves as a crosswalk to duplicate and rename fields.
<br>

```r
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
```

## 5c. Download percentages from Census API {#five_c}

Download percentage tables that are available for four of IPD's nine indicators. We will compute percentages and their associated MOEs for the rest of the dataset later. The procedure is identical to that described in Section 5b.
<br>

```r
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
  rename(GEOID20 = GEOID)

# For DP downloads, make sure percs_calls and dl_percs match

percs_calls$api <- str_replace(percs_calls$api, "PE", "")
names(dl_percs) <- str_replace(names(dl_percs), "PE", "E")
names(dl_percs) <- str_replace(names(dl_percs), "PM", "M")

for(i in 1:length(percs_calls$id)){
  names(dl_percs) <- str_replace(names(dl_percs),
                                 percs_calls$api[i],
                                 percs_calls$id[i])
}
```

## 5d. Format downloads {#five_d}

Subset `dl_counts` and `dl_percs` for DVRPC's nine-county region. Percentages should range from 0 to 100.
<br>

```r
dl_counts <- dl_counts %>%
  filter(str_sub(GEOID20, 1, 5) %in% ipd_counties)
dl_percs <- dl_percs %>%
  filter(str_sub(GEOID20, 1, 5) %in% ipd_counties)
```

### 5d.i. _Exception_ {#five_d_i}

Note that variable `B02001_002 ("Estimate; Total: - White alone")` was downloaded as the count for racial minority. Compute `B02001_001 (Universe)` - `B02001_002 ("Estimate; Total: - White alone")` and substitute for `RM_CE`.
<br>

```r
dl_counts <- dl_counts %>% mutate(x = RM_UE - RM_CE) %>%
  select(-RM_CE) %>%
  rename(RM_CE = x)
```

### 5d.ii. _Exception_ {#five_d_ii}

Half-standard deviations serve as the classification bins for IPD scores, and including zero-population tracts affects computed standard deviation values. Start by removing the 11 census tracts with zero population.
<br>

```r
slicer <- c("34005981802","34005982200","34021980000","42017980000",
            "42045980300","42045980000","42045980200","42091980100",
            "42091980000","42091980200","42091980300","42101036901",
            "42101980001","42101980002","42101980003","42101980300",
            "42101980701","42101980702","42101980800","42101980100",
            "42101980200", "42101980400","42101980500","42101980600",
            "42101980901","42101980902","42101980903","42101980904",
            "42101980905","42101980906", "42101989100","42101989200",
            "42101989300")
dl_counts <- dl_counts %>% filter(!(GEOID20 %in% slicer))
dl_percs <- dl_percs %>% filter(!(GEOID20 %in% slicer))
```

Here are the first few lines of `dl_counts` and `dl_percs`. Notice the naming convention:

- `UE` = universe estimate
- `UM` = universe MOE
- `CE` = count estimate
- `CM` = count MOE
- `PE` = percentage estimate
- `PM` = percentage MOE

We use these strings to select columns, so consistency is key.
<br>

# 6. ACS estimates calculations {#acs_estimates_calculations}

For all nine indicators, this section computes:

- Percentages and percentage MOEs
- Percentile
- IPD score and classification
- Composite IPD score

Split `dl_counts` into a list named `comp` for processing and arrange column names in alphabetical order. The name of the list, `comp`, is a nod to the "component parts" of `dl_counts`. The structure of `comp` is similar to a four-tab Excel spreadsheet: for example, `comp` is the name of the `.xlsx` file, `uni_est` is a tab for universe estimates, and `uni_est` has nine columns and 1,368 rows, where the column is the IPD indicator and the row is the census tract observation.

The order of columns is important because processing is based on vector position. We want to make sure that the first column of every tab corresponds to the Disabled indicator, the second to Ethnic Minority, et cetera.
<br>

```r
comp <- list()
comp$uni_est <- dl_counts %>% select(ends_with("UE")) %>% select(sort(tidyselect::peek_vars()))
comp$uni_moe <- dl_counts %>% select(ends_with("UM")) %>% select(sort(tidyselect::peek_vars()))
comp$count_est <- dl_counts %>% select(ends_with("CE")) %>% select(sort(tidyselect::peek_vars()))
comp$count_moe <- dl_counts %>% select(ends_with("CM")) %>% select(sort(tidyselect::peek_vars()))
```

## 6a. Percentages and percentage MOEs {#six_a}

### 6a.i. Calculation {#six_a_i}

MOEs of the percentage values are obtained using the `tidycensus` function `moe_prop`. This chunk mentions `r` and `c` several times: continuing the spreadsheet analogy, think of `r` as the row number and `c` as the column number for a given spreadsheet tab.
<br>

```r
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
```

### 6a.ii. Result {#six_a_ii}

`pct` and `pct_moe` stores the percentages and associated MOEs for the nine indicator variables. Results are rounded to the tenths place and range from 0 to 100.
<br>

```r
pct <- as_tibble(pct_matrix) %>% mutate_all(~ . * 100) %>% mutate_all(round_1)
names(pct) <- str_replace(names(comp$uni_est), "_UE", "_PctEst")
pct_moe <- as_tibble(pct_moe_matrix) %>% mutate_all(~ . * 100) %>% mutate_all(round_1)
names(pct_moe) <- str_replace(names(comp$uni_est), "_UE", "_PctMOE")
```

### 6a.iii. _Exception_ {#six_a_iii}

If the percentage MOE equals 0, then overwrite it to equal 0.1. This should be a rare occurence with survey data at the census tract level.
<br>

```r
pct_moe <- pct_moe %>% replace(., . == 0, 0.1)
```

### 6a.iv. _Exception_ {#six_a_iv}

Substitute percentages and associated MOEs when available. This applies to the older adults, female, limited English proficiency, and disabled variables.
<br>

```r
pct <- pct %>% mutate(D_PctEst = dl_percs$D_PE,
                      OA_PctEst = dl_percs$OA_PE,
                      LEP_PctEst = dl_percs$LEP_PE,
                      F_PctEst = dl_percs$F_PE)
pct_moe <- pct_moe %>% mutate(D_PctMOE = dl_percs$D_PM,
                              OA_PctMOE = dl_percs$OA_PM,
                              LEP_PctMOE = dl_percs$LEP_PM,
                              F_PctMOE = dl_percs$F_PM)
```

## 6b. Percentile {#six_b}

### 6b.i. Calculation {#six\_\_b_i}

Add percentiles (an additional "spreadsheet tab") to `comp`, making sure to first sort column names alphabetically. Compute the empirical cumulative distribution function for each of the nine indicator variables. The ECDF can range from 0 to 1, where 1 indicates the largest observed percentage.
<br>

```r
comp$pct_est <- pct %>% select(sort(tidyselect::peek_vars()))
percentile_matrix <- NULL
for (c in 1:length(comp$uni_est)){
  p <- unlist(comp$pct_est[,c])
  rank <- ecdf(p)(p)
  percentile_matrix <- cbind(percentile_matrix, rank)
}

```

### 6b.ii. Result {#six_b_ii}

`percentile` stores the percentile for the nine indicator variables. Results are rounded to the hundredths place.
<br>

```r
percentile <- as_tibble(percentile_matrix) %>% mutate_all(round_2)
names(percentile) <- str_replace(names(comp$uni_est), "_UE", "_Pctile")
```

## 6c. IPD score and classification {#six_c}

Each observation is assigned an IPD score for each indicator. The IPD score for an individual indicator can range from 0 to 4, which corresponds to the following classification and bin breaks:

| IPD Score | IPD Classification |              Standard Deviations              |
| :-------  | :----------------: | -------------------------------------------:  |
|     0     | Well Below Average |            x $< -1.5 \cdot stdev$             |
|     1     |   Below Average    | $-1.5 \cdot stdev \leq$ x $<-0.5 \cdot stdev$ |
|     2     |      Average       | $-0.5 \cdot stdev \leq$ x $<0.5 \cdot stdev$  |
|     3     |   Above Average    |  $0.5 \cdot stdev \leq$ x $<1.5 \cdot stdev$  |
|     4     | Well Above Average |           x $\geq 1.5 \cdot stdev$            |

<br>

### 6c.i. Calculation {#six_c_i}

The function `st_dev_breaks` is called to compute the bin breaks for each indicator. These breaks determine the IPD score stored in `score`. Note that we divide *rounded* `PctEst` columns by *unrounded* half-standard deviation breaks to compute the `score`. `class` is a textual explanation of the IPD score.
<br>

```r
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
```

### 6c.ii. Result {#six_c_ii}

`score` and `class` store the IPD scores and associated descriptions for the nine indicator variables.
<br>

```r
score <- as_tibble(score_matrix)
names(score) <- str_replace(names(comp$uni_est), "_UE", "_Score")
class <- as_tibble(class_matrix)
names(class) <- str_replace(names(comp$uni_est), "_UE", "_Class")
```

## 6d. Composite IPD score {#six_d}

### 6d.i. Calculation {#six_d_i}

Sum the IPD scores for the nine indicator variables to determine the composite IPD score.
<br>

```r
score <- score %>% mutate(IPD_Score = rowSums(.))
```

# 7. ACS estimates cleaning {#acs_estimates_cleaning}

There is a specific output format for `ipd.csv`, including column names, column order, flags for missing data, and census tracts with insufficient data. This section ensures conformity with the output formatting.

Merge the percentage estimates, percentage MOEs, percentile, score, and class data frames into a single data frame called `ipd`.
<br>

```r
ipd <- bind_cols(dl_counts, pct) %>%
  bind_cols(., pct_moe) %>%
  bind_cols(., percentile) %>%
  bind_cols(., score) %>%
  bind_cols(., class)
```

Rename columns.
<br>

```r
names(ipd) <- str_replace(names(ipd), "_CE", "_CntEst")
names(ipd) <- str_replace(names(ipd), "_CM", "_CntMOE")
ipd <- ipd %>% mutate(STATEFP20 = str_sub(GEOID20, 1, 2),
                      COUNTYFP20 = str_sub(GEOID20, 3, 5),
                      NAME20 = str_sub(GEOID20, 6, 11),
                      U_TPopEst = F_UE,
                      U_TPopMOE = F_UM,
                      U_Pop6Est = LEP_UE,
                      U_Pop6MOE = LEP_UM,
                      U_PPovEst = LI_UE,
                      U_PPovMOE = LI_UM,
                      U_PNICEst = D_UE,
                      U_PNICMOE = D_UM) %>%
  select(-ends_with("UE"), -ends_with("UM"))
```

Reorder columns, with `GEOID` and FIPS codes first, the following variables in alphabetical order, and the total IPD score and universes at the end.
<br>

```r
ipd <- ipd %>% select(GEOID20, STATEFP20, COUNTYFP20, NAME20, sort(tidyselect::peek_vars())) %>%
  select(move_last(., c("IPD_Score", "U_TPopEst", "U_TPopMOE",
                        "U_Pop6Est", "U_Pop6MOE", "U_PPovEst",
                        "U_PPovMOE", "U_PNICEst", "U_PNICMOE")))
```

At the beginning of processing, we removed the slicer census tracts from processing because their populations were equal to zero. Tack these back on to the dataset.
<br>

```r
slicer <- enframe(slicer, name = NULL, value = "GEOID20")
ipd <- plyr::rbind.fill(ipd, slicer)
```

Replace `NA` values with `NoData` if character we'll wait to replace `NA` with `-99999` if numeric.
<br>

```r
ipd <- ipd %>% mutate_if(is.character, ~(ifelse(is.na(.), "NoData", .))) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .)))
```

# 8. Summary Tables {#summary_tables}

This section generates a handful of other deliverables, including:

a. Counts by indicator
b. Breaks by indicator
c. Summary by indicator
d. County means by indicator

Replace `-99999` with `0` for numeric columns to avoid distorting summary statistics.
<br>

```r
ipd_summary <- ipd
ipd_summary[ipd_summary == 0]
```

## 8a. Counts by indicator {#eight_a}

The number of census tracts that fall in each bin. Count census tracts by indicator and bin. Reorder factor levels so that "Well Below Average" appears before "Below Average," and the like.
<br>

```r
counts <- ipd_summary %>% select(ends_with("Class"))
export_counts <- apply(counts, 2, function(i) plyr::count(i))
for(i in 1:length(export_counts)){
  export_counts[[i]]$var <- names(export_counts)[i]
}
export_counts <- map_dfr(export_counts, `[`, c("var", "x", "freq"))
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
```

## 8b. Breaks by indicator {#eight_b}

The bin breaks for each indicator. Apply the `st_dev_breaks` function to all percentage values and export results.
<br>

```r
breaks <- ipd_summary %>% select(ends_with("PctEst"))
export_breaks <- round(mapply(st_dev_breaks, x = breaks, i = 5, na.rm = TRUE), digits = 3)
export_breaks <- as_tibble(export_breaks) %>%
  mutate(Class = c("Min", "1", "2", "3", "4", "Max")) %>%
  select(Class, tidyselect::peek_vars())
```

## 8c. Summary by indicator {#eight_c}

Summary statistics of each indicator. Round results to two decimal places.
<br>

```r
pcts <- ipd_summary %>% select(ends_with("PctEst"))
summary_data <- apply(pcts, MARGIN=2, description)
export_summary <- as_tibble(summary_data) %>%
  mutate_all(round_2) %>%
  mutate(Statistic = c("Minimum", "Median", "Mean", "SD", "Half-SD", "Maximum")) %>%
  select(Statistic, tidyselect::peek_vars())
```

## 8d. County means by indicator {#eight_d}

Population-weighted means by county and indicator. For the most accurate percentage values, aggregate all counts back to the county level and compute percentages. In the export file, counties are referred to by the five-digit character supplied by the user to `ipd_counties`.
<br>

```r
export_means <- dl_counts %>% select(GEOID20, ends_with("UE"), ends_with("CE")) %>%
  select(GEOID20, sort(tidyselect::peek_vars())) %>%
  mutate(County = str_sub(GEOID20, 1, 5)) %>%
  select(-GEOID20) %>%
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
```

## 8e. Replacing NA with NoData if character and -99999 if numeric {#eight_e}
Now we replace NA with NoData if character and -99999 if numeric so tract 42091206702 doesn't mess up breaks and means by indicator.
```r
ipd <- ipd %>% mutate_if(is.character, ~(ifelse(is.na(.), "NoData", .))) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), -99999, .)))
ipd_summary[ipd_summary == -99999] <- NA
```

In addition we extract elements of the GEOID and convert some of the fields to a `character` data type.

```r
ipd$STATEFP20 <- str_sub(ipd$GEOID20,1,2) 
ipd$COUNTYFP20 <- str_sub(ipd$GEOID20,3,5) 
ipd$NAME20 <- str_sub(ipd$GEOID20,6,11) 

ipd$GEOID20 <- as.character(ipd$GEOID20)
ipd$STATEFP20 <- as.character(ipd$STATEFP20)
ipd$COUNTYFP20 <- as.character(ipd$COUNTYFP20)
ipd$NAME20 <- as.character(ipd$NAME20)
ipd$namelsad <- paste(substr(ipd$GEOID20, 6, 9), substr(ipd$GEOID20, 10, 11), sep = ".")
```

# 9. Export {#export}

## 9a. Append to TIGER/LINE file {#nine_a}

Using the arguments supplied in `ipd_county`, download the relevant census tracts and append `ipd` to them. Uncommenting `cb = TRUE` will greatly speed processing time by downloading generalized tract boundary shapefiles instead of detailed ones.
<br>

```r
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
  left_join(., ipd, by = c("GEOID" = "GEOID20")) %>%
  rename(GEOID20 = GEOID)
```

## 9b. Export files {#nine_b}

Results are saved in `outputs`.
<br>

```r
st_write(trct, here("outputs", "ipd_2021.shp"), delete_dsn = TRUE, quiet = TRUE)
write_csv(ipd, here("outputs", "ipd_2021.csv"))
write_csv(export_counts, here("outputs", "counts_by_indicator_2021.csv"))
write_csv(export_breaks, here("outputs", "breaks_by_indicator_2021.csv"))
write_csv(export_summary, here("outputs", "summary_by_indicator_2021.csv"))
write_csv(export_means, here("outputs", "mean_by_county_2021.csv"))
```

# 10. Metadata table with sources {#metadata}

This is a table of the final output with some additional data such as municipality name and area added but not included in the script.
<br>

| Variable            | Concept                                             | acs table       | acs variable    | data source | Source Type  | Universe Variable |
|:------------------- | :---------------------------------------------------: | :---------------: | :---------------: | :-----------: | :------------: | -----------------: |
| geoid20             | 11-digit tract GEOID                                | n/a             | n/a             | ACS 5-year  | n/a          | n/a               |
| statefp20           | 2-digit state GEOID                                 | n/a             | n/a             | ACS 5-year  | n/a          | n/a               |
| countyfp20          | 3-digit county GEOID                                | n/a             | n/a             | ACS 5-year  | n/a          | n/a               |
| name20              | Tract and county name                               | n/a             | n/a             | ACS 5-year  | n/a          | n/a               |
| d_class             | Disabled percentile class                           | n/a             | n/a             | calculated  | calculated   | S1810_C01_001     |
| d_cntest            | Disabled count estimate                             | S1810_C02_001_E | acs variable    | ACS 5-year  | acs variable | S1810_C01_001     |
| d_cntmoe            | Disabled count margin of error                      | S1810_C02_001_M | acs variable    | ACS 5-year  | acs variable | S1810_C01_001     |
| d_pctest            | Disabled percent estimate                           | S1810_C03_001_E | acs variable    | ACS 5-year  | acs variable | S1810_C01_001     |
| d_pctile            | Disabled percentile                                 | n/a             | n/a             | calculated  | calculated   | S1810_C01_001     |
| d_pctmoe            | Disabled percent margin of error                    | S1810_C03_001_M | acs variable    | ACS 5-year  | acs variable | S1810_C01_001     |
| d_score             | Disabled percentile score                           | n/a             | n/a             | calculated  | calculated   | S1810_C01_001     |
| em_class            | Ethnic minority percentile class                    | B03002          | n/a             | calculated  | calculated   | B03002_001        |
| em_cntest           | Ethnic minority count estimate                      | B03002          | B03002_012_E    | ACS 5-year  | acs variable | B03002_001        |
| em_cntmoe           | Ethnic minority count margin of error               | B03002          | B03002_012_M    | ACS 5-year  | acs variable | B03002_001        |
| em_pctest           | Ethnic minority percent estimate                    | B03002          | n/a             | calculated  | calculated   | B03002_001        |
| em_pctile           | Ethnic minority percentile                          | B03002          | n/a             | calculated  | calculated   | B03002_001        |
| em_pctmoe           | Ethnic minority percent margin of error             | B03002          | n/a             | calculated  | calculated   | B03002_001        |
| em_score            | Ethnic minority percentile score                    | B03002          | n/a             | calculated  | calculated   | B03002_001        |
| f_class             | Female percentile class                             | S0101           | n/a             | calculated  | calculated   | S0101_C01_001     |
| f_cntest            | Female count estimate                               | S0101           | S0101_C05_001_E | ACS 5-year  | acs variable | S0101_C01_001     |
| f_cntmoe            | Female count margin of error                        | S0101           | S0101_C05_001_M | ACS 5-year  | acs variable | S0101_C01_001     |
| f_pctest            | Female percent estimate                             | S0101           | DP05_0003PE_E   | ACS 5-year  | acs variable | S0101_C01_001     |
| f_pctile            | Female percentile                                   | S0101           | n/a             | calculated  | calculated   | S0101_C01_001     |
| f_pctmoe            | Female percent margin of error                      | S0101           | DP05_0003PE_M   | ACS 5-year  | acs variable | S0101_C01_001     |
| f_score             | Female percentile score                             | S0101           | n/a             | calculated  | calculated   | S0101_C01_001     |
| fb_class            | Foreign-born percentile class                       | B05012          | n/a             | calculated  | calculated   | B05012_001        |
| fb_cntest           | Foreign-born count estimate                         | B05012          | B05012_003_E    | ACS 5-year  | acs variable | B05012_001        |
| fb_cntmoe           | Foreign-born count margin of error                  | B05012          | B05012_003_M    | ACS 5-year  | acs variable | B05012_001        |
| fb_pctest           | Foreign-born percent estimate                       | B05012          | n/a             | calculated  | calculated   | B05012_001        |
| fb_pctile           | Foreign-born percentile                             | B05012          | n/a             | calculated  | calculated   | B05012_001        |
| fb_pctmoe           | Foreign-born percent margin of error                | B05012          | n/a             | calculated  | calculated   | B05012_001        |
| fb_score            | Foreign-born percentile score                       | B05012          | n/a             | calculated  | calculated   | S1601_C01_001     |
| lep_class           | Limited English proficiency percentile class        | S1601           | n/a             | calculated  | calculated   | S1601_C01_001     |
| lep_cntest          | Limited English proficiency count estimate          | S1601           | S1601_C05_001_E | ACS 5-year  | acs variable | S1601_C01_001     |
| lep_cntmoe          | Limited English proficiency count margin of error   | S1601           | S1601_C05_001_M | ACS 5-year  | acs variable | S1601_C01_001     |
| lep_pctest          | Limited English proficiency percent estimate        | S1601           | S1601_C06_001_E | ACS 5-year  | acs variable | S1601_C01_001     |
| lep_pctile          | Limited English proficiency percentile              | S1601           | n/a             | calculated  | calculated   | S1601_C01_001     |
| lep_pctmoe          | Limited English proficiency percent margin of error | S1601           | S1601_C06_001_M | ACS 5-year  | acs variable | S1601_C01_001     |
| lep_score           | Limited English proficiency percentile score        | S1601           | n/a             | calculated  | calculated   | S1601_C01_001     |
| li_class            | Low-income percentile class                         | n/a             | n/a             | calculated  | calculated   | S1701_C01_001     |
| li_cntest           | Low-income count estimate                           | S1701           | S1701_C01_042_E | ACS 5-year  | acs variable | S1701_C01_001     |
| li_cntmoe           | Low-income count margin of error                    | S1701           | S1701_C01_042_M | ACS 5-year  | acs variable | S1701_C01_001     |
| li_pctest           | Low-income percent estimate                         | n/a             | n/a             | calculated  | calculated   | S1701_C01_001     |
| li_pctile           | Low-income percentile                               | n/a             | n/a             | calculated  | calculated   | S1701_C01_001     |
| li_pctmoe           | Low-income percent margin of error                  | n/a             | n/a             | calculated  | calculated   | S1701_C01_001     |
| li_score            | Low-income percentile score                         | n/a             | n/a             | calculated  | calculated   | S1701_C01_001     |
| oa_class            | Older adult percentile class                        | S0101           | n/a             | calculated  | calculated   | B02001_001        |
| oa_cntest           | Older adult count estimate                          | S0101           | S0101_C01_001_E | ACS 5-year  | acs variable | B02001_001        |
| oa_cntmoe           | Older adult count margin of error                   | S0101           | S0101_C01_001_M | ACS 5-year  | acs variable | B02001_001        |
| oa_pctest           | Older adult percent estimate                        | S0101           | S0101_C02_030_E | ACS 5-year  | acs variable | B02001_001        |
| oa_pctile           | Older adult percentile                              | S0101           | n/a             | calculated  | calculated   | B02001_001        |
| oa_pctmoe           | Older adult percent margin of error                 | S0101           | S0101_C02_030_M | ACS 5-year  | acs variable | B02001_001        |
| oa_score            | Older adult percentile score                        | S0101           | n/a             | calculated  | calculated   | B02001_001        |
| rm_class            | Racial minority percentile class                    | B02001          | n/a             | calculated  | calculated   | B02001_001        |
| rm_cntest           | Racial minority count estimate                      | B02001          | B02001_002_E    | ACS 5-year  | acs variable | B02001_001        |
| rm_cntmoe           | Racial minority count margin of error               | B02001          | B02001_002_M    | ACS 5-year  | acs variable | B02001_001        |
| rm_pctest           | Racial minority percent estimate                    | B02001          | n/a             | calculated  | calculated   | B02001_001        |
| rm_pctile           | Racial minority percentile                          | B02001          | n/a             | calculated  | calculated   | B02001_001        |
| rm_pctmoe           | Racial minority percent margin of error             | B02001          | n/a             | calculated  | calculated   | B02001_001        |
| rm_score            | Racial minority percentile score                    | B02001          | n/a             | calculated  | calculated   | B02001_001        |
| y_class             | Youth percentile class                              | B09001          | n/a             | calculated  | calculated   | B03002_001        |
| y_cntest            | Youth count estimate                                | B09001          | B09001_001      | ACS 5-year  | acs variable | B03002_001        |
| y_cntmoe            | Youth count margin of error                         | B09001          | B09001_001      | ACS 5-year  | acs variable | B03002_001        |
| y_pctest            | Youth population percentage estimate                | B09001          | n/a             | calculated  | calculated   | B03002_001        |
| y_pctile            | Youth population percentile                         | B09001          | n/a             | calculated  | calculated   | B03002_001        |
| y_pctmoe            | Youth population percentage margin of error         | B09001          | n/a             | calculated  | calculated   | B03002_001        |
| y_score             | Youth percentile score                              | B09001          | n/a             | calculated  | calculated   | B03002_001        |
| ipd_score           | Indicator of potential disadvantage score           | n/a             | n/a             | calculated  | calculated   | n/a               |
| u_tpopest           | Total population estimate                           | B02001          | B02001_001_E    | ACS 5-year  | acs variable | B02001_001        |
| u_tpopmoe           | Total population margin of error                    | B02001          | B02001_001_E    | ACS 5-year  | acs variable | B02001_001        |
| u_pop6est           | Population over 6 years of age estimate             | S1601           | S1601_C01_001_E | ACS 5-year  | acs variable | S1601_C01_001     |
| u_pop6moe           | Population over 6 years of age margin of error      | S1601           | S1601_C01_001_M | ACS 5-year  | acs variable | S1601_C01_001     |
| u_ppovest           | Total population poverty rate estimate              | S1701           | S1701_C01_001_E | ACS 5-year  | acs variable | S1701_C01_001     |
| u_ppovmoe           | Total population poverty rate margin of error       | S1701           | S1701_C01_001_M | ACS 5-year  | acs variable | S1701_C01_001     |
| u_pnicest           | Disabled universe total estimate                    | S1810           | S1810_C01_001_E | ACS 5-year  | acs variable | S1810_C01_001     |
| u_pnicmoe           | Disabled universe total margin of error             | S1810           | S1810_C01_001_M | ACS 5-year  | acs variable | S1810_C01_001     |
| namelsad            | Geography name                                      | n/a             | n/a             | ACS 5-year  | acs variable | n/a               |
| mun1                | First municipality name                             | n/a             | n/a             | calculated  | calculated   | n/a               |
| mun2                | Second municipality name                            | n/a             | n/a             | calculated  | calculated   | n/a               |
| mun3                | Third municipality name                             | n/a             | n/a             | calculated  | calculated   | n/a               |
| co_name             | County Name                                         | n/a             | n/a             | calculated  | calculated   | n/a               |
| state               | State name                                          | n/a             | n/a             | ACS 5-year  | n/a          | n/a               |
| st_area(shape)      | Area of a geometry                                  | n/a             | n/a             | ACS 5-year  | n/a          | n/a               |
| st_perimeter(shape) | Perimeter of the geometry                           | n/a             | n/a             | ACS 5-year  | n/a          | n/a               |