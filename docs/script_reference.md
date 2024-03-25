## Table of Contents##

# 1. About
DVRPC's IPD analysis identifies populations of interest under Title VI of the Civil Rights Act and the Executive Order on Environmental Justice (#12898) using American Community Survey (ACS) five-year estimates from the U.S. Census Bureau. IPD analysis assists both DVRPC and outside organizations in equity work by identifying populations of interest, including youth, older adults, female, racial minority, ethnic minority, foreign-born, limited English proficiency, disabled, and low-income populations at the census tract level in DVRPC's nine-county region.

There are many ways of identifying these populations of interest. This document discusses DVRPC's process, which is automated in an `R` script.

## 1a. Getting started
For guidance on software prerequisites and how to run this script, see `getting_started.pdf` in the `documentation` folder.

## 1b. Output abbreviation
Components of field names that you'll see in `outputs` and throughout the script.

 | Component |   Equivalent                      |
 | --------- | --------------------------------- |
 | d         |   Disabled                        |
 | em        |   Ethnic Minority                 |
 | f         |   Female                          |
 | fb        |   Foreign-Born                    |
 | lep       |   Limited English Proficiency     |
 | li        |   Low-Income                      |
 | oa        |   Older Adults                    |
 | rm        |   Racial Minority                 |
 | y         |   Youth                           |


Abbreviations of field names that you'll see in `outputs` *not* comprised of the above components.


| Abbreviation | Equivalent |
|:-------------|:-----------|
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

## 1c. Project structure  
This script uses relative file paths based off the location of `ipd.Rproj`. As long as you download the entire repository, the script should have no trouble locating the correct subfolders. All of the subsequent years files are based on the same architecture. The project is structured as follows:

```{r file_structure, eval = FALSE}
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

# 2. Setup  
## 2a. Dependencies 
Packages required to run this script. If you don't have the packages, you'll get the warning `Error in library (<name of package>) : there is no package called '<name of package>'`, in which case you'll need to install the package before proceeding.

```{r packages, message = FALSE}
library(plyr); library(here); library(sf); library(summarytools);
library(tidycensus); library(tidyverse); library(tigris); library(dplyr); library(descr);
```

## 2b. Fields 
The base information we need for IPD analysis are universes, counts, and percentages for nine indicators at the census tract level. For each indicator, the table below shows the indicator name, its abbreviation used in the script, its universe, its count, and its percentage field if applicable. Some percentage fields are empty. This is okay: we will compute the percentages when they are not directly available from the ACS.


| Indicator | Abbreviation | Universe | Count | Percentage |
|:----------|:------------:|:--------:|:-----:|:----------:|
| Disabled | d | S1810_C01_001 | S1810_C02_001 | S1810_C03_001 |
| Ethnic Minority | em | B03002_001 | B03002_012 | N/A |
| Female | f | S0101_C01_001 | S0101_C05_001 | DP05_0003PE |
| Foreign-Born | fb | B05012_001 | B05012_003 | N/A |
| Limited English Proficiency | lep | S1601_C01_001 | S1601_C05_001 | S1601_C06_001 |
| Low-Income | li | S1701_C01_001 | S1701_C01_042 | N/A |
| Older Adults | oa | S0101_C01_001 | S0101_C01_030 | S0101_C02_030 |
| Racial Minority | rm | B02001_001 | B02001_003...008 | N/A |
| Youth | y | B03002_001 | B09001_001 | N/A |


The user should check that the field names point to the correct API request with every IPD update. The best way to check the field names is to visit [Census Developers](https://www.census.gov/developers/) and select the corresponding API. For a history of the ACS variables used in previous IPD results, see `variables.csv` in the `documentation` folder.


## 2c. Year 
The data download year.
```{r year}
ipd_year <- 2022
```

## 2d. States 
The data download state or states. Use the two-character text abbreviation.
```{r states}
ipd_states <- c("NJ", "PA")
```

## 2e. Counties 
The counties in your study area. Use five-digit characters concatenating the two-digit state and three-digit county FIPS codes.
<br>
```{r counties}
ipd_counties <- c("34005", "34007", "34015", "34021",
                  "42017", "42029", "42045", "42091", "42101")
```
<br>

## 2f. Census API Key 
Placeholder if you have never installed an API key before. If this is your first time accessing the Census API using `R`, see `getting_started.pdf` in the `documentation` folder.
<br>
```{r api_key}
# Census API Key
# census_api_key("YOUR API KEY GOES HERE", install = TRUE)
```

# *THE TYPICAL USER SHOULD NOT HAVE TO EDIT ANYTHING BELOW THIS POINT.*

# 3. ACS Estimates Download

## 3a. Fields
```
acs5_dt_list <- c(
  tot_pop = "B01003_001", # Total Population
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
```

## 3b. Raw Data Tables
```
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
```

## 3c. Combine Data Tables and Calculate MOE  
```
estimates_table <- raw_data_combined %>%
  mutate(rm_est = blk_est + aia_est + asn_est + hpi_est + oth_est + two_est) %>% # Racial minority calculation
  select(-blk_est, -aia_est, -asn_est, -hpi_est, -oth_est, -two_est, -blk_est_MOE, -aia_est_MOE, -asn_est_MOE, -hpi_est_MOE, -oth_est_MOE, -two_est_MOE) %>%
  mutate(rm_pct = round(100 * (rm_est/rm_uni), digits = 1)) %>%
  mutate(em_pct = round(100 * (em_est/em_uni), digits = 1)) %>%
  mutate(fb_pct = round(100 * (fb_est/fb_uni), digits = 1)) %>%
  mutate(li_pct = round(100 * (li_est/li_uni), digits = 1)) %>%
  mutate(y_pct = round(100 * (y_est/tot_pop), digits = 1)) %>%
  mutate(em_pct_MOE = round(moe_prop(em_est,em_uni,em_est_MOE,em_uni_MOE) * 100,1)) %>%
  mutate(fb_pct_MOE = round(moe_prop(fb_est,fb_uni,fb_est_MOE,fb_uni_MOE) * 100,1)) %>%
  mutate(li_pct_MOE = round(moe_prop(li_est,li_uni,li_est_MOE,li_uni_MOE) * 100,1)) %>%
  mutate(y_pct_MOE = round(moe_prop(y_est,tot_pop,y_est_MOE,tot_pop_MOE) * 100,1))
```

# 4. Variance replicate table download 
This will feel out of order, but it's necessary. The racial minority indicator is created by summing up several subgroups in ACS Table B03002. This means that the MOE for the count has to be computed. While the ACS has issued guidance on computing the MOE by aggregating subgroups, using the approximation formula can artificially deflate the derived MOE. Variance replicate tables are used instead to account for covariance and compute a more accurate MOE. The MOE computed from variance replicates is substituted in for the racial minority count MOE in Section 5d.ii.

See the Census Bureau's [Variance Replicate Tables Documentation](https://www.census.gov/programs-surveys/acs/technical-documentation/variance-tables.html) for additional guidance on working with variance replicates.

## 4a. Download variance replicates from Census website 
Download, unzip, and read variance replicate tables for Table B02001. Results are combined into a single table called `var_rep`.

```
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

## 4b. Calculate MOE for percent racial minority 
```
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
  rename(GEOID = ids, rm_est_MOE = moe) %>%
  mutate_at(vars(rm_est_MOE), as.numeric)

estimates_table <- estimates_table %>%
  left_join(., rm_moe) %>%
  mutate(rm_pct_MOE = round(moe_prop(rm_est,rm_uni,rm_est_MOE,rm_uni_MOE) * 100,1))
```


## 4c. Drop Exceptional Census Tracts 
There are 33 census tracts dropped from the IPD calculation. These tracts either have low population counts or contain correctional facilities or military bases. These tracts are removed from the IPD calculation to avoid skewing the standard deviation results.  


```
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
```



# 5. Percentile Calculation
Add percentiles (an additional "spreadsheet tab") to `comp`, making sure to first sort column names alphabetically. Compute the empirical cumulative distribution function for each of the nine indicator variables. The ECDF can range from 0 to 1, where 1 indicates the largest observed percentage.

```
comp$pct_est <- pct %>% select(sort(current_vars()))
percentile_matrix <- NULL
for (c in 1:length(comp$uni_est)){
  p <- unlist(comp$pct_est[,c])
  rank <- ecdf(p)(p)
  percentile_matrix <- cbind(percentile_matrix, rank)
}
```

# 6. IPD score and Classification 
Each observation is assigned an IPD score for each indicator. The IPD score for an individual indicator can range from 0 to 4, which corresponds to the following classification and bin breaks:

| IPD Score | IPD Classification | Standard Deviations |
|:---------:|:------------------:|:-------------------:|
| 0 | Well Below Average | x $< -1.5 \cdot stdev$ |
| 1 | Below Average | $-1.5 \cdot stdev \leq$ x $<-0.5 \cdot stdev$ |
| 2 | Average | $-0.5 \cdot stdev \leq$ x $<0.5 \cdot stdev$ |
| 3 | Above Average | $0.5 \cdot stdev \leq$ x $<1.5 \cdot stdev$ |
| 4 | Well Above Average | x $\geq 1.5 \cdot stdev$ |

## 6a Calculation 
The function `st_dev_breaks` is called to compute the bin breaks for each indicator. These breaks determine the IPD score stored in `score`. Note that we divide *rounded* `PctEst` columns by *unrounded* half-standard deviation breaks to compute the `score`. `class` is a textual explanation of the IPD score.

```{r score_class}
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

## 6b. Result 
`score` and `class` store the IPD scores and associated descriptions for the nine indicator variables.

```{r score_class_res, warning = FALSE}
score <- as_tibble(score_matrix)
names(score) <- str_replace(names(comp$uni_est), "_UE", "_Score")
class <- as_tibble(class_matrix)
names(class) <- str_replace(names(comp$uni_est), "_UE", "_Class")
```
Here are the first few lines of `score` and `class`:

```{r score_preview}
head(score)
head(class)
```

## 6c. Composite IPD score 
Sum the IPD scores for the nine indicator variables to determine the composite IPD score.

# 7. Summary Tables 
This section generates a handful of other deliverables, including:

a. Counts by indicator
b. Breaks by indicator
c. Summary by indicator
d. County means by indicator


## 7a. Counts by indicator 
The number of census tracts that fall in each bin. Count census tracts by indicator and bin. Reorder factor levels so that "Well Below Average" appears before "Below Average," and the like.

```{r summary_counts, message = FALSE, warning = FALSE}
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
  mutate_all(funs(replace_na(., 0))) %>%
  mutate(TOTAL = rowSums(.[2:7], na.rm = TRUE))
```

## 7b. Breaks by indicator 
The bin breaks for each indicator. Apply the `st_dev_breaks` function to all percentage values and export results.

```{r summary_breaks}
breaks <- ipd_summary %>% select(ends_with("PctEst"))
export_breaks <- round(mapply(st_dev_breaks, x = breaks, i = 5, na.rm = TRUE), digits = 3)
export_breaks <- as_tibble(export_breaks) %>%
  mutate(Class = c("Min", "1", "2", "3", "4", "Max")) %>%
  select(Class, current_vars())
```

## 7c. Summary by indicator 
Summary statistics of each indicator. Round results to two decimal places.  

```{r summary_summary}
pcts <- ipd_summary %>% select(ends_with("PctEst"))
summary_data <- apply(pcts, 2, description)
export_summary <- as_tibble(summary_data) %>%
  mutate_all(round, 2) %>%
  mutate(Statistic = c("Minimum", "Median", "Mean", "SD", "Half-SD", "Maximum")) %>%
  select(Statistic, current_vars())
```

## 7d. County means by indicator 
Population-weighted means by county and indicator. For the most accurate percentage values, aggregate all counts back to the county level and compute percentages. In the export file, counties are referred to by the five-digit character supplied by the user to `ipd_counties`.

```{r summary_county, warning = FALSE, message = FALSE}
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
  mutate_if(is.numeric, funs(. * 100)) %>%
  mutate_if(is.numeric, round, 1)
```

# 8. Export 
## 8a. Append to TIGER/LINE file 
Using the arguments supplied in `ipd_county`, download the relevant census tracts and append `ipd` to them.  

```
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
```

## 8b. Export files 
Results are saved in `outputs`.

```
write.csv(ipd_table, paste(output_dir,"ipd_", ipd_year, ".csv", sep=""))
st_write(ipd_shapefile, paste(output_dir,"ipd_", ipd_year, ".shp", sep=""))  

write.csv(counts_table, paste(output_dir,"counts_by_indicator_", ipd_year, ".csv", sep=""))
write.csv(class_breaks_table, paste(output_dir,"breaks_by_indicator_", ipd_year, ".csv", sep=""))
write.csv(summary_table, paste(output_dir,"summary_by_indicator_", ipd_year, ".csv", sep=""))
write.csv(means_table, paste(output_dir,"means_by_county_", ipd_year, ".csv", sep=""))
```

# 19. Metadata table with sources 
This is a table of the final output with some additional data such as municipality name and area added through GIS processes but not included in the R script.


| Variable   | Concept                                     | acs table | acs variable | data source | Source Type | Universe Variable |
|------------|---------------------------------------------|-----------|--------------|-------------|-------------|-------------------|
| geoid20    | 11-digit tract GEOID                        | n/a       | n/a          | ACS 5-year  | n/a         | n/a               |
| statefp20  | 2-digit state GEOID                         | n/a       | n/a          | ACS 5-year  | n/a         | n/a               |
| countyfp20 | 3-digit county GEOID                       | n/a       | n/a          | ACS 5-year  | n/a         | n/a               |
| name20     | Tract and county name                      | n/a       | n/a          | ACS 5-year  | n/a         | n/a               |
| d_class    | Disabled percentile class                   | n/a       | n/a          | calculated  | calculated  | S1810_C01_001     |
| d_cntest   | Disabled count estimate                     | S1810_C02_001_E | acs variable | ACS 5-year  | acs variable | S1810_C01_001     |
| d_cntmoe   | Disabled count margin of error              | S1810_C02_001_M | acs variable | ACS 5-year  | acs variable | S1810_C01_001     |
| d_pctest   | Disabled percent estimate                   | S1810_C03_001_E | acs variable | ACS 5-year  | acs variable | S1810_C01_001     |
| d_pctile   | Disabled percentile                          | n/a       | n/a          | calculated  | calculated  | S1810_C01_001     |
| d_pctmoe   | Disabled percent margin of error            | S1810_C03_001_M | acs variable | ACS 5-year  | acs variable | S1810_C01_001     |
| d_score    | Disabled percentile score                   | n/a       | n/a          | calculated  | calculated  | S1810_C01_001     |
| em_class   | Ethnic minority percentile class            | B03002    | n/a          | calculated  | calculated  | B03002_001        |
| em_cntest  | Ethnic minority count estimate              | B03002    | B03002_012_E | ACS 5-year  | acs variable | B03002_001        |
| em_cntmoe  | Ethnic minority count margin of error       | B03002    | B03002_012_M | ACS 5-year  | acs variable | B03002_001        |
| em_pctest  | Ethnic minority percent estimate            | B03002    | n/a          | calculated  | calculated  | B03002_001        |
| em_pctile  | Ethnic minority percentile                   | B03002    | n/a          | calculated  | calculated  | B03002_001        |
| em_pctmoe  | Ethnic minority percent margin of error     | B03002    | n/a          | calculated  | calculated  | B03002_001        |
| em_score   | Ethnic minority percentile score            | B03002    | n/a          | calculated  | calculated  | B03002_001        |
| f_class    | Female percentile class                     | S0101     | n/a          | calculated  | calculated  | S0101_C01_001     |
| f_cntest   | Female count estimate                       | S0101     | S0101_C05_001_E | ACS 5-year  | acs variable | S0101_C01_001     |
| f_cntmoe   | Female count margin of error                | S0101     | S0101_C05_001_M | ACS 5-year  | acs variable | S0101_C01_001     |
| f_pctest   | Female percent estimate                     | S0101     | DP05_0003PE_E | ACS 5-year  | acs variable | S0101_C01_001     |
| f_pctile   | Female percentile                            | S0101     | n/a          | calculated  | calculated  | S0101_C01_001     |
| f_pctmoe   | Female percent margin of error              | S0101     | DP05_0003PE_M | ACS 5-year  | acs variable | S0101_C01_001     |
| f_score    | Female percentile score                     | S0101     | n/a          | calculated  | calculated  | S0101_C01_001     |
| fb_class   | Foreign-born percentile class                | B05012    | n/a          | calculated  | calculated  | B05012_001        |
| fb_cntest  | Foreign-born count estimate                  | B05012    | B05012_003_E | ACS 5-year  | acs variable | B05012_001        |
| fb_cntmoe  | Foreign-born count margin of error           | B05012    | B05012_003_M | ACS 5-year  | acs variable | B05012_001        |
| fb_pctest  | Foreign-born percent estimate                | B05012    | n/a          | calculated  | calculated  | B05012_001        |
| fb_pctile  | Foreign-born percentile                       | B05012    | n/a          | calculated  | calculated  | B05012_001        |
| fb_pctmoe  | Foreign-born percent margin of error         | B05012    | n/a          | calculated  | calculated  | B05012_001        |
| fb_score   | Foreign-born percentile score                | B05012    | n/a          | calculated  | calculated  | S1601_C01_001     |
| lep_class  | Limited English proficiency percentile class | S1601     | n/a          | calculated  | calculated  | S1601_C01_001     |
| lep_cntest | Limited English proficiency count estimate   | S1601     | S1601_C05_001_E | ACS 5-year  | acs variable | S1601_C01_001     |
| lep_cntmoe | Limited English proficiency count margin of error | S1601 | S1601_C05_001_M | ACS 5-year  | acs variable | S1601_C01_001     |
| lep_pctest | Limited English proficiency percent estimate | S1601   | S1601_C06_001_E | ACS 5-year  | acs variable | S1601_C01_001     |
| lep_pctile | Limited English proficiency percentile       | S1601   | n/a          | calculated  | calculated  | S1601_C01_001     |
| lep_pctmoe | Limited English proficiency percent margin of error | S1601 | S1601_C06_001_M | ACS 5-year  | acs variable | S1601_C01_001     |
| lep_score  | Limited English proficiency percentile score | S1601 | n/a          | calculated  | calculated  | S1601_C01_001     |
| li_class   | Low-income percentile class                 | n/a       | n/a          | calculated  | calculated  | S1701_C01_001     |
| li_cntest  | Low-income count estimate                   | S1701     | S1701_C01_042_E | ACS 5-year  | acs variable | S1701_C01_001     |
| li_cntmoe  | Low-income count margin of error            | S1701     | S1701_C01_042_M | ACS 5-year  | acs variable | S1701_C01_001     |
| li_pctest  | Low-income percent estimate                 | n/a       | n/a          | calculated  | calculated  | S1701_C01_001     |
| li_pctile  | Low-income percentile                        | n/a       | n/a          | calculated  | calculated  | S1701_C01_001     |
| li_pctmoe  | Low-income percent margin of error          | n/a       | n/a          | calculated  | calculated  | S1701_C01_001     |
| li_score   | Low-income percentile score                 | n/a       | n/a          | calculated  | calculated  | S1701_C01_001     |
| oa_class   | Older adult percentile class                | S0101     | n/a          | calculated  | calculated  | B02001_001        |
| oa_cntest  | Older adult count estimate                  | S0101     | S0101_C01_001_E | ACS 5-year  | acs variable | B02001_001        |
| oa_cntmoe  | Older adult count margin of error           | S0101     | S0101_C01_001_M | ACS 5-year  | acs variable | B02001_001        |
| oa_pctest  | Older adult percent estimate                | S0101     | S0101_C02_030_E | ACS 5-year  | acs variable | B02001_001        |
| oa_pctile  | Older adult percentile                       | S0101     | n/a          | calculated  | calculated  | B02001_001        |
| oa_pctmoe  | Older adult percent margin of error         | S0101     | S0101_C02_030_M | ACS 5-year  | acs variable | B02001_001        |
| oa_score   | Older adult percentile score                | S0101     | n/a          | calculated  | calculated  | B02001_001        |
| rm_class   | Racial minority percentile class            | B02001    | n/a          | calculated  | calculated  | B02001_001        |
| rm_cntest  | Racial minority count estimate              | B02001    | B02001_002_E | ACS 5-year  | acs variable | B02001_001        |
| rm_cntmoe  | Racial minority count margin of error       | B02001    | B02001_002_M | ACS 5-year  | acs variable | B02001_001        |
| rm_pctest  | Racial minority percent estimate            | B02001    | n/a          | calculated  | calculated  | B02001_001        |
| rm_pctile  | Racial minority percentile                   | B02001    | n/a          | calculated  | calculated  | B02001_001        |
| rm_pctmoe  | Racial minority percent margin of error     | B02001    | n/a          | calculated  | calculated  | B02001_001        |
| rm_score   | Racial minority percentile score            | B02001    | n/a          | calculated  | calculated  | B02001_001        |
| y_class    | Youth percentile class                      | B09001    | n/a          | calculated  | calculated  | B03002_001        |
| y_cntest   | Youth count estimate                        | B09001    | B09001_001  | ACS 5-year  | acs variable | B03002_001        |
| y_cntmoe   | Youth count margin of error                 | B09001    | B09001_001  | ACS 5-year  | acs variable | B03002_001        |
| y_pctest   | Youth population percentage estimate        | B09001    | n/a          | calculated  | calculated  | B03002_001        |
| y_pctile   | Youth population percentile                  | B09001    | n/a          | calculated  | calculated  | B03002_001        |
| y_pctmoe   | Youth population percentage margin of error | B09001    | n/a          | calculated  | calculated  | B03002_001        |
| y_score    | Youth percentile score                      | B09001    | n/a          | calculated  | calculated  | B03002_001        |
| ipd_score  | Indicator of potential disadvantage score   | n/a       | n/a          | calculated  | calculated  | n/a               |
| u_tpopest  | Total population estimate                   | B02001    | B02001_001_E | ACS 5-year  | acs variable | B02001_001        |
| u_tpopmoe  | Total population margin of error            | B02001    | B02001_001_E | ACS 5-year  | acs variable | B02001_001        |
| u_pop6est  | Population over 6 years of age estimate     | S1601     | S1601_C01_001_E | ACS 5-year  | acs variable | S1601_C01_001     |
| u_pop6moe  | Population over 6 years of age margin of error | S1601  | S1601_C01_001_M | ACS 5-year  | acs variable | S1601_C01_001     |
| u_ppovest  | Total population poverty rate estimate      | S1701     | S1701_C01_001_E | ACS 5-year  | acs variable | S1701_C01_001     |
| u_ppovmoe  | Total population poverty rate margin of error | S1701 | S1701_C01_001_M | ACS 5-year  | acs variable | S1701_C01_001     |
| u_pnicest  | Disabled universe total estimate            | S1810     | S1810_C01_001_E | ACS 5-year  | acs variable | S1810_C01_001     |
| u_pnicmoe  | Disabled universe total margin of error     | S1810     | S1810_C01_001_M | ACS 5-year  | acs variable | S1810_C01_001     |
| namelsad   | Geography name                              | n/a       | n/a          | ACS 5-year  | acs variable | n/a               |
| mun1       | First municipality name                     | n/a       | n/a          | calculated  | calculated  | n/a               |
| mun2       | Second municipality name                    | n/a       | n/a          | calculated  | calculated  | n/a               |
| mun3       | Third municipality name                     | n/a       | n/a          | calculated  | calculated  | n/a               |
| co_name    | County Name                                 | n/a       | n/a          | calculated  | calculated  | n/a               |
| state      | State name                                  | n/a       | n/a          | ACS 5-year  | n/a         | n/a               |
| st_area(shape) | Area of a geometry                       | n/a       | n/a          | ACS 5-year  | n/a         | n/a               |
| st_perimeter(shape) | Perimeter of the geometry           | n/a       | n/a          | ACS 5-year  | n/a         | n/a               |