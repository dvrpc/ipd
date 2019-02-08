require(tidyverse)
# Download files
nj_url <- "https://www2.census.gov/programs-surveys/acs/replicate_estimates/2017/data/5-year/140/B02001_34.csv.gz"
pa_url <- "https://www2.census.gov/programs-surveys/acs/replicate_estimates/2017/data/5-year/140/B02001_42.csv.gz"
nj_temp <- tempfile()
download.file(nj_url, nj_temp)
nj_var_rep <- read_csv(gzfile(nj_temp))
pa_temp <- tempfile()
download.file(pa_url, pa_temp)
pa_var_rep <- read_csv(gzfile(pa_temp))
# Merge and subset for DVRPC region
keep_cty <- c("34005", "34007", "34015", "34021",
              "42017", "42029", "42045", "42091", "42101")
var_rep <- bind_rows(nj_var_rep, pa_var_rep) %>%
  mutate_at(vars(GEOID), funs(str_sub(., 8, 18))) %>%
  filter(str_sub(GEOID, 1, 5) %in% keep_cty) %>%
  select(-TBLID, -NAME, -ORDER, -moe, -CME, -SE) %>%
  filter(TITLE %in% c("Black or African American alone",
                      "American Indian and Alaska Native alone",
                      "Asian alone",
                      "Native Hawaiian and Other Pacific Islander alone",
                      "Some other race alone",
                      "Two or more races:"))
# Sum up subfields
num <- var_rep %>% 
  group_by(GEOID) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  select(-GEOID)
estim <- num %>% select(estimate)
individual_replicate <- num %>% select(-estimate)
# Grab GEOIDs to append to results
id <- var_rep %>% select(GEOID) %>% distinct(.) %>% pull(.)
sqdiff_fun <- function(v, e) (v-e)^2
sqdiff <- mapply(sqdiff_fun, individual_replicate, estim) 
sum_sqdiff <- rowSums(sqdiff)
variance <- 0.05 * sum_sqdiff
moe <- round(sqrt(variance) * 1.645, 0)
# Export
export_moe <- cbind(id, moe) %>%
  as_tibble(.) %>%
  rename(GEOID = id, RM_CntMOE = moe) %>%
  write_csv(., here("outputs", "rm_moe.csv"))
