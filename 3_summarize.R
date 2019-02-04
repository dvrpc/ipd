# Dependencies
require(tidycensus); require(tidyverse); require(here)

# Functions
source("functions.R")

# Input data
ipd <- read_csv(here("outputs", "ipd.csv"))
# Replace -99999 with NA for our purposes
ipd[ipd == -99999] <- NA

# Count of tracts that fall in each bin
counts <- ipd %>% select(ends_with("Class"))
export_counts <- apply(counts, 2, function(x) plyr::count(x))
for(i in 1:length(export_counts)){
  export_counts[[i]]$var <- names(export_counts)[i]
}
export_counts <- map_dfr(export_counts, `[`, c("var", "x", "freq"))
# Format export
colnames(export_counts) <- c("Variable", "Classification", "Count")
export_counts$Classification <- factor(export_counts$Classification,
                                       levels = c("Well Below Average",
                                                  "Below Average",
                                                  "Average",
                                                  "Above Average",
                                                  "Well Above Average",
                                                  "NoData"))
export_counts <- arrange(export_counts, Variable, Classification)

# Bin break points
breaks <- ipd %>% select(ends_with("PctEst"))
export_breaks <- round(mapply(st_dev_breaks, x = breaks, i = 5, na.rm = TRUE), digits = 3)
export_breaks <- as_tibble(export_breaks)

# mean, min, max, 1 st dev
pcts <- ipd %>% select(ends_with("PctEst"))
summary_data <- apply(pcts, 2, summary)
for(i in 1:length(summary_data)){
  summary_data[[i]]$Variable <- names(summary_data)[i]
}
export_summary <- map_dfr(summary_data, `[`,
                          c("Variable", "min_val", "median_val", "mean_val", "sd_val", "max_val"))

# Population-weighted county means for each indicator
pcts <- ipd %>% select(GEOID, ends_with("PctEst"), U_TPopEst) %>%
  mutate(cty = str_sub(GEOID, 3, 5)) %>%
  select(-GEOID)
export_means <- pcts %>% group_by(cty) %>%
  summarize_all(funs(weighted.mean(., U_TPopEst, na.rm = TRUE))) %>%
  # summarize_all(funs(mean(., na.rm = TRUE))) %>%
  mutate_if(is.numeric, ~round(., 3)) %>%
  mutate(County = case_when(cty == "005" ~ "Burlington",
                            cty == "007" ~ "Camden",
                            cty == "015" ~ "Gloucester",
                            cty == "021" ~ "Mercer",
                            cty == "017" ~ "Bucks",
                            cty == "029" ~ "Chester",
                            cty == "045" ~ "Delaware",
                            cty == "091" ~ "Montgomery",
                            cty == "101" ~ "Philadelphia")) %>%
  select(County, current_vars()) %>% select(-cty)
# Export
write_csv(export_counts, here("outputs", "counts_by_indicator.csv"))
write_csv(export_breaks, here("outputs", "breaks_by_indicator.csv"))
write_csv(export_summary, here("outputs", "summary_by_indicator.csv"))
write_csv(export_means, here("outputs", "mean_by_county.csv"))
