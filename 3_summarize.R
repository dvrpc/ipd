# Dependencies
require(tidycensus); require(tidyverse); require(here)
# Functions
st_dev_breaks <- function(x, i, na.rm = TRUE){
  half_st_dev_count <- c(-1 * rev(seq(1, i, by = 2)),
                         seq(1, i, by = 2))
  if((i %% 2) == 1) {
    half_st_dev_breaks <- unlist(lapply(half_st_dev_count,
                                        function(i) (0.5 * i * sd(x, na.rm = TRUE)) + mean(x, na.rm = TRUE)))
    half_st_dev_breaks[[1]] <- ifelse(min(x, na.rm = TRUE) < half_st_dev_breaks[[1]],
                                      min(x, na.rm = TRUE), half_st_dev_breaks[[1]])
    half_st_dev_breaks[[i + 1]] <- ifelse(max(x, na.rm = TRUE) > half_st_dev_breaks[[i + 1]],
                                          max(x, na.rm = TRUE), half_st_dev_breaks[[i + 1]])
  } else {
    half_st_dev_breaks <- NA
  }
  return(half_st_dev_breaks)
}
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
colnames(export_counts) <- c("Variable", "Classification", "Count")
# Bin break points, mean, min, max, 1 st dev
breaks <- ipd %>% select(ends_with("PctEst"))
export_breaks <- round(mapply(st_dev_breaks, x = breaks, i = 5, na.rm = TRUE), digits = 3)
export_breaks <- as_tibble(export_breaks)
# County means for each indicator 
pcts <- ipd %>% select(GEOID, ends_with("PctEst")) %>%
  mutate(cty = str_sub(GEOID, 3, 5)) %>%
  select(-GEOID)
export_means <- pcts %>% group_by(cty) %>% summarize_all(funs(mean(., na.rm = TRUE))) %>%
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
write_csv(export_means, here("outputs", "mean_by_county.csv"))
