require(tidycensus); require(tidyverse); require(sf); require(here)
# Using DC right now for testing -- speedy DLs

summary_tables <- get_acs(geography = "tract", state = "DC",
                          variables = c(LI_U = "S1701_C01_001",
                                        LI_C = "S1701_C01_042",
                                        F_U = "S0101_C01_001",
                                        F_C = "S0101_C03_001",
                                        D_U = "S1810_C01_001",
                                        D_C = "S1810_C02_001",
                                        D_P = "S1810_C03_001",
                                        OA_U = "S1601_C01_001",
                                        LEP_U = "S1601_C01_001",
                                        LEP_C = "S1601_C05_001",
                                        LEP_P = "S1601_C06_001"))
detailed_tables <- get_acs(geography = "tract", state = "DC",
                           variables = c(EM_U = "B03002_001",
                                         EM_C = "B03002_012",
                                         Y_U = "B03002_001",
                                         Y_C = "B09001_001",
                                         FB_U = "B05012_001",
                                         FB_C = "B05012_003"))
data_profiles <- get_acs(geography = "tract", state = "DC",
                         variables = c("DP05_0003PE",
                                       "DP05_0025E")) %>%
  mutate(TEMP = if_else(variable == "DP05_0003P", "F_P", "OA_C")) %>%
  select(-variable) %>% rename(variable = TEMP)

dl <- bind_rows(summary_tables, detailed_tables) %>%
  bind_rows(., data_profiles) %>%
  select(-NAME)

dl %>% filter(GEOID == "11001006804")
