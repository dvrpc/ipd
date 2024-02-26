# Original Scores
original_ipd = read.csv("G:\\My Drive\\ipd_review\\original_ipd_scores.csv")

# Review Scores
review_table = read.csv("G:\\My Drive\\ipd_review\\test_table.csv")

# Merge Tables
combined_table <- original_ipd %>%
  left_join(review_table, by = c("GEOID20", "GEOID")) %>%
  mutate(test = ifelse(IPD_Score == ipd_score, "", 1)) %>%
  select(GEOID20, GEOID, LEP_PctEst, lep_pct, LEP_Score, lep_pct_score, test) %>%
  filter(LEP_Score != lep_pct_score)

combined_df <- left_join(x = original_ipd, y = review_table, by.x = "GEOID20", by.y = "GEOID") %>%
  select(GEOID20, GEOID) %>%
  filter(GEOID20 != GEOID)

# # Compare Scores
# results <- combined_table %>%
#   mutate(difference = IPD_Score - ipd_score) %>%
#   filter(difference != 0) %>%
#   select(GEOID20, IPD_Score, ipd_score, difference)
# 

test_frame <- combined_table %>%
  select(GEOID20, LEP_PctEst, lep_pct, LEP_Score, lep_pct_score)



# 
# # Below - LEP review
# below_review <- review_ipd %>%
#   select(ends_with("_score")) %>%
#   filter(lep_pct_score == 1)
# 
# below_original <- original_ipd %>%
#   select(LEP_Score) %>%
#   filter(LEP_Score == 1)
# 
# # Average - LEP review
# average_review <- review_ipd %>%
#   select(ends_with("_score")) %>%
#   filter(lep_pct_score == 2)
# 
# average_original <- original_ipd %>%
#   select(LEP_Score) %>%
#   filter(LEP_Score == 2)
# 
# # Above Average - LEP review
# above_average_review <- review_ipd %>%
#   select(ends_with("_score")) %>%
#   filter(lep_pct_score == 3)
# 
# above_average_original <- original_ipd %>%
#   select(LEP_Score) %>%
#   filter(LEP_Score == 3)
# 
# # Well Above Average - LEP review
# well_above_average_review <- review_ipd %>%
#   select(ends_with("_score")) %>%
#   filter(lep_pct_score == 4)
# 
# well_above_average_original <- original_ipd %>%
#   select(LEP_Score) %>%
#   filter(LEP_Score == 4)