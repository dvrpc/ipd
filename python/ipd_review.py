import pandas as pd
import numpy as np

# Original IPD
ipd_original = pd.read_csv(
    "G:\\My Drive\\ipd_review\\original_ipd_scores.csv", dtype={"GEOID20": str}
)

# IPD review
ipd_review = pd.read_csv(
    "G:\\My Drive\\ipd_review\\test_table.csv",
    dtype={"GEOID": str, "ipd_score": np.int64},
)


# Join Tables
combined_table = ipd_original.merge(
    ipd_review, how="inner", left_on="GEOID20", right_on="GEOID"
)

combined_table = combined_table[
    [
        "GEOID",
        "GEOID20",
        "IPD_Score",
        "ipd_score",
        "LEP_Score",
        "lep_pct_score",
        "D_Score",
        "dis_pct_score",
        "EM_Score",
        "eth_pct_score",
        "F_Score",
        "fem_pct_score",
        "FB_Score",
        "fbo_pct_score",
        "LI_Score",
        "inc_pct_score",
        "OA_Score",
        "old_pct_score",
        "RM_Score",
        "rac_pct_score",
        "Y_Score",
        "you_pct_score",
    ]
]

combined_table["ipd_score"] = combined_table["ipd_score"].astype(np.int64)

combined_table.to_csv("G:\\My Drive\\ipd_review\\python_output.csv")
