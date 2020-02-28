# Indicators of Potential Disadvantage

This project fully automates DVRPC's Indicators of Potential Disadvantage (IPD) analysis, including data download, processing, and export. For more on IPD analysis, see [Equity Analysis for the Greater Philadelphia Region v2.0](https://www.dvrpc.org/webmaps/ipd/).

- If you're using this repository for the first time, check out [getting_started](../master/documentation/getting_started.pdf).
- The [discussion](../master/documentation/discussion.pdf) shows required calculations and important exceptions behind IPD scoring, no programming involved.
- Lastly, the [technical_reference](../master/documentation/script_reference.pdf) gets under the hood of the script in case you're curious.

All outputs are stored in the `outputs` folder, including:
- `ipd.csv`: tract-level statistics and scores for IPD's nine indicators
- `ipd.shp`: spatial version of `ipd.csv`
- `breaks_by_indicator.csv`: bin breaks by indicator
- `counts_by_indicator.csv`: census tract counts by bin and indicator
- `summary_by_indicator.csv`: basic summary stats by indicator
- `mean_by_county.csv`: population-weighted county means by indicator

This project is written to be both temporally and geographically flexible. Future edits may be made to improve this flexibility.
