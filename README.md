# Indicators of Potential Disadvantage

This project automates DVRPC's Indicators of Potential Disadvantage (IPD) analysis, including data download, processing, and export. For more on IPD analysis, see [Equity Analysis for the Greater Philadelphia Region v2.0](https://www.dvrpc.org/webmaps/ipd/).

## Getting the Code and Software

1. Clone the repository. 
2. Download and install R from https://www.r-project.org/
3. Download and install R Studio from https://www.rstudio.com/products/rstudio/#Desktop

## Installing Package Dependencies 

The R script has the following dependencies: 

- plyr
- here
- sf
- summarytools
- tidycensus
- tidyverse
- tigris

If you have not previously installed the dependencies, you will need to do so. If you try to run the script without installing the packages, you will get an error message like 
`Error in library (name_of_package) : there is no package called 'name_of_package'`.

Install each package from R Studio's console (typically at the bottom of the screen in R Studio) with the command  `install.packages('name_of_package')` (include the quotation marks). 

## Updating the Script for a New 5-Year Dataset

If you are running the code against a newly released 5-year ACS dataset, do the following:

1. Make a copy of the latest .R file (e.g. script-2018.R) and rename it for the year you are working on. (This is to ensure that any schema changes for a particular 5-year dataset are kept with the code for that set.)
2. Adjust the value for the `ipd_year` variable (to be the end year of the dataset).
3. Verify the field names (listed under the `# Fields` section). Follow the link provided to check the schema for that dataset.

## Running the Code

1. Open RStudio. 
2. Open the R file (File -> Open File)
3. Run the code by clicking the Source button or Ctrl+A followed by Ctrl+Enter. 
 
If you see an error about packages not being installed, see [Installing Package Dependencies](#installing-package-dependencies) above. 

If for some reason the API key currently included in the code doesn't work (this is required for the `tidycensus` package, not the Census API), you may get another one [here](https://api.census.gov/data/key_signup.html).

### Outputs 

After the code has finished, outputs are saved in the /outputs subdirectory of where you cloned the repository on your local machine, including:
- `ipd.csv`: tract-level statistics and scores for IPD's nine indicators
- `ipd.shp`: spatial version of `ipd.csv`
- `breaks_by_indicator.csv`: bin breaks by indicator
- `counts_by_indicator.csv`: census tract counts by bin and indicator
- `summary_by_indicator.csv`: basic summary stats by indicator
- `mean_by_county.csv`: population-weighted county means by indicator

## Additional Information
- [documentation/discussion.pdf](https://github.com/dvrpc/ipd/blob/master/documentation/discussion.pdf) shows the essential math required to compute IPD scores. 
- [documentation/script_reference.pdf](https://github.com/dvrpc/ipd/blob/master/documentation/script_reference.pdf) is a companion document to the script and explains the way the script downloads data and implements IPD analysis, code chunk by code chunk.
