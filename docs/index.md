## Background
DVRPC's [Indicators of Potential Disadvantage](https://www.dvrpc.org/webmaps/ipd/#home) (IPD) identify populations of interest under Title VI of the Civil Rights Act and the Executive Order on Environmental Justice (#12898) using American Community Survey (ACS) five-year estimates from the U.S. Census Bureau. IPD analysis assists both DVRPC and outside organizations in equity work by identifying populations of interest, including youth, older adults, female, racial minority, ethnic minority, foreign-born, limited English proficiency, disabled, and low-income populations at the census tract level in DVRPC's nine-county region.

Below is an overview of the calculations required to conduct IPD analysis and is meant to provide enough information to replicate prior years of IPD analysis. 

## IPD Indicators and ACS Data
| IPD Indicator | ACS Data Table | Protected Class Representation | Authorizing Source/Guiding Document |
| ------------- | -------------- | ------------------------------ | ----------------------------------- |
| Youth | B09001: Population Under 18 Years by Age | Age | FHWA's Title VI Program and Related Authorities: 23 CFR 200 |
| Older Adults | S0101: Age and Sex | Age | FHWA's Title VI Program and Related Authorities: 23 CFR 200 |
| Female | S0101: Age and Sex | Sex | FHWA's Title VI Program and Related Authorities: 23 CFR 200 |
| Racial Minority | B02001: Race | Race and Minority | Executive Order 12898, Title VI of the Civil Rights Act of 1964, FHWA's Title VI Program and Related Authorities: 23 CFR, and Title VI Requirements and Guidelines for FTA Recipients |
| Ethnic Minority | B03002: Hispanic or Latino Origin by Race | Minority and National Origin | Executive Order 12898, Title VI of the Civil Rights Act of 1964, FHWA's Title VI Program and Related Authorities: 23 CFR, and Title VI Requirements and Guidelines for FTA Recipients |
| Foreign-Born | B05012: Nativity in the United States | National Origin | Title VI of the Civil Rights Act of 1964, FHWA's Title VI Program and Related Authorities: 23 CFR, and Title VI Requirements and Guidelines for FTA Recipients |
| Limited English Proficiency | S1601: Language Spoken at Home | Limited English Proficiency and National Origin | Title VI of the Civil Rights Act of 1964, FHWA's Title VI Program and Related Authorities: 23 CFR, and Title VI Requirements and Guidelines for FTA Recipients |
| Disabled | S1810: Disability Characteristics | Disability | FHWA's Title VI Program and Related Authorities: 23 CFR |
| Low-Income | S1701: Poverty Status in Past 12 Months | Low-Income | Executive Order 12898 and FHWA's Title VI Program and Related Authorities: 23 CFR 200 |

## IPD Score
IPD Scores can be presented in two ways: individual IPD score and composite IPD score.

### Individual IPD Score
For each indicator, percent estimates are split into five bins, which are detailed in the table below. There is one exception to the standard deviation classification: if `mean(pop) - (1.5 * stdev(pop))` is a negative value, it is manually reassigned to 0.1. This ensures that at least some census tracts fall in the bottom bin regardless of the spread of the indicator. 

`p` = percent estimate  
`mean(pop)` = mean for indicator population  
`stdev(pop)` = standard deviation for indicator population  

| IPD Score | IPD Classification | Standard Deviations |
|:---------:|:------------------:|:-------------------:|
| 0 | Well Below Average | p < mean(pop) - (1.5 * stdev(pop)) |
| 1 | Below Average | mean(pop) - (1.5 * stdev(pop)) <= p <  mean(pop) - (0.5 * stdev(pop))|
| 2 | Average | mean(pop) - (0.5 * stdev(pop)) <= p < mean(pop) + (0.5 * stdev(pop))|
| 3 | Above Average | mean(pop) + (0.5 * stdev(pop)) <= p < mean(pop) + (1.5 * stdev(pop))|
| 4 | Well Above Average | p >= mean(pop) + (1.5 * stdev(pop))|

### Composite IPD Score
The composite IPD score is computed by summing the individual IPD scores. In theory, the composite IPD score can range from 0 to 36, since each indicator's IPD score can be as high as 4. In practice, the mean composite score in 2021 is 17.57, and the highest observed composite score is 32.

