### crab
crabreport_jess.Rmd generates my favorite/final version of the blue crab report update

### LINDY
Lindy's SAS index code converted to R. How to...

### blkdrum
`StandardizeIndexGLM2_PSEGBlackDrum-jess.R` is my code for the black drum index, that runs off my data pulls from PSEG. A YOY index of abundance from 1995 onward was developed from this survey. Length data was only available for 56.5% of the black drum caught in through 2020, but only 4 of 1000 fish in that time period were greater than 300mm TL (which were removed from the data set), so all data are assumed to track YOY abundance. Stations north of the confluence with the Salem River were excluded from the data set since their sampling was suspended in 2016, and only 3 black drum were captured at these stations during the time series through 2020. A negative binomial GLM was used to develop the index of abundance. The unit of effort was black drum caught per net set. Year, month, and state were included in the final GLM as factors. There were no patterns in residuals. The dispersion parameter is 1.25. The standardized index showed high interannual variability, with no clear trend over the time series.

### mrip
`inhouse_outlier_id.R` is my script for our in-house process of generating outputs to identify MRIP outliers. Need to update the wave and prelim year for a given run
