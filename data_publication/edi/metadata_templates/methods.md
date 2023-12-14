# Methods 

We integrated data collected from various agencies to examine the relationship between chlorophyll and water temperature, solar radiation, flow, and inundation. Please see original data sources for metadata and associated code. Data were downloaded and integrated using R Version 4.3.0. 

- We downloaded parameters of interest (flow, inundation, solar radiation, water temperature) from original data sources (Table 1).
    + Flow
        * We obtained USGS daily mean flow data using the “readNWISdv” function from the dataRetrieval package (DeCicco et al. 2022).
        * We obtained dayflow data using the “calc_inundation” function from the inundation package (Clark & Goertler 2022). 
    + Inundation
        * We obtained inundation data using the “calc_inundation” function from the inundation package (Clark & Goertler 2022, Goertler 2022).
    + Solar radiation 
        * We obtained solar radiation data using the “download_daymet” function from the daymetr package (Hufkens et al. 2018).
    +	Water temperature
        * We obtained water temperature data from Goertler & Pien (2022). 
- We further processed data and created a full dataset for covariates.
    + We used the “rollapply” function from the zoo package (Zeileis & Grothendiek 2005) to calculate weekly mean water temperature and solar radiation. 
    + Missing data 
        *	We filled missing flow data at Sacramento River at Rio Vista using linear regression with an upstream station (USGS-11447905, Sacramento River below Georgiana Slough; R2 = 0.938). For any remaining missing data, values were imputed with the "imputeTS"" R package (Moritz and Bartz-Beielstein 2017). 
        * Missing solar radiation data were filled with the “fill” function from the tidyr package when data were missing (Wickham & Girlich 2023). 
    + We assigned each data point to the designated region (see pdf metadata). 
- We downloaded chlorophyll a data from original data sources (Table 1), and assigned each station to the designated region. When there was more than one chlorophyll value for a given station on a given date (8 instances), we used the mean of the values. 
- We joined covariates to the chlorophyll-a data, thus removing dates when chlorophyll-a data were not available.
- We filtered the dataset to March 1999 to December 2019 to cover the period of data availability.
- We filtered the dataset to period of potential inundation (December 5 to May 2) based on historical dates of inundation.
- Processing code are available at: [https://github.com/Delta-Stewardship-Council/swg-21-connectivity/tree/main/data_publication/R](https://github.com/Delta-Stewardship-Council/swg-21-connectivity/tree/main/data_publication/R)


**Table 1.** Summary of Data Sources and Metrics in Integrated Dataset. 

| Data type	| Metric | Dataset |	Description of dataset | Reference |
| --------- | ------- | --------- | ---------------------- | --------- |
| Chlorophyll-a	| Chlorophyll-a (µg/L) | Yolo Bypass Fish Monitoring Program | Discrete chlorophyll data |	Adams and Pien 2023 |
| Chlorophyll-a	| Chlorophyll-a (µg/L)	| Discrete Water Quality Integrated Dataset |	Discrete chlorophyll from 16 monitoring programs  | Bashevkin et al. 2023 |
| Flow |	Daily mean flow (cfs)	| USGS flow data | Continuous discharge (parameter 00060) from USGS 11445500 (Sacramento River at Verona) | USGS 2023 |
| Flow	| Daily mean flow (cfs)	| USGS flow data	| Continuous tidally filtered discharge (parameter 72137) from USGS 11455420 (Sacramento River at Rio Vista)	| USGS 2023 |
| Flow | Daily mean QYOLO (cfs) | Dayflow | Modeled Yolo Bypass flow	| CDWR 2022 |
| Solar radiation |	Mean weekly solar radiation	| Daymet | Simulated daily solar radiation | Hufkens et al. 2018 |
| Water temperature	| Mean weekly water temperature	| Yolo Bypass water temperature dataset	| Continuous and discrete water temperature from the Yolo Bypass, Rio Vista and Sacramento River at Sherwood Harbor	| Goertler & Pien 2022 |
| Inundation | Inundation duration factor (no inundation, short inundation <21 days, long inundation >= 21 days) | Yolo Bypass Inundation | Inundation metrics based on stage height at Fremont Weir and modeled flow data |	Clark & Goertler 2022, Goertler 2022 |

## Methods References

* Adams, J. and C. Pien. 2023. Interagency Ecological Program: Discrete water quality and phytoplankton data from the Sacramento River floodplain and Yolo Bypass tidal slough, collected by the Yolo Bypass Fish Monitoring Program, 1998 - 2022 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/5791d7eaca09fb9471c5589c66f86863.

* Bashevkin, S.M., D. Bosworth, S.E. Perry, E.B. Stumpner, and R. Hartman. 2023. Six decades (1959-2022) of water quality in the upper San Francisco Estuary: an integrated database of 16 discrete monitoring surveys in the Sacramento San Joaquin Delta, Suisun Bay, Suisun Marsh, and San Francisco Bay ver 7. Environmental Data Initiative. https://doi.org/10.6073/pasta/8dbd29c8c22f3295bbc5d3819fb51d00.

* California Department of Water Resources (CDWR). 2023. Dayflow data available on the World Wide Web, accessed in 2023. https://data.cnra.ca.gov/dataset/dayflow.

* Clark J, Goertler P (2022). _inundation: Calculate Delta Inundation Metrics_. R package version 0.1.0. https://zenodo.org/records/6450272 

* De Cicco, L.A., Hirsch, R.M., Lorenz, D., Watkins, W.D., Johnson, M., 2022, dataRetrieval: R packages for discovering and retrieving water data available from Federal hydrologic web services, v.2.7.12, doi:10.5066/P9X4L3GE

* Goertler, P. 2022. Modeled daily Yolo Bypass inundation ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/d57a74bcff69ed673bfac1de994dd9d2.

* Goertler, P. and C. Pien. 2022. Daily water temperature (C) in the Yolo Bypass and Sacramento River, 1998-2019 ver 2. Environmental Data Initiative. https://doi.org/10.6073/pasta/5d84e5b8ea74dd0854d4aba1e4a6122d.

* Hufkens et al. (2018). An integrated phenology modelling framework in R: modelling vegetation phenology with phenor Methods in Ecology & Evolution, 9(2), 1-10.

* Moritz S, Bartz-Beielstein T (2017). “imputeTS: Time Series Missing Value Imputation in R.” _The R Journal_,*9*(1), 207-218. doi:10.32614/RJ-2017-009 <https://doi.org/10.32614/RJ-2017-009>.

* U.S. Geological Survey (USGS). 2023, National Water Information System data available on the World Wide Web (Water Data for the Nation), accessed in 2023. http://waterdata.usgs.gov/nwis/.

* Wickham H, Vaughan D, Girlich M (2023). _tidyr: Tidy Messy Data_. R package version 1.3.0, <https://CRAN.R-project.org/package=tidyr>.


			
