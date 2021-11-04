getwd()
setwd("C:/Users/estumpne/Documents/R/NCEAS_UCD/")

library(dataRetrieval)
library(readr)
library(readxl)
library(dplyr)

startDate <- ""

endDate <- ""

parameterCd = c("00608", "00613", "00631", "00671", "62854", "70953", "62360", "00681", "50624", "63162")

# USGS discrete stations 

USGS_CAWSC_sites <- read_xlsx("USGS_CAWSC_stations.xlsx")

site_list <- (USGS_CAWSC_sites$siteNumber)

siteNumber <- site_list

#retrieve discrete USGS CAWSC nutrients 

#00608 = NH4 wf, 00613 = NO2 wf, 00631 = NO3+NO2 wf, 00671 = Ortho-PO4 wf, 62854 = TDN wf, 70953 = Chl-a, 62360 = Pheophytin, 00681 = DOC, 50624 = Abs 254 nm, 63162 = SUVA

USGS_CAWSC_discrete <- readNWISqw(siteNumber, parameterCd, startDate, endDate, expanded=TRUE, reshape=TRUE)

write_csv(USGS_CAWSC_discrete, "C:/Users/estumpne/Documents/R/NCEAS/USGS_CAWSC_discrete.csv")

#select columns=====================================================================



USGS_CAWSC_discrete_select <-select(USGS_CAWSC_discrete, 
                        agency_cd, 
                        site_no, 
                        sample_dt, 
                        sample_tm, 
                        sample_start_time_datum_cd_reported, 
                        remark_cd_00631, 
                        result_va_00631, 
                        rpt_lev_va_00631,  
                        rpt_lev_cd_00631, 
                        remark_cd_00608, 
                        result_va_00608, 
                        rpt_lev_va_00608, 
                        rpt_lev_cd_00608, 
                        remark_cd_00671, 
                        result_va_00671, 
                        rpt_lev_va_00671, 
                        rpt_lev_cd_00671, 
                        remark_cd_00613, 
                        result_va_00613, 
                        rpt_lev_va_00613, 
                        rpt_lev_cd_00613, 
                        remark_cd_62854, 
                        result_va_62854, 
                        rpt_lev_va_62854, 
                        rpt_lev_cd_62854,
                        remark_cd_70953, 
                        result_va_70953, 
                        rpt_lev_va_70953, 
                        rpt_lev_cd_70953,  
                        remark_cd_62360, 
                        result_va_62360, 
                        rpt_lev_va_62360, 
                        rpt_lev_cd_62360, 
                        remark_cd_00681, 
                        result_va_00681, 
                        rpt_lev_va_00681, 
                        rpt_lev_cd_00681, 
                        remark_cd_50624, 
                        result_va_50624, 
                        rpt_lev_va_50624, 
                        rpt_lev_cd_50624, 
                        remark_cd_63162, 
                        result_va_63162, 
                        rpt_lev_va_63162, 
                        rpt_lev_cd_63162)

#write_csv(USGS_CAWSC_discrete_select, "C:/Users/estumpne/Documents/R/NCEAS/USGS_CAWSC_discrete_select.csv")
#rename columns=====================================================================

USGS_CAWSC_discrete_clean <-rename(USGS_CAWSC_discrete_select,
                       remark_cd_NO3_NO2_mg_L = remark_cd_00631, 
                       result_va_NO3_NO2_mg_L=result_va_00631, 
                       rpt_lev_va_NO3_NO2_mg_L=rpt_lev_va_00631, 
                       rpt_lev_cd_NO3_NO2_mg_L = rpt_lev_cd_00631,
                       remark_cd_NH4_mg_L = remark_cd_00608, 
                       result_va_NH4_mg_L=result_va_00608, 
                       rpt_lev_va_NH4_mg_L=rpt_lev_va_00608, 
                       rpt_lev_cd_NH4_mg_L = rpt_lev_cd_00608,
                       remark_cd_NO2_mg_L = remark_cd_00613,
                       result_va_NO2_mg_L=result_va_00613, 
                       rpt_lev_va_NO2_mg_L=rpt_lev_va_00613, 
                       rpt_lev_cd_NO2_mg_L = rpt_lev_cd_00613,
                       remark_cd_TDN_mg_L = remark_cd_62854,
                       result_va_TDN_mg_L=result_va_62854, 
                       rpt_lev_va_TDN_mg_L=rpt_lev_va_62854, 
                       rpt_lev_cd_TDN_mg_L = rpt_lev_cd_62854,
                       remark_cd_OrthoP_mg_L = remark_cd_00671,
                       result_va_OrthoP_mg_L=result_va_00671, 
                       rpt_lev_va_OrthoP_L=rpt_lev_va_00671, 
                       rpt_lev_cd_OrthoP_mg_L = rpt_lev_cd_00671,
                       remark_cd_Chla_ugL=remark_cd_70953, 
                       result_va_Chla_ugL=result_va_70953, 
                       rpt_lev_va_Chla_ugL=rpt_lev_va_70953, 
                       rpt_lev_cd_Chla_ugL=rpt_lev_cd_70953,  
                       remark_cd_Phaeo_ugL=remark_cd_62360, 
                       result_va_Phaeo_ugL=result_va_62360, 
                       rpt_lev_va_Phaeo_ugL=rpt_lev_va_62360, 
                       rpt_lev_cd_Phaeo_ugL=rpt_lev_cd_62360, 
                       remark_cd_DOC_mgL=remark_cd_00681, 
                       result_va_DOC_mgL=result_va_00681, 
                       rpt_lev_va_DOC_mgL=rpt_lev_va_00681, 
                       rpt_lev_cd_DOC_mgL=rpt_lev_cd_00681, 
                       remark_cd_A254_nm=remark_cd_50624, 
                       result_va_A254_nm=result_va_50624, 
                       rpt_lev_va_A254_nm=rpt_lev_va_50624, 
                       rpt_lev_cd_A254_nm=rpt_lev_cd_50624, 
                       remark_cd_SUVA=remark_cd_63162, 
                       result_va_SUVA=result_va_63162, 
                       rpt_lev_va_SUVA=rpt_lev_va_63162, 
                       rpt_lev_cd_SUVA=rpt_lev_cd_63162)

write_csv(USGS_CAWSC_discrete_clean, "C:/Users/estumpne/Documents/R/NCEAS_UCD/USGS_CAWSC_discrete_clean.csv")
