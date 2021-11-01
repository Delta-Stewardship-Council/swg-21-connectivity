getwd()

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(padr)

#remove scientific notation
options(scipen = 100)

#read in data pull

discrete <- read_csv("USGS_CAWSC_discrete.csv")

#select columns

discrete_select <-discrete %>% 
select(startDateTime, tz_cd, site_no, remark_cd_00608, result_va_00608, dqi_cd_00608, rpt_lev_va_00608, rpt_lev_cd_00608, remark_cd_00613, result_va_00613, dqi_cd_00613, rpt_lev_va_00613, rpt_lev_cd_00613, remark_cd_00631, result_va_00631, dqi_cd_00631, rpt_lev_va_00631, rpt_lev_cd_00631, remark_cd_00671, result_va_00671, dqi_cd_00671, rpt_lev_va_00671, rpt_lev_cd_00671, remark_cd_62854, result_va_62854, dqi_cd_62854, rpt_lev_va_62854, rpt_lev_cd_62854, remark_cd_70953, result_va_70953, dqi_cd_70953, rpt_lev_va_70953, rpt_lev_cd_70953, remark_cd_62360, result_va_62360, dqi_cd_62360, rpt_lev_va_62360, rpt_lev_cd_62360, remark_cd_00681, result_va_00681, dqi_cd_00681, rpt_lev_va_00681, rpt_lev_cd_00681, remark_cd_50624, result_va_50624, dqi_cd_50624, rpt_lev_va_50624, rpt_lev_cd_50624, remark_cd_63162, result_va_63162, dqi_cd_63162, rpt_lev_va_63162, rpt_lev_cd_63162)

#id p-codes

#00608 = NH4 wf, 00613 = NO2 wf, 00631 = NO3+NO2 wf, 00671 = Ortho-PO4 wf, 62854 = TDN wf, 70953 = Chl-a, 62360 = Pheophytin, 00681 = DOC, 50624 = Abs 254 nm, 63162 = SUVA

#rename columns

discrete_rename <- discrete_select %>%
dplyr::rename(NH4_mgL_remark_cd = remark_cd_00608) %>%
  dplyr::rename(NH4_mgL = result_va_00608) %>%
  dplyr::rename(NH4_mgL_dqi_cd = dqi_cd_00608) %>%
  dplyr::rename(NH4_mgL_rpt_level_va = rpt_lev_va_00608) %>%
  dplyr::rename(NH4_mgL_rpt_lev_cd = rpt_lev_cd_00608) %>%
  dplyr::rename(NO2_mgL_remark_cd = remark_cd_00613) %>%
  dplyr::rename(NO2_mgL = result_va_00613) %>%
  dplyr::rename(NO2_mgL_dqi_cd = dqi_cd_00613) %>%
  dplyr::rename(NO2_mgL_rpt_lev_va = rpt_lev_va_00613) %>%
  dplyr::rename(NO2_mgL_rpt_lev_cd = rpt_lev_cd_00613) %>%
  dplyr::rename(NO3_NO2_mgL_remark_cd = remark_cd_00631) %>%
  dplyr::rename(NO3_NO2_mgL = result_va_00631) %>%
  dplyr::rename(NO3_NO2_mgL_dqi_cd = dqi_cd_00631) %>%
  dplyr::rename(NO3_NO2_mgL_rpt_lev_va = rpt_lev_va_00631) %>%
  dplyr::rename(NO3_NO2_mgL_rpt_lev_cd = rpt_lev_cd_00631) %>%
  dplyr::rename(Ortho_PO4_mgL_remark_cd = remark_cd_00671) %>%
  dplyr::rename(Ortho_PO4_mgL = result_va_00671) %>%
  dplyr::rename(Ortho_PO4_mgL_dqi_cd = dqi_cd_00671) %>%
  dplyr::rename(Ortho_PO4_mgL_rpt_lev_va = rpt_lev_va_00671) %>%
  dplyr::rename(Ortho_PO4_mgL_rpt_lev_cd = rpt_lev_cd_00671) %>%
  dplyr::rename(TDN_mgL_remark_cd = remark_cd_62854) %>%
  dplyr::rename(TDN_mgL = result_va_62854) %>%
  dplyr::rename(TDN_mgL_dqi_cd = dqi_cd_62854) %>%
  dplyr::rename(TDN_mgL_rpt_lev_va = rpt_lev_va_62854) %>%
  dplyr::rename(TDN_mgL_rpt_lev_cd = rpt_lev_cd_62854) %>%
  dplyr::rename(Chla_mgL_remark_cd = remark_cd_70953) %>%
  dplyr::rename(Chla_mgL = result_va_70953) %>%
  dplyr::rename(Chla_mgL_dqi_cd = dqi_cd_70953) %>%
  dplyr::rename(Chla_mgL_rpt_lev_va = rpt_lev_va_70953) %>%
  dplyr::rename(Chla_mgL_rpt_lev_cd = rpt_lev_cd_70953) %>%
  dplyr::rename(Phaeo_mgL_remark_cd = remark_cd_62360) %>%
  dplyr::rename(Phaeo_mgL = result_va_62360) %>%
  dplyr::rename(Phaeo_mgL_dqi_cd = dqi_cd_62360) %>%
  dplyr::rename(Phaeo_mgL_rpt_lev_va = rpt_lev_va_62360) %>%
  dplyr::rename(Phaeo_mgL_rpt_lev_cd = rpt_lev_cd_62360) %>%
  dplyr::rename(DOC_mgL_remark_cd = remark_cd_00681) %>%
  dplyr::rename(DOC_mgL = result_va_00681) %>%
  dplyr::rename(DOC_mgL_dqi_cd = dqi_cd_00681) %>%
  dplyr::rename(DOC_mgL_rpt_lev_va = rpt_lev_va_00681) %>%
  dplyr::rename(DOC_mgL_rpt_lev_cd = rpt_lev_cd_00681) %>%
  dplyr::rename(A254_remark_cd = remark_cd_50624) %>%
  dplyr::rename(A254 = result_va_50624) %>%
  dplyr::rename(A254_dqi_cd = dqi_cd_50624) %>%
  dplyr::rename(A254_rpt_lev_va = rpt_lev_va_50624) %>%
  dplyr::rename(A254_rpt_lev_cd = rpt_lev_cd_50624) %>%
  dplyr::rename(SUVA_remark_cd = remark_cd_63162) %>%
  dplyr::rename(SUVA = result_va_63162) %>%
  dplyr::rename(SUVA_dqi_cd = dqi_cd_63162) %>%
  dplyr::rename(SUVA_rpt_lev_va = rpt_lev_va_63162) %>%
  dplyr::rename(SUVA_rpt_lev_cd = rpt_lev_cd_63162)
  

# select columns

# select NH4 only

discrete_NH4 <-discrete_rename %>% 
  select(startDateTime, site_no, tz_cd, NH4_mgL_remark_cd, NH4_mgL, NH4_mgL_dqi_cd, NH4_mgL_rpt_level_va, NH4_mgL_rpt_lev_cd)

#drop unapproved NH4

discrete_NH4_R <- discrete_NH4  %>% 
  filter(NH4_mgL_dqi_cd %in% c("R", "NA", "A"))

# select NO2 only

discrete_NO2 <-discrete_rename %>% 
  select(startDateTime, site_no, tz_cd, NO2_mgL_remark_cd, NO2_mgL, NO2_mgL_dqi_cd, NO2_mgL_rpt_lev_va, NO2_mgL_rpt_lev_cd)

#drop unapproved NO2

discrete_NO2_R <- discrete_NO2  %>% 
  filter(NO2_mgL_dqi_cd %in% c("R", "NA", "A"))

# select NO3_NO2 only

discrete_NO3_NO2 <-discrete_rename %>% 
  select(startDateTime, site_no, tz_cd, NO3_NO2_mgL_remark_cd, NO3_NO2_mgL, NO3_NO2_mgL_dqi_cd, NO3_NO2_mgL_rpt_lev_va, NO3_NO2_mgL_rpt_lev_cd)

#drop unapproved NO3_NO2

discrete_NO3_NO2_R <- discrete_NO3_NO2  %>% 
  filter(NO3_NO2_mgL_dqi_cd %in% c("R", "NA", "A"))

# select Ortho-PO4 only

discrete_Ortho_PO4 <-discrete_rename %>% 
  select(startDateTime, site_no, tz_cd, Ortho_PO4_mgL_remark_cd, Ortho_PO4_mgL, Ortho_PO4_mgL_dqi_cd, Ortho_PO4_mgL_rpt_lev_va, Ortho_PO4_mgL_rpt_lev_cd)

#drop unapproved Ortho_PO4

discrete_Ortho_PO4_R <- discrete_Ortho_PO4 %>% 
  filter(Ortho_PO4_mgL_dqi_cd %in% c("R", "NA", "A"))

# select TDN only

discrete_TDN <-discrete_rename %>% 
  select(startDateTime, site_no, tz_cd, TDN_mgL_remark_cd, TDN_mgL, TDN_mgL_dqi_cd, TDN_mgL_rpt_lev_va, TDN_mgL_rpt_lev_cd)

#drop unapproved TDN

discrete_TDN_R <- discrete_TDN  %>% 
  filter(TDN_mgL_dqi_cd %in% c("R", "NA", "A"))

# select Chla only

discrete_Chla <-discrete_rename %>% 
  select(startDateTime, site_no, tz_cd, Chla_mgL_remark_cd, Chla_mgL, Chla_mgL_dqi_cd, Chla_mgL_rpt_lev_va, Chla_mgL_rpt_lev_cd)

#drop unapproved Chla

discrete_Chla_R <- discrete_Chla  %>% 
  filter(Chla_mgL_dqi_cd %in% c("R", "NA", "A"))

# select Phaeo only

discrete_Phaeo <-discrete_rename %>% 
  select(startDateTime, site_no, tz_cd, Phaeo_mgL_remark_cd, Phaeo_mgL, Phaeo_mgL_dqi_cd, Phaeo_mgL_rpt_lev_va, Phaeo_mgL_rpt_lev_cd)

#drop unapproved Phaeo

discrete_Phaeo_R <- discrete_Phaeo  %>% 
  filter(Phaeo_mgL_dqi_cd %in% c("R", "NA"))

# select DOC only

discrete_DOC <-discrete_rename %>% 
  select(startDateTime, site_no, tz_cd, DOC_mgL_remark_cd, DOC_mgL, DOC_mgL_dqi_cd, DOC_mgL_rpt_lev_va, DOC_mgL_rpt_lev_cd)

#drop unapproved DOC

discrete_DOC_R <- discrete_DOC  %>% 
  filter(DOC_mgL_dqi_cd %in% c("R", "NA"))

# select A254 only

discrete_A254 <-discrete_rename %>% 
  select(startDateTime, site_no, tz_cd, A254_remark_cd, A254, A254_dqi_cd, A254_rpt_lev_va, A254_rpt_lev_cd)

#drop unapproved A254

discrete_A254_R <- discrete_A254  %>% 
  filter(A254_dqi_cd %in% c("R", "NA"))

# select SUVA only

discrete_SUVA <-discrete_rename %>% 
  select(startDateTime, site_no, tz_cd, SUVA_remark_cd, SUVA, SUVA_dqi_cd, SUVA_rpt_lev_va, SUVA_rpt_lev_cd)

#drop unapproved A254

discrete_SUVA_R <- discrete_SUVA  %>% 
  filter(SUVA_dqi_cd %in% c("R", "NA"))

#join all data

USGS_CAWSC_approved_WQ_1 <- full_join(discrete_NH4_R, discrete_NO3_NO2_R, by="startDateTime", "site_no")

USGS_CAWSC_approved_WQ_2 <- full_join(discrete_NO2_R, discrete_Ortho_PO4_R, by="startDateTime", "site_no")

USGS_CAWSC_approved_WQ_3 <- full_join(discrete_TDN_R,  discrete_Chla_R, by="startDateTime", "site_no")

USGS_CAWSC_approved_WQ_4 <- full_join(discrete_Phaeo_R, discrete_DOC_R, by="startDateTime", "site_no")

USGS_CAWSC_approved_WQ_5 <- full_join(discrete_A254_R, discrete_SUVA_R, by="startDateTime", "site_no")

USGS_CAWSC_approved_WQ_1_2 <- full_join(USGS_CAWSC_approved_WQ_1, USGS_CAWSC_approved_WQ_2, by="startDateTime", "site_no")

USGS_CAWSC_approved_WQ_3_4 <- full_join(USGS_CAWSC_approved_WQ_3, USGS_CAWSC_approved_WQ_4, by="startDateTime", "site_no")

USGS_CAWSC_approved_WQ_1_2_5 <- full_join(USGS_CAWSC_approved_WQ_1_2, USGS_CAWSC_approved_WQ_5, by="startDateTime", "site_no")

USGS_CAWSC_approved_WQ <- full_join(USGS_CAWSC_approved_WQ_1_2_5, USGS_CAWSC_approved_WQ_3_4, by="startDateTime", "site_no")

#rename site_no and tz cols

#USGS_CAWSC_approved_WQ <- USGS_CAWSC_approved_WQ %>%
  dplyr::rename(site_no = site_no.x.x.x) %>%
  dplyr::rename(tz_cd = tz_cd.x.x.x)
  
#drop extra site_no and tz cols - this is leading to NAs in the site_no columns. should I incorporate an IFNULL statement so the site_no is never NA
USGS_CAWSC_approved_WQ_discrete <- subset (USGS_CAWSC_approved_WQ, select = -c (site_no.y.y.x, tz_cd.y.y.x, site_no.x.y.x, tz_cd.x.y.x, site_no.x, tz_cd.x, site_no.y, tz_cd.y, site_no.x.x.y, tz_cd.x.x.y, site_no.y.y.x, tz_cd.y.y.x, site_no.x.y.y, tz_cd.x.y.y, site_no.y.x.y, tz_cd.y.x.y, site_no.y.y.y, tz_cd.y.y.y))


#exploring ways to drop unapproved data

discrete_Dave <-discrete %>% 
  select(startDateTime, site_no, result_va_00608, dqi_cd_00608, result_va_00631, dqi_cd_00631)


levs(discrete_Dave$dqi_cd_00631)

str(discrete_Dave$dqi_cd_00631)

discrete_Dave_R_approved_00608 <- discrete_Dave %>% 
  filter(dqi_cd_00608 %in% c("R", "NA"))


?levels

tail(discrete_Dave)

str(discrete_Dave)

# check NH4 values [00608], S = provisional, R = approved

dqiR_00608 = filter(discrete,dqi_cd_00608=="R")

dqiS_00608 = filter(discrete,dqi_cd_00608=="S")

# check NO3+NO2 values [00631], S = provisional, R = approved

dqiR_00631 = filter(discrete,dqi_cd_00608=="R")

dqiS_00631 = filter(discrete,dqi_cd_00608=="S")








#=============================================================



# check NO3+NO2 values [00631], S = provisional, R = approved

dqiR_NO3 = filter(discrete,dqi_cd_00608=="R")

dqiS_NO3 = filter(discrete,dqi_cd_00608=="S")#check Chl-a values [70953], S = provisional, R = approved

dqiR_Chl = filter(discrete,dqi_cd_70953=="R")

dqiS_Chl = filter(discrete,dqi_cd_70953=="S")

discrete_Chl_select <- read_csv("USGS_CAWSC_discrete.csv") %>% 
  filter(!is.na(result_va_70953)) %>%
  group_by(site_no, dqi_cd_70953) %>%
  summarize(n=n())



# check Phaeophytin values [00681], S = provisional, R = approved

dqiR_phaeo = filter(discrete,dqi_cd_00681=="R")

dqiS_phaeo = filter(discrete,dqi_cd_62360=="S")



# check OrthoP values [00671], S = provisional, R = approved

dqiR_OrthoP = filter(discrete,dqi_cd_00671=="R")

dqiS_OrthoP = filter(discrete,dqi_cd_00671=="S")

# check TDN values [62854], S = provisional, R = approved

dqiR_TDN = filter(discrete,dqi_cd_62854=="R")

dqiS_TDN = filter(discrete,dqi_cd_62854=="S")


?stat

ggplot(discrete_select, aes(fill = dqi_cd_70953, y = n, x = site_no)) +
  geom_bar(position='dodge', stat='identity') +
  ggtitle('dqi') +
  xlab('site_no') +
  ylab('counts') +
  scale_fill_manual('dqi_cd_70953', values=c('coral2','steelblue', 'pink'))

dqi <- ggplot(discrete_select, aes(x=site_no, 
                                   y=dqi_cd_70953))
dqi + geom_bar(x, alpha, color, fill=3, linetype, size, weight)

head(discrete_select)

discrete_Chl_dqi <- summary(discrete_Chl_select)
head("discrete_dqi")


FPT_uv <- read.csv()
