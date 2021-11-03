# make_model dataset

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# FLOW --------------------------------------------
# pull in flow data

## INUNDATION: Inundation Data ------------------------------------------------

# this has DayFlow Yolo, Stage of Sac at Fremont Weir, inundation days (from Pascale's code inundation_days.R)

inund <- read_csv("data/inundation_days.csv") %>%
  select(Date:Topped.days) # drop row id column

## DAILY FLOW: Verona Daily Flow ---------------------------------------------------

library(dataRetrieval)

# discharge is "00060"
verona <- dataRetrieval::readNWISdv(siteNumbers = "11425500", parameterCd = c("00060"))

# fix names
verona <- addWaterYear(verona)
verona <- dataRetrieval::renameNWISColumns(verona)

# quick plot
(g1 <-ggplot(verona) +
    geom_line(aes(x=Date, y=Flow)) +
    geom_line(data=inund, aes(x=Date, y=YOLO), color="blue") +
    theme_minimal() +
    labs(title="Flow at USGS Verona (11425500)",
         subtitle = "Inundation Data: Yolo (blue)"))

#ggsave("figures/usgs_daily_flow_at_verona.png", width = 11, height = 8.5, dpi=300)

# no missing data in this period of flow

# package checks for missing data
library(naniar)
gg_miss_var(verona) # no data missing
gg_miss_var(inund) # no data missing

## Merge FLOW & INUNDATION Data --------------------------------------------------------------

# only merge for same period of record of inund data

flow_out <- left_join(inund, verona, by="Date") %>%
  # drop columns
  select(-c(agency_cd, Flow_cd)) %>%
  # rename
  rename(Flow_usgs_verona = Flow,
         site_no_usgs = site_no)

summary(flow_out)

gg_miss_var(flow_out)

# DAYMET: Get DayMet Data ---------------------------------------------------------

# Yolo
lats <- 38.307857513
lons <- -121.692428589

library(daymetr)
yolo_daymet <- download_daymet(site = "Yolo",
                                   lat = lats,
                                   lon =  lons,
                                   start = 1994,
                                   end = 2020, internal = TRUE)


# rename variables, create a date column
yolo_daymet_df <- yolo_daymet$data %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday) -1,
            daymet_precip_mm = prcp..mm.day., # mm ppt / day
            daymet_tmax = tmax..deg.c., #max temp
            daymet_tmin = tmin..deg.c., # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad..W.m.2., # soloar radiation
            daymet_vpd = vp..Pa.)

gg_miss_var(yolo_daymet_df)

# save out:
write_csv(yolo_daymet_df, "data/cimis_yolo_1994-2020.csv")

# Bring in Air Temperature: CIMIS ------------------------------------------------

bryte <- read_csv("data/Bryte_CIMIS_daily.csv")

# R far out than historical max
# M missing
# Y out of historical range
# P
# H any solar radiation flagged M/S/Q
# S extreme solar radiation

# format date
bryte <- bryte %>%
  mutate(Date = mdy(Date))

# make clean column names
library(janitor)
bryte <- clean_names(bryte)

# see what qc codes exist
table(bryte$qc_9, exclude = "ifany")

# filter to cols of interest and filter flagged data:
# if there's a letter, drop data?
bryte_filt <- bryte %>%
  # select cols of interest
  select(stn_id:date,
         precip_mm, qc_9,
         sol_rad_w_sq_m, qc_11,
         max_air_temp_c, qc_15,
         min_air_temp_c, qc_17,
         avg_air_temp_c, qc_19) %>%
  # filter to only data without flags
  filter(is.na(qc_9) & is.na(qc_11) &
           is.na(qc_15) & is.na(qc_17) &
           is.na(qc_19))

# check
summary(bryte_filt)
table(bryte_filt$qc_9, exclude = "ifany")

# drop QA cols
bryte_filt <- select(bryte_filt,
                     stn_id:date, precip_mm,
                     sol_rad_w_sq_m,
                     ends_with("air_temp_c"))

# plots
ggplot() + geom_line(data=bryte_filt, aes(x=date, y=sol_rad_w_sq_m))
ggplot() + geom_line(data=bryte_filt, aes(x=date, y=avg_air_temp_c))

# check temps
ggplot() +
  geom_line(data=bryte_filt,
            aes(x=date, y=max_air_temp_c), color="red4") +
  geom_line(data=bryte_filt,
            aes(x=date, y=min_air_temp_c), color="darkblue") +
  geom_line(data=bryte_filt,
            aes(x=date, y=avg_air_temp_c), color="gray40") +
  theme_minimal() +
  labs(title="CIMIS (Bryte)",
       subtitle = "Air Temperature (max=red, min=darkblue, avg=gray)",
       x="", y="Air Temp (C)")
#ggsave("figures/cimis_bryte_daily_airtemp.png", width = 11,
#       height = 8.5, dpi=300)


# Plot Daymet vs. CIMIS ---------------------------------------------------

# join w daymet and plot?
cimis_daymet <- left_join(yolo_daymet_df, bryte_filt, by=c("date"))

# whats missing?
gg_miss_var(cimis_daymet)
# def gaps in the CIMIS data

# plot
ggplot() +
  geom_line(data=cimis_daymet, aes(x=date, y=daymet_tmean), color="darkblue") +
  geom_line(data=cimis_daymet, aes(x=date, y=avg_air_temp_c), color="orange", alpha=0.8) +
  theme_minimal() +
  labs(title="Comparison of CIMIS Bryte and DayMet Air Temp",
       subtitle = "CIMIS=orange, DayMet=blue", x="", y="Mean Air Temp (C)")

# ggsave(filename = "figures/cimis_vs_daymet_airtemp.png",width = 11, height = 8.5, dpi=300)

# sol rad
ggplot() +
  geom_line(data=cimis_daymet, aes(x=date, y=sol_rad_w_sq_m), color="orange") +
  geom_line(data=cimis_daymet, aes(x=date, y=daymet_srad), color="darkblue", alpha=0.8) +
  theme_minimal() +
  labs(title="Comparison of CIMIS Bryte and DayMet Solar Radiation", caption = "DayMet from: 38.307857513,
-121.692428589", subtitle = "CIMIS=orange, DayMet=blue", x="", y="Solar Rad (sq.m)")

# ggsave(filename = "figures/cimis_vs_daymet_solarrad.png",width = 11, height = 8.5, dpi=300)

# JOIN Data ---------------------------------------------------------------
# join daymet with flow data
names(yolo_daymet_df)

# plot to see if data lines up
ggplot() +
  geom_path(data=flow_out,
                 aes(x=Date, y="USGS"), color="blue") +
  geom_path(data=yolo_daymet_df,
                 aes(x=date, y="DayMet")) +
  geom_path(data=bryte_filt,
            aes(x=date, y="CIMIS"), color="orange") +
  theme_classic()

# join
mod_data_out <- left_join(flow_out, yolo_daymet_df, by=c("Date"="date"))

summary(mod_data_out)
# note: the NAs are due to leap years

#drop the NAs
mod_data_out <- mod_data_out %>%
  filter(!is.na(daymet_srad))
naniar::gg_miss_var(mod_data_out)


# Export Data -------------------------------------------------------------

# write out
write_csv(mod_data_out, "data/yolo_daymet_for_model.csv")


## Pull in Phyto Data from Jessica & Liz -----------------------------------

load("scripts/sam_models/Chla_all.Rda") # file called "total"


