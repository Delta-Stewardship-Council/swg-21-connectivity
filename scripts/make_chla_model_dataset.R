# make_model dataset

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# Bring in Inundation Data ------------------------------------------------

# this has DayFlow Yolo, Stage of Sac at Fremont Weir, inundation days (from Pascale's code inundation_days.R)

inund <- read_csv("data/inundation_days.csv") %>%
  select(Date:Topped.days) # drop row id column

# Get Verona Daily Flow ---------------------------------------------------

library(dataRetrieval)

# discharge is "00060"
verona <- dataRetrieval::readNWISdv(siteNumbers = "11425500", parameterCd = c("00060"))

# fix names
verona <- addWaterYear(verona)
verona <- dataRetrieval::renameNWISColumns(verona)

# write out
# write_csv(verona, "data/usgs_verona_discharge_1929-2021.csv")

# quick plot
(g1 <-ggplot(verona) + geom_line(aes(x=Date, y=Flow)) +
    geom_line(data=inund, aes(x=Date, y=YOLO), color="blue"))

# check for missing flow data
library(naniar)
gg_miss_var(verona)

# use plotly to interactively visualize
# library(plotly)
# ggplotly(g1)


# Merge Data --------------------------------------------------------------

# only merge for same period of record of inund data

df_out <- left_join(inund, verona, by="Date") %>%
  # drop columns
  select(-c(agency_cd, Flow_cd)) %>%
  # rename
  rename(Flow_usgs_verona = Flow,
         site_no_usgs = site_no)

summary(df_out)

gg_miss_var(df_out)


# Get DayMet Data ---------------------------------------------------------

# Yolo
lats <- 38.307857513
lons <- -121.692428589

library(daymetr)
yolo_daymet <- download_daymet(site = "Yolo",
                                   lat = lats,
                                   lon =  lons,
                                   start = 1997,
                                   end = 2020, internal = TRUE)


# rename variables, create a date column
yolo_daymet_df <- yolo_daymet$data %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday) -1,
            precip_mm = prcp..mm.day., # mm ppt / day
            tmax = tmax..deg.c., #max temp
            tmin = tmin..deg.c., # min temp
            tmean = (tmax + tmin) / 2, # mean temp
            trange = tmax - tmin, # temp range
            srad = srad..W.m.2., # soloar radiation
            vpd = vp..Pa.)

plot(yolo_daymet_df$date, yolo_daymet_df$tmean)
gg_miss_var(yolo_daymet_df)

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

# see what qc has
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
            aes(x=date, y=avg_air_temp_c), color="cyan4")

# look for missing data?
library(naniar)
naniar::gg_miss_var(bryte_filt)
gg_miss_case(bryte_filt)


# Plot Daymet vs. CIMIS ---------------------------------------------------

# join w daymet and plot?
cimis_daymet <- left_join(bryte_filt, yolo_daymet_df, by=c("date"))

# replot
ggplot() +
  geom_line(data=cimis_daymet, aes(x=date, y=tmean), color="blue") +
  geom_line(data=cimis_daymet, aes(x=date, y=avg_air_temp_c), color="orange") +
  theme_classic() +
  labs(title="Comparison of CIMIS Bryte and DayMet Air Temp")
ggsave(filename = "figures/cimis_vs_daymet_airtemp.png",width = 11, height = 8.5, dpi=300)

# sol rad
ggplot() +
  geom_line(data=cimis_daymet, aes(x=date, y=sol_rad_w_sq_m), color="blue") +
  geom_line(data=cimis_daymet, aes(x=date, y=srad), color="orange") +
  theme_classic() +
  labs(title="Comparison of CIMIS Bryte and DayMet Solar Radiation", caption = "Using DayMet for (38.307857513,
-121.692428589)")

ggsave(filename = "figures/cimis_vs_daymet_airtemp.png",width = 11, height = 8.5, dpi=300)


# JOIN Data ---------------------------------------------------------------

# join
df_out_2 <- left_join(cimis_daymet, df_out, by=c("date"="Date"))

naniar::gg_miss_var(df_out_2)
gg_miss_case(df_out_2)

# Export Data -------------------------------------------------------------

# write out
write_csv(df_out_2, "data/yolo_data_for_model.csv")


# Update Model Dataset? --------------------------------

## Pull in Phyto Data from Jessica & Liz -----------------------------------

load("scripts/sam_models/Chla_all.Rda") # file called "total"


