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

# use plotly to interactively visualize
library(plotly)
ggplotly(g1)


# Merge Data --------------------------------------------------------------

# only merge for same period of record of inund data

df_out <- left_join(inund, verona, by="Date") %>%
  # drop columns
  select(-c(agency_cd, Flow_cd)) %>%
  # rename
  rename(Flow_usgs_verona = Flow,
         site_no_usgs = site_no)

summary(df_out)



# Bring in Air Temperature ------------------------------------------------

bryte <- read_csv("data/Bryte_CIMIS_daily.csv")

# format date
bryte <- bryte %>%
  mutate(Date = mdy(Date))

# make clean column names
library(janitor)
bryte <- clean_names(bryte)



# filter to cols of interest (drop qc cols)
bryte_filt <- select(bryte,
                     stn_id:date, precip_mm, sol_rad_w_sq_m,
                     ends_with("air_temp_c"))

# check
summary(bryte_filt)

# plot
ggplot() + geom_line(data=bryte_filt, aes(x=date, y=sol_rad_w_sq_m))

# some missing and error data, fix and fill w NA
bryte_filt <- bryte_filt %>%
  mutate(sol_rad_w_sq_m =
           case_when(
             sol_rad_w_sq_m < 0 ~ NA_real_,
             sol_rad_w_sq_m > 500 ~ NA_real_,
             TRUE ~ sol_rad_w_sq_m
           ))

# replot
ggplot() + geom_line(data=bryte_filt, aes(x=date, y=sol_rad_w_sq_m))

summary(bryte_filt)

# check temps
ggplot() +
  geom_line(data=bryte_filt,
                     aes(x=date, y=max_air_temp_c), color="red4") +
  geom_line(data=bryte_filt,
            aes(x=date, y=min_air_temp_c), color="darkblue") +
  geom_line(data=bryte_filt,
            aes(x=date, y=avg_air_temp_c), color="cyan4")

# ok
# join
df_out_2 <- left_join(df_out, bryte_filt, by=c("Date"="date"))

# Export Data -------------------------------------------------------------

# write out
write_csv(df_out_2, "data/yolo_data_for_model.csv")
