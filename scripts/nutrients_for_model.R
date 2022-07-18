# PCA of nutrient data -

# Libraries --------------------------------------------------

library(tidyverse)
library(stats)
library(ggpubr)
library(lubridate)
library(imputeTS)
suppressPackageStartupMessages(library(tidyverse))
options(scipen = 999) # turn off scientific notation

# Correlation libraries

library(corrr)
library(DT)

# Load data----------------

# use read_csv instead of read.csv to automatically format dates on import

# these data are used in the gam, from functions/f_load_model_chla_covars_data.R)
gamdat <- read_csv("data_model/model_chla_covars_gam.csv")
length(unique(gamdat$station_wq_chl)) # n= 19

# these data come from scripts/functions/f_load_wq_chla_nuts.R
explore <- read_csv("data_model/model_chla_nuts_combined.csv") #3662 obs
# has duplicates for chl-a by date for a station
length(unique(explore$station_wq_chl)) # so 33 unique stations

# inundation data
# inun <- read_csv("data_model/inundation_with_doy1998.csv") #8309 obs

# final chlorophyll data
chl_fin <- read_csv("data_model/chlorophyll_fin.csv") %>%
  select(-1) # remove rowID #1454
length(unique(chl_fin$station_wq_chl)) # n=19 unique stations


# Filter to Same Stations in GAM/Chl-a Data -------------------------------

# check stations are same? (should be all true)
unique(gamdat$station_wq_chl)==unique(chl_fin$station_wq_chl)

# filter to just stations we want
explore <- explore %>%
  dplyr::filter(station_wq_chl %in% unique(gamdat$station_wq_chl))
unique(explore$station_wq_chl)
summary(explore)

# duplicates on same days?
explore %>%
  group_by(station_wq_chl, date) %>%
  tally() %>%
  arrange(desc(n))

# yes...many dups...sample a single value to carry forward?
# if we drop the chlorophyll column and and sample 1 from duplicates
# should be ok
explore_trim <- explore %>%
  select(-chlorophyll) %>%
  distinct(.keep_all = TRUE) %>% # keep all columns
  group_by(station_wq_chl, date) %>%
  group_by(station_wq_chl, date) %>%
  slice_sample(n=1) %>%
  ungroup()

# double check
explore_trim %>%
  group_by(station_wq_chl, date) %>%
  tally() %>% View() # yay only 1's!

# Pull in Nutrients Data --------------------------------------------------

# join nutrient data to clean and final chl-a dataset
chl_nuts <- left_join(chl_fin, explore_trim, by=c("station_wq_chl", "date"))
# should match n of rows in original dataset

# since location has multiple stations for each label ("above", "below")
# unique ID may be best for station and doy1998 since it's more explicit
chl_nuts$unique_id <- paste(chl_nuts$station_wq_chl, chl_nuts$doy1998, sep="_")
length(unique(chl_nuts$unique_id)) # n=1454

#review NA's in explore df----------------------

(dat_na <- data.frame("totNA"=sapply(chl_nuts, function(x) sum(is.na(x)))))

#Create unique df of stations/regions----------------------

# look at tally of obs by station and location
chl_nuts %>%
  group_by(station_wq_chl, location) %>% tally() %>%
  arrange(location)

location <- chl_fin[c("station_wq_chl", "location")] #or doy1998
(unique_location <- unique(location)) # n=19

#Remove nutrients if not paired with chl-a -------------------------

# drop cols we don't need, using names here in case col number changes
cols_to_keep <- c("doy1998","station_wq_chl", "location",
                  "latitude","longitude",
                  "date", "chlorophyll", "diss_ammonia",
                  "diss_nitrate_nitrite", "din", "diss_orthophos")

new_dat <- chl_nuts[,names(chl_nuts) %in% cols_to_keep]

# check for NAs in chlorophyll and remove if need be
summary(new_dat)
new_dat <- new_dat[!is.na(new_dat$chlorophyll),]

# still many NAs and stations without location?
table(new_dat$location, useNA="ifany") # no NAs!

#Format date----------------------------

# not required if using read_csv
# new_dat$date <- as.Date(new_dat$date, format = "%Y-%m-%d")

# Add month-------------------------------------------

library(lubridate)
new_dat$month <- lubridate::month(new_dat$date)

# make location a number-----------------------------

# make a location integer
new_dat <- new_dat %>%
  mutate(location_int = case_when(
    location == "above" ~ 1,
    location == "below" ~ 2,
    location == "cache" ~ 3,
    location == "yolo" ~ 4,))

#drop anything without a location

new_dat <- new_dat[!is.na(new_dat$location),] # no change
# n=1454
summary(new_dat)

#correlation to identify if any params should be dropped------------------
names(new_dat)
corr_fld <- new_dat %>%
  select(where(is.numeric), where(is.integer))
# check names to see what is here now:
names(corr_fld)

# correlation matrix
corr_fld <- cor(corr_fld, use="pairwise.complete.obs")

# arrange and plot
corr_fld  %>%
  #rearrange(absolute = FALSE) %>% # order by corr
  #shave() %>% # take lower triangle only
  rplot() +
  theme(axis.text.x = element_text(angle=70,
                                   size = 7),
        axis.text.y = element_text(size = 7)
  ) -> corplot1
corplot1

# make interactive
library(plotly)
ggplotly(corplot1)

#summarize new dat to see what needs to be imputed?
#try this - add year and month_year coloumn
new_dat$year <- lubridate::year(new_dat$date)

new_dat$month_year <- month_year(new_dat$date)

new_dat$month_year <- format(new_dat$date, '%Y-%m')

#summarize data by month_year to look for gaps

din_test <- new_dat %>%
  group_by(month_year, location) %>%
  summarize(mean_din = mean(din))

summary(din_test)

# drop diss_nitrite_nitrate and longitude, both are above 0.7
# drop ammonia, it is ~0.62-0.68 and correlated with long/lat and DIN
new_dat <- new_dat %>%
  select(-c(diss_ammonia, diss_nitrate_nitrite, longitude))
names(new_dat)

#create unique identifiers for new_dat to bind nutrient results to chl_fin at end of script-----------------------

new_dat$unique_id <- paste(new_dat$date, new_dat$station_wq_chl, sep="_")
length(unique(new_dat$unique_id)) # unique

# what stations are duplicated?
new_dat %>%
  group_by(station_wq_chl, date) %>%
  tally() %>% arrange(desc(n))
# all 1's so no duplicates

#Calculate monthly DIN and orthophosphate by region for non-inundated and inundation periods---------------------------------------
##first import inundation dataset

inun <- read.csv("data_model/inundation_with_doy1998.csv")

inun$date <- as.Date(inun$date)

#subset new_dat to WY2009 - WY2020

nuts_WY08 <- subset(new_dat, date >= "2008-10-01")

#merge new_dat and inun dfs

nuts_inun <- merge(nuts_WY08, inun, by="date", all.x="TRUE")

#group DIN and ortho-phosphate by month, inundation, region (location)

din_loc_month_summary <- nuts_inun %>%
  group_by(month, location, inundation) %>%
  summarize(month_din = mean(din, na.rm=TRUE), n_din = n())

orthop_loc_month_summary <- nuts_inun %>%
  group_by(month, location, inundation) %>%
  summarize(month_orthop = mean(diss_orthophos, na.rm=TRUE), n_orthop = n())

#add monthly means of DIN and ortho-phosphate to df

din_month_col <- merge(nuts_inun, din_loc_month_summary, by = c("location", "month", "inundation"))

nuts_month_col <- merge(din_month_col, orthop_loc_month_summary , by = c("location", "month", "inundation"))

nuts_month_col <- subset(nuts_month_col, select = -c(doy1998.y))

#if else statement to use monthly means if DIN or ortho-phosphate values missing--------------'
#view inundation vs non-inundated periods

nuts_month_col$DIN_for_model <- ifelse(is.na(nuts_month_col$din), nuts_month_col$month_din, nuts_month_col$din)

nuts_month_col$orthop_for_model <- ifelse(is.na(nuts_month_col$diss_orthophos), nuts_month_col$month_orthop, nuts_month_col$diss_orthophos)

nuts_month_col$din_flag <- ifelse(is.na(nuts_month_col$din), "0", "1")

nuts_month_col$din_flag <- as.numeric(nuts_month_col$din_flag)

nuts_month_col$orthop_flag <- ifelse(is.na(nuts_month_col$diss_orthophos), "0", "1")

nuts_month_col$orthop_flag <- as.numeric(nuts_month_col$orthop_flag)

#cache - higher mean DIN concentration and greater range during non-inundation

cache <- subset(nuts_month_col, location=="cache")
plot(cache$date, cache$DIN_for_model, col = ifelse(cache$din_flag>0, "1", "2"), xlab = "date", ylab = "din_mgL", main = "Cache din")
legend("topleft", legend = c("discrete (n = 89)", "mean (n = 104)"), col = 1:2, pch = 1)
boxplot(cache$DIN_for_model~cache$inundation)

print(table(cache$din_flag)) #means = 104, #discrete = 89

#cache - slightly higher mean ortho-p concentration and greater range during non-inundation

cache <- subset(nuts_month_col, location=="cache")
plot(cache$date, cache$orthop_for_model, col = ifelse(cache$orthop_flag>0, "1", "2"), xlab = "date", ylab = "orthop_mgL", main = "Cache orthop")
legend("top", legend = c("discrete (n = 110)", "mean (n = 83)"), col = 1:2, pch = 1)
boxplot(cache$orthop_for_model~cache$inundation)

print(table(cache$orthop_flag)) #means = 83, #discrete = 110

#above - higher mean DIN concentration during inundation
above <- subset(nuts_month_col, location=="above")
plot(above$date, above$DIN_for_model, col = ifelse(above$din_flag>0, "1", "2"), xlab = "date", ylab = "din_mgL", main = "Above din")
legend("top", legend = c("discrete (n = 96)", "mean (n = 224)"), col = 1:2, pch = 1)
boxplot(above$DIN_for_model~above$inundation)

print(table(above$din_flag)) #means = 224, #discrete = 96

#above - higher mean DIN concentration during inundation

above <- subset(nuts_month_col, location=="above")
plot(above$date, above$orthop_for_model, col = ifelse(above$orthop_flag>0, "1", "2"), xlab = "date", ylab = "orthop_mgL", main = "Above orthop")
legend("top", legend = c("discrete (n = 130)", "mean (n = 190)"), col = 1:2, pch = 1)
boxplot(above$orthop_for_model~above$inundation)

print(table(above$orthop_flag)) #means = 190, #discrete = 130

#below - higher mean ortho-p concentration inundation but greater range during non-inundatation

below <- subset(nuts_month_col, location=="below")
plot(below$date, below$DIN_for_model, col = ifelse(below$din_flag>0, "1", "2"), xlab = "date", ylab = "din_mgL", main = "Below din")
legend("top", legend = c("discrete (n = 194)", "mean (n = 162)"), col = 1:2, pch = 1)
boxplot(below$DIN_for_model~below$inundation)

print(table(below$din_flag)) #means = 162, #discrete = 194

#below - slightly higher mean ortho-p concentration and similar range during inundation and
#non-inundation

below <- subset(nuts_month_col, location=="below")
plot(below$date, below$orthop_for_model, col = ifelse(below$orthop_flag>0, "1", "2"), xlab = "date", ylab = "orthop_mgL", main = "Below orthop")
legend("top", legend = c("discrete (n = 196)", "mean (n = 160)"), col = 1:2, pch = 1)
boxplot(below$orthop_for_model~below$inundation)

print(table(below$orthop_flag)) #means = 160, #discrete = 196

#yolo - higher mean DIN concentration and greater range during non-inundation

yolo <- subset(nuts_month_col, location=="yolo")
plot(yolo$date, yolo$DIN_for_model, col = ifelse(yolo$din_flag>0, "1", "2"), xlab = "date", ylab = "din_mgL", main = "Yolo din")
legend("top", legend = c("discrete (n = 194)", "mean (n = 162)"), col = 1:2, pch = 1)
boxplot(yolo$DIN_for_model~yolo$inundation)

print(table(yolo$din_flag)) #means = 189, discrete = 175

#yolo - higher mean ortho-p concentration and greater range during non-inundation

yolo <- subset(nuts_month_col, location=="yolo")
plot(yolo$date, yolo$orthop_for_model, col = ifelse(yolo$orthop_flag>0, "1", "2"), xlab = "date", ylab = "orthop_mgL", main = "Yolo orthop")
legend("top", legend = c("discrete (n = 185)", "mean (n = 179)"), col = 1:2, pch = 1)
boxplot(yolo$orthop_for_model~yolo$inundation)

print(table(yolo$orthop_flag)) #means = 179, discrete = 185

#subset final df ------------------------

nuts_month_col_final <- subset(nuts_month_col, select = c(location, month, inundation, date, station_wq_chl, din, diss_orthophos, month_din, month_orthop, DIN_for_model, orthop_for_model))

#write .csv--------------------------------

write.csv(nuts_month_col_final, "data_model/nutrients_for_model.csv")
