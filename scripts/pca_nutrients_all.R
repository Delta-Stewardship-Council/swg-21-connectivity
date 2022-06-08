# PCA of nutrient data -

# Libraries --------------------------------------------------

library(tidyverse)
library(stats)
library(ggpubr)
library(lubridate)
library(signal)
library(dataRetrieval)
suppressPackageStartupMessages(library(tidyverse))
options(scipen = 999) # turn off scientific notation

# PCA libraries

library(missMDA) # estimate missing values for PCA
library(FactoMineR)
library(factoextra) # clustering visualization/stats
library(broom) # stats functions

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

# drop diss_nitrite_nitrate and longitude, both are above 0.7
# drop ammonia, it is ~0.62-0.68 and correlated with long/lat and DIN
new_dat <- new_dat %>%
  select(-c(diss_ammonia, diss_nitrate_nitrite, longitude))
names(new_dat)

#create unique identifiers for new_dat to bind PCA results to chl_fin at end of script-----------------------

new_dat$unique_id <- paste(new_dat$date, new_dat$station_wq_chl, sep="_")
length(unique(new_dat$unique_id)) # unique

# what stations are duplicated?
new_dat %>%
  group_by(station_wq_chl, date) %>%
  tally() %>% arrange(desc(n))
# all 1's so no duplicates

#Subset data >= WY 2009 and region---------------------------------------

df_for_PCA_cache <- subset(new_dat, date >= "2008-10-01" & location_int=="3")
df_for_PCA_yolo <- subset(new_dat, date >= "2008-10-01" & location_int=="4")
df_for_PCA_below <- subset(new_dat, date >= "2008-10-01" & location_int=="2")
df_for_PCA_above <- subset(new_dat, date >= "2008-10-01" & location_int=="1")

# Select only quantitative data--------------------------------------

# select numeric data and select only columns with NAs
nut_quant_cache <- df_for_PCA_cache %>%
  # drop doy and lat
  select(-c(doy1998, latitude)) %>%
  select(where(is.numeric)) %>%
  select_if(~ any(is.na(.)))
# check names?
names(nut_quant_cache) # so doy1998, latitude, din, and diss_orthophos

nut_quant_yolo <- df_for_PCA_yolo %>%
  select(where(is.numeric)) %>%
  select_if(~ any(is.na(.)))
names(nut_quant_yolo)

nut_quant_below <- df_for_PCA_below %>%
  select(where(is.numeric)) %>%
  select_if(~ any(is.na(.)))

nut_quant_above <- df_for_PCA_above %>%
  select(where(is.numeric)) %>%
  select_if(~ any(is.na(.)))

summary(nut_quant_cache)
summary(nut_quant_yolo)
summary(nut_quant_below)
summary(nut_quant_above)

# Imputation----------------------------------------------------------

nut_completed_cache <- imputePCA(nut_quant_cache, ncp = 1, scale = TRUE)
nut_completed_yolo <- imputePCA(nut_quant_yolo, ncp = 1, scale = TRUE)
nut_completed_below <- imputePCA(nut_quant_below, ncp = 1, scale = TRUE)
nut_completed_above <- imputePCA(nut_quant_above, ncp = 1, scale = TRUE)

# pull out the "completed Observations"
nut_completed_cache_df <- nut_completed_cache$completeObs
nut_completed_yolo_df <- nut_completed_yolo$completeObs
nut_completed_below_df <- nut_completed_below$completeObs
nut_completed_above_df <- nut_completed_above$completeObs

# check, shouldn't have NAs
summary(nut_completed_cache_df)
summary(nut_completed_yolo_df)
summary(nut_completed_below_df)
summary(nut_completed_above_df)

# Stitch together full df - include columns missing from imputed data--------------

names(df_for_PCA_cache)

# bind together but only keep din and diss_orthophos from imputed data
df_for_PCA_cache <- cbind(nut_completed_cache_df, df_for_PCA_cache[,-c(7,8)])
df_for_PCA_yolo <- cbind(nut_completed_yolo_df, df_for_PCA_yolo[,-c(7,8)])
df_for_PCA_below <- cbind(nut_completed_below_df, df_for_PCA_below[,-c(7,8)])
df_for_PCA_above <- cbind(nut_completed_above_df, df_for_PCA_above[,-c(7,8)])
names(df_for_PCA_above)

# PCA: With FactoExtra---------------------------------------
summary(df_for_PCA_cache)
summary(df_for_PCA_yolo)
summary(df_for_PCA_below)
summary(df_for_PCA_above)

# Stitch data for PCA ------------------------------------------------------

# bind together
df_for_PCA <- bind_rows(df_for_PCA_cache, df_for_PCA_yolo, df_for_PCA_above, df_for_PCA_below)

# check for NAs
table(df_for_PCA$location, useNA = "ifany")
summary(df_for_PCA)

# drop the one with NA in doy1998
df_for_PCA <- df_for_PCA %>% filter(!is.na(doy1998))



# PCA --------------------------------------------------------------------

# PCA: With tidy
# arrow style
arrow_style <- arrow(
  angle = 20, length = grid::unit(8, "pt"),
  ends = "first", type = "closed"
)

# Select, scale data (mean of zero, sd =1), and pca
nuts_pca <- df_for_PCA %>%
  #drop the char data:
  dplyr::select(-c(date, station_wq_chl, location, unique_id)) %>%
  as_tibble() %>%
  scale() %>%
  prcomp()

#Add PC coordinates and PLOT: PC1 vs P2 by lat--------------------------

nuts_pca %>%
  augment(df_for_PCA) %>% #head()
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(color=latitude), alpha=0.8, size=4.5) +
  theme_classic() +
  #scale_color_viridis_d() +
  labs(title = "PCA of Nutrients",
       x="PC1", y="PC2") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

#Add PC coordinates and PLOT: by location --------------------------

nuts_pca %>%
  augment(df_for_PCA) %>% #head()
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(color=location), alpha=0.8, size=4.5) +
  theme_classic() +
  #scale_color_viridis_d() +
  labs(title = "PCA of Nutrients",
       x="PC1", y="PC2") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

#Add PC coordinates and PLOT: by chl-a --------------------------

nuts_pca %>%
  augment(df_for_PCA) %>% #head()
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(color=chlorophyll), alpha=0.8, size=4.5) +
  theme_classic() +
  scale_color_viridis_c() +
  labs(title = "PCA of Nutrients",
       x="PC1", y="PC2") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

## Look at Rotation Matrix -------------------------------------------------

nuts_pca %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  ) %>%
  ggplot(aes(PC1, PC2)) + ggtitle("All") +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-1, 1) + ylim(-1, 1) +
  coord_fixed()

# could drop diss_orthopos as same area as location_int?

# look at PC1-3
nuts_pca %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  ) %>%
  ggplot(aes(PC1, PC3)) + ggtitle("All") +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-1, 1) + ylim(-1, 1) +
  coord_fixed()

# plot by region ---------------------

# get the rotation matrix
nuts_rot_pca <- nuts_pca %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  )

# get vars used in PCA
var_names <- list(row.names(nuts_pca$rotation))
nuts_pca %>%
  augment(df_for_PCA) %>%
  # fix names
  rename_with(., ~gsub(".fitted", replacement = "", .x, fixed=TRUE), starts_with(".fitted")) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color=location), alpha=0.8, size=4.5) +
  theme_classic() +
  geom_segment(data=nuts_rot_pca, aes(PC1, PC2),
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(data=nuts_rot_pca, aes(label = column), hjust = 1) +
  #xlim(-5, 3) + ylim(-4, 3) +
  coord_fixed() +
  #scale_color_viridis_d() +
  labs(title = "PCA of Nutrients",
       caption = glue::glue("Vars included: {var_names}"),
       x="PC1", y="PC2") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

ggsave(filename = "figures/pca_chla_nutrients_by_region_w_rotation.png", width = 10, height = 8, units = "in", dpi=300)


# Variance Explained ------------------------------------------------------

# look at how each var is contributing to each PC (Dim1 = PC1, etc)
contribs <- get_pca_var(nuts_pca)$contrib %>%
  as.data.frame() %>%
  rownames_to_column(var = "var")

# look at percentage for each var
contribs

# write out for looking at later
write_csv(contribs, file="output/pca_variance_explained_per_variable.csv")

# barplot
nuts_pca %>%
  # extract eigenvalues
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col() +
  scale_x_continuous(
    # create one axis tick per PC
    breaks = 1:6
  ) +
  scale_y_continuous(
    name = "variance explained",
    # format y axis ticks as percent values
    label = scales::label_percent(accuracy = 1)
  )

# All regions PCA-------------------------------
# PC1 and PC2 explain ~31 and 21% of variance
fviz_pca_var(nuts_pca, axes = c(1,2)) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - all regions (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# Add eigen values to dat_final-----------------------------------

dat_final <- cbind(df_for_PCA, nuts_pca$x)

# make sure unique id is uniqe
length(unique(paste0(dat_final$station_wq_chl, dat_final$doy1998)))
length(unique(paste0(dat_final$station_wq_chl, dat_final$date)))
length(unique(dat_final$unique_id))


# Write out ---------------------------------------------------------------

# use write_csv to avoid adding column of row ID numbers
write_csv(dat_final, "data_model/nutrient_pca_results_for_gam.csv")



