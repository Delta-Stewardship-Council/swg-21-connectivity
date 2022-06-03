# PCA of nutrient data -
# Libraries --------------------------------------------------
library(readr)
library(dplyr)
library(stats)
library(tidyr)
library(ggplot2)
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

explore <- read.csv("data_model/model_chla_nuts_combined.csv") #3662 obs

inun <- read.csv("data_model/inundation_with_doy1998.csv") #8309 obs

location <- read.csv("data_model/chlorophyll_fin.csv") #1454 obs

chl_fin <- read.csv("data_model/chlorophyll_fin.csv")

#create unique identifiers chl_fin here-----------------------

#first add doy1998 to chl_fin

chl_fin <- merge(chl_fin, inun[-c(2:6)], by = "date")

chl_fin$unique_id <- paste(chl_fin$location, chl_fin$doy1998, sep="_")

#review NA's in explore df----------------------
dat_na <- data.frame(sapply(X = explore, FUN = function(x) sum(is.na(x))))

#Create unique df of stations/regions----------------------

location <- location[c(2, 6)]

unique_location <- unique(location)

#Remove nutrients if not paired with chl-a -------------------------

new_dat <- explore[,c(2:4, 7, 10, 12, 14, 15, 18)]

new_dat_chl <- new_dat[!is.na(new_dat$chlorophyll),]

summary(new_dat_chl)

#add location to new_dat_chl-----------------------------------

new_dat_chl <- left_join(new_dat_chl, unique_location, by = "station_wq_chl")

#Format date----------------------------

new_dat_chl$date <- as.POSIXct(new_dat_chl$date, format = "%Y-%m-%d")

#Add month-------------------------------------------

new_dat_chl$month <- format(new_dat_chl[, "date"], "%m")

new_dat_chl$month <- as.integer(new_dat_chl$month)

#make location a number-----------------------------

new_dat_chl <- new_dat_chl %>%
  mutate(location = case_when(
    location == "above"~1,
    location == "below"~2,
    location == "cache"~3,
    location == "yolo"~4,))

#drop anything without a location

new_dat_chl <- new_dat_chl[!is.na(new_dat_chl$location),]

#correlation to identify if any params should be dropped------------------

corr_fld <- new_dat_chl %>%
  select(where(is.numeric), where(is.integer))

corr_fld <- cor(corr_fld, use="pairwise.complete.obs")

corr_fld  %>%
  #rearrange(absolute = FALSE) %>% # order by corr
  #shave() %>% # take lower triangle only
  rplot() +
  theme(axis.text.x = element_text(angle=70,
                                   size = 7),
        axis.text.y = element_text(size = 7)
  )

#drop ammonia and diss_nitrite_nitrate here

new_dat_chl <- new_dat_chl[,-c(6, 7)]

#create unique identifiers for new_dat_chl to bind PCA results to chl_fin at end of script-----------------------

new_dat_chl$unique_id <- paste(new_dat_chl$date, new_dat_chl$station_wq_chl, sep="_")

#Subset data >= WY 2009 and region---------------------------------------

df_for_PCA_cache <- subset(new_dat_chl, date >= "2008-10-01"&location=="3")
df_for_PCA_yolo <- subset(new_dat_chl, date >= "2008-10-01"&location=="4")
df_for_PCA_below <- subset(new_dat_chl, date >= "2008-10-01"&location=="2")
df_for_PCA_above <- subset(new_dat_chl, date >= "2008-10-01"&location=="1")

# Select only quantitative data--------------------------------------
nut_quant_cache <- df_for_PCA_cache %>%
  select(where(is.numeric)) %>%
  select_if(~ any(is.na(.)))

nut_quant_yolo <- df_for_PCA_yolo %>%
  select(where(is.numeric)) %>%
  select_if(~ any(is.na(.)))

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

#Imputation----------------------------------------------------------

nut_completed_cache <- imputePCA(nut_quant_cache, ncp = 1, scale = TRUE)
nut_completed_yolo <- imputePCA(nut_quant_yolo, ncp = 1, scale = TRUE)
nut_completed_below <- imputePCA(nut_quant_below, ncp = 1, scale = TRUE)
nut_completed_above <- imputePCA(nut_quant_above, ncp = 1, scale = TRUE)

nut_completed_cache_df <- nut_completed_cache$completeObs
nut_completed_yolo_df <- nut_completed_yolo$completeObs
nut_completed_below_df <- nut_completed_below$completeObs
nut_completed_above_df <- nut_completed_above$completeObs

summary(nut_completed_cache_df)
summary(nut_completed_yolo_df)
summary(nut_completed_below_df)
summary(nut_completed_above_df)

#Stich together full df - include columns missing from imputed data--------------

df_for_PCA_cache <- cbind(nut_completed_cache_df, df_for_PCA_cache[,c(1, 3, 5,  8, 9)])
df_for_PCA_yolo <- cbind(nut_completed_yolo_df, df_for_PCA_yolo[,c(1, 3, 5, 8, 9)])
df_for_PCA_below <- cbind(nut_completed_below_df, df_for_PCA_below[,c(1, 3, 5, 8, 9)])
df_for_PCA_above <- cbind(nut_completed_above_df, df_for_PCA_above[,c(1, 3, 5, 8, 9)])

# Make month numeric----------------------------------
df_for_PCA_cache$month <- as.numeric(df_for_PCA_cache$month)
df_for_PCA_yolo$month <- as.numeric(df_for_PCA_yolo$month)
df_for_PCA_below$month <- as.numeric(df_for_PCA_below$month)
df_for_PCA_above$month <- as.numeric(df_for_PCA_above$month)

# PCA: With FactoExtra---------------------------------------
summary(df_for_PCA_cache)
summary(df_for_PCA_yolo)
summary(df_for_PCA_below)
summary(df_for_PCA_above)

# Stich data for PCA ------------------------------------------------------

df_for_PCA <- bind_rows(df_for_PCA_cache, df_for_PCA_yolo, df_for_PCA_above, df_for_PCA_below)

table(df_for_PCA$location)
summary(df_for_PCA)

#bind df_for_PCA and new_dat_chl on doy1998 and location?

# PCA --------------------------------------------------------------------
# PCA: With tidy
# arrow style
arrow_style <- arrow(
  angle = 20, length = grid::unit(8, "pt"),
  ends = "first", type = "closed"
)

# Select, scale data (mean of zero, sd =1), and pca

nuts_pca <- df_for_PCA %>% as_tibble() %>%
  scale() %>%
  prcomp()

#Add PC coordinates and PLOT: PC1 vs P2--------------------------

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

## Look at Rotation Matrix -------------------------------------------------

nuts_pca %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  ) %>%
  ggplot(aes(PC1, PC2)) + ggtitle("Cache") +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-1.5, 0.5) + ylim(-1, 1) +
  coord_fixed()

# Variance Explained ------------------------------------------------------

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

#All regions PCA-------------------------------
# PC1 and PC2 explain ~30 and 21% of variance
fviz_pca_var(nuts_pca) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - all regions (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

#Assign region names into dat-final-------------------------------

levels(df_for_PCA$location)

dat_final <- df_for_PCA %>%
  mutate(location_name = case_when(
    location == 1~"above",
    location == 2~"below",
    location == 3~"cache",
    location == 4~"yolo",
    #TRUE~location
  ))

#add eigen values to dat_final

dat_final <- cbind(dat_final, nuts_pca$x)

#make unique ID for dat_final

dat_final <- dat_final %>%
  mutate(unique_id = paste0(location_name,
                        "_", doy1998))
#join dat_fin to chl_fin - merge works but left_join produces NAs...used distinct in cases where >1 nutrient sample collected each day

merge <- merge(chl_fin[-c(2)], dat_final[-c(3:8)], by = "unique_id")

final <- distinct(merge, unique_id, .keep_all = TRUE)

write.csv(final, "data_clean/nutrient_pca.csv")



