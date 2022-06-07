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

# load data

explore <- read.csv("data_model/model_chla_nuts_combined.csv")

inun <- read.csv("data_model/inundation_with_doy1998.csv")

location <- read.csv("data_model/chlorophyll_fin.csv")

#create unique df of stations/regions

location <- location[c(2, 6)]

unique_location <- unique(location)

dat_na <- data.frame(sapply(X = explore, FUN = function(x) sum(is.na(x))))

#remove nutrients if not paired with chl-a

new_dat <- explore[,c(2:5, 7,10,12,14,15)]

new_dat_chl <- new_dat[!is.na(new_dat$chlorophyll),]

summary(new_dat_chl)

new_dat_chl <- left_join(new_dat_chl, unique_location, by = "station_wq_chl" )

new_dat_chl$location <- as.factor(new_dat_chl$location)

#format date

new_dat_chl$date <- as.POSIXct(new_dat_chl$date, format = "%Y-%m-%d")

#pick up here

#subset data >= WY 2009 and region

df_for_PCA_cache <- subset(new_dat_chl, date >= "2008-10-01"&location=="cache")
df_for_PCA_yolo <- subset(new_dat_chl, date >= "2008-10-01"&location=="yolo")
df_for_PCA_below <- subset(new_dat_chl, date >= "2008-10-01"&location=="below")
df_for_PCA_above <- subset(new_dat_chl, date >= "2008-10-01"&location=="above")

# select only quantitative data
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

nut_completed_cache <- imputePCA(nut_quant_cache, ncp = 2, scale = TRUE)
nut_completed_yolo <- imputePCA(nut_quant_yolo, ncp = 2, scale = TRUE)
nut_completed_below <- imputePCA(nut_quant_below, ncp = 2, scale = TRUE)
nut_completed_above <- imputePCA(nut_quant_above, ncp = 2, scale = TRUE)

nut_completed_cache_df <- nut_completed_cache$completeObs
nut_completed_yolo_df <- nut_completed_yolo$completeObs
nut_completed_below_df <- nut_completed_below$completeObs
nut_completed_above_df <- nut_completed_above$completeObs

summary(nut_completed_cache_df)
summary(nut_completed_yolo_df)
summary(nut_completed_below_df)
summary(nut_completed_above_df)

#stich together full df - include columns missing from imputed data

df_for_PCA_cache <- cbind(nut_completed_cache_df, df_for_PCA_cache[,c(1, 3, 4, 6)])
df_for_PCA_yolo <- cbind(nut_completed_yolo_df, df_for_PCA_yolo[,c(1, 3, 4, 6)])
df_for_PCA_below <- cbind(nut_completed_below_df, df_for_PCA_below[,c(1, 3, 4, 6)])
df_for_PCA_above <- cbind(nut_completed_above_df, df_for_PCA_above[,c(1, 3, 4, 6)])

summary(df_for_PCA_cache)
summary(df_for_PCA_yolo)
summary(df_for_PCA_below)
summary(df_for_PCA_above)

# PCA: With FactoExtra
summary(df_for_PCA_cache)
summary(df_for_PCA_yolo)
summary(df_for_PCA_below)
summary(df_for_PCA_above)


# PCA: With tidy
# arrow style
arrow_style <- arrow(
  angle = 20, length = grid::unit(8, "pt"),
  ends = "first", type = "closed"
)

# select, scale data (mean of zero, sd =1), and pca
nuts_pca_cache <- df_for_PCA_cache %>% as_tibble() %>%
  scale() %>%
  prcomp()

nuts_pca_yolo <- df_for_PCA_yolo %>% as_tibble() %>%
  scale() %>%
  prcomp()

nuts_pca_below <- df_for_PCA_below %>% as_tibble() %>%
  scale() %>%
  prcomp()

nuts_pca_above <- df_for_PCA_above %>% as_tibble() %>%
  scale() %>%
  prcomp()

#add PC coordinates and PLOT: PC1 vs P2
nuts_pca_cache %>%
  augment(df_for_PCA_cache) %>% #head()
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(color=latitude), alpha=0.8, size=4.5) +
  theme_classic() +
  #scale_color_viridis_d() +
  labs(title = "PCA of Nutrients",
       x="PC1", y="PC2") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

nuts_pca_yolo %>%
  augment(df_for_PCA_yolo) %>% #head()
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(color=latitude), alpha=0.8, size=4.5) +
  theme_classic() +
  #scale_color_viridis_d() +
  labs(title = "PCA of Nutrients",
       x="PC1", y="PC2") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

nuts_pca_below %>%
  augment(df_for_PCA_below) %>% #head()
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(color=latitude), alpha=0.8, size=4.5) +
  theme_classic() +
  #scale_color_viridis_d() +
  labs(title = "PCA of Nutrients",
       x="PC1", y="PC2") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

nuts_pca_above %>%
  augment(df_for_PCA_above) %>% #head()
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(color=latitude), alpha=0.8, size=4.5) +
  theme_classic() +
  #scale_color_viridis_d() +
  labs(title = "PCA of Nutrients",
       x="PC1", y="PC2") +
  guides(fill = guide_legend(
    override.aes = aes(label = "")))

## Look at Rotation Matrix -------------------------------------------------

nuts_pca_cache %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  ) %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-1.5, 0.5) + ylim(-1, 1) +
  coord_fixed()

nuts_pca_yolo %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  ) %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-0.5, 0.5) + ylim(-1, 1) +
  coord_fixed()

nuts_pca_below %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  ) %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-1.5, 0.5) + ylim(-1, 1) +
  coord_fixed()

nuts_pca_above %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(
    names_from = "PC", values_from = "value",
    names_prefix = "PC"
  ) %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  xlim(-1.5, 0.5) + ylim(-1, 1) +
  coord_fixed()
# Variance Explained ------------------------------------------------------

nuts_pca_cache %>%
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

nuts_pca_yolo %>%
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

nuts_pca_below %>%
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

nuts_pca_above %>%
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

# for 'cache' first two PCs explain ~39 and 20% (total of ~59%) of the variance

fviz_pca_var(nuts_pca_cache) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - cache (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_cache_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# for 'yolo' first two PCs explain ~37% and 27% (total of ~64%) of the variance

fviz_pca_var(nuts_pca_yolo) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - yolo (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_yolo_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# for 'below' first two PCs explain ~39% and 26% (total of ~65%) of the variance

fviz_pca_var(nuts_pca_below) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - below (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_below_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# for 'above' first two PCs explain ~33% and 29% (total of ~62%) of the variance

fviz_pca_var(nuts_pca_above) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - above (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_above_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# get var contributing for cache
## so PC1 is largely driven by din and diss_ammonia (~54%)
### and PC2 is largely driven by lat and long (~55%)
var <- get_pca_var(nuts_pca_cache)
var_exp <- var$contrib %>% as.data.frame %>% rownames_to_column() #error
head(var_exp, 6)

# get var contributing for yolo
## so PC1 is largely driven by din and diss_ammonia (~52%)
### and PC2 is largely driven by lat and long (~56%)
var <- get_pca_var(nuts_pca_yolo)
var_exp <- var$contrib %>% as.data.frame %>% rownames_to_column() #error
head(var_exp, 6)


# get var contributing for below
## so PC1 is largely driven by din and diss_ammonia (~51%)
### and PC2 is largely driven by lat and long (~71%)
var <- get_pca_var(nuts_pca_below)
var_exp <- var$contrib %>% as.data.frame %>% rownames_to_column() #error
head(var_exp, 6)

# get var contributing for above
## so PC1 is largely driven by din and diss_ammonia but diss_nitrate_nitrite similar to diss_ammonia (~51%)
### and PC2 is largely driven by lat and long (~71%)
var <- get_pca_var(nuts_pca_below)
var_exp <- var$contrib %>% as.data.frame %>% rownames_to_column() #error
head(var_exp, 6)


