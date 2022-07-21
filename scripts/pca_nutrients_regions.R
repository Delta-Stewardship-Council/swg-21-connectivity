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

#Add nutrients to final chlorophyll and then do PCA? would reduce n quite a bit

#Create unique df of stations/regions----------------------

location <- location[c(2, 6)]

unique_location <- unique(location)

dat_na <- data.frame(sapply(X = explore, FUN = function(x) sum(is.na(x))))

#Remove nutrients if not paired with chl-a----------------------

new_dat <- explore[,c(2:5, 7,10,12,14,15,18)]

new_dat_chl <- new_dat[!is.na(new_dat$chlorophyll),]

summary(new_dat_chl)

new_dat_chl <- left_join(new_dat_chl, unique_location, by = "station_wq_chl")

#check Shag Slough station (USGS-11455276")-------------------

#cache <- subset(new_dat_chl, location == "cache")

#shag <- subset(new_dat_chl, station_wq_chl == "USGS-11455276")

#shag_unique <- unique(shag$date)

#cache_unique <- unique(cache$date)

#setdiff(shag_unique, cache_unique)

#make location a number

new_dat_chl <- new_dat_chl %>%
  mutate(location = case_when(
    location == "above"~1,
    location == "below"~2,
    location == "cache"~3,
    location == "yolo"~4,
    #TRUE~location
  ))


#make location a factor------------------

#new_dat_chl$location <- as.factor(new_dat_chl$location)

#make location as.integer--------------------

#new_dat_chl$location <- as.factor(new_dat_chl$location)

#Format date----------------------------

new_dat_chl$date <- as.POSIXct(new_dat_chl$date, format = "%Y-%m-%d")

#Add month-------------------------------------------

new_dat_chl$month <- format(new_dat_chl[, "date"], "%m")

new_dat_chl$month <- as.integer(new_dat_chl$month)

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

#Stich together full df - include columns missing from imputed data--------------

df_for_PCA_cache <- cbind(nut_completed_cache_df, df_for_PCA_cache[,c(1, 3, 4, 6, 11, 12)])
df_for_PCA_yolo <- cbind(nut_completed_yolo_df, df_for_PCA_yolo[,c(1, 3, 4, 6, 11, 12)])
df_for_PCA_below <- cbind(nut_completed_below_df, df_for_PCA_below[,c(1, 3, 4, 6, 11, 12)])
df_for_PCA_above <- cbind(nut_completed_above_df, df_for_PCA_above[,c(1, 3, 4, 6, 11, 12)])

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

#nuts_pca_cache <- df_for_PCA_cache %>% as_tibble() %>%
  #scale() %>%
  #prcomp()

#nuts_pca_yolo <- df_for_PCA_yolo %>% as_tibble() %>%
  #scale() %>%
  #prcomp()

#nuts_pca_below <- df_for_PCA_below %>% as_tibble() %>%
  #scale() %>%
  #prcomp()

#nuts_pca_above <- df_for_PCA_above %>% as_tibble() %>%
  #scale() %>%
  #prcomp()

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

#nuts_pca_cache %>%
  #augment(df_for_PCA_cache) %>% #head()
  #ggplot(aes(.fittedPC1, .fittedPC2)) +
  #geom_point(aes(color=latitude), alpha=0.8, size=4.5) +
  #theme_classic() +
  #scale_color_viridis_d() +
  #labs(title = "PCA of Nutrients",
       #x="PC1", y="PC2") +
  #guides(fill = guide_legend(
    #override.aes = aes(label = "")))

#nuts_pca_yolo %>%
  #augment(df_for_PCA_yolo) %>% #head()
  #ggplot(aes(.fittedPC1, .fittedPC2)) +
  #geom_point(aes(color=latitude), alpha=0.8, size=4.5) +
  #theme_classic() +
  #scale_color_viridis_d() +
  #labs(title = "PCA of Nutrients",
       #x="PC1", y="PC2") +
  #guides(fill = guide_legend(
    #override.aes = aes(label = "")))

#nuts_pca_below %>%
  #augment(df_for_PCA_below) %>% #head()
  #ggplot(aes(.fittedPC1, .fittedPC2)) +
  #geom_point(aes(color=latitude), alpha=0.8, size=4.5) +
  #theme_classic() +
  #scale_color_viridis_d() +
  #labs(title = "PCA of Nutrients",
       #x="PC1", y="PC2") +
  #guides(fill = guide_legend(
    #override.aes = aes(label = "")))

#nuts_pca_above %>%
  #augment(df_for_PCA_above) %>% #head()
  #ggplot(aes(.fittedPC1, .fittedPC2)) +
  #geom_point(aes(color=latitude), alpha=0.8, size=4.5) +
  #theme_classic() +
  #scale_color_viridis_d() +
  #labs(title = "PCA of Nutrients",
       #x="PC1", y="PC2") +
  #guides(fill = guide_legend(
    #override.aes = aes(label = "")))

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

#nuts_pca_cache %>%
  #tidy(matrix = "rotation") %>%
  #pivot_wider(
    #names_from = "PC", values_from = "value",
    #names_prefix = "PC"
  #) %>%
  #ggplot(aes(PC1, PC2)) + ggtitle("Cache") +
  #geom_segment(
    #xend = 0, yend = 0,
    #arrow = arrow_style
 # ) +
  #geom_text(aes(label = column), hjust = 1) +
  #xlim(-1.5, 0.5) + ylim(-1, 1) +
  #coord_fixed()

#nuts_pca_yolo %>%
  #tidy(matrix = "rotation") %>%
  #pivot_wider(
    #names_from = "PC", values_from = "value",
    #names_prefix = "PC"
  #) %>%
  #ggplot(aes(PC1, PC2)) + ggtitle("Yolo")+
  #geom_segment(
    #xend = 0, yend = 0,
    #arrow = arrow_style
  #) +
  #geom_text(aes(label = column), hjust = 1) +
  #xlim(-0.5, 0.5) + ylim(-1, 1) +
  #coord_fixed()

#nuts_pca_below %>%
  #tidy(matrix = "rotation") %>%
  #pivot_wider(
    #names_from = "PC", values_from = "value",
    #names_prefix = "PC"
 # ) %>%
  #ggplot(aes(PC1, PC2)) + ggtitle("Below") +
  #geom_segment(
    #xend = 0, yend = 0,
    #arrow = arrow_style
  #) +
  #geom_text(aes(label = column), hjust = 1) +
 # xlim(-1.5, 0.5) + ylim(-1, 1) +
  #coord_fixed()

#nuts_pca_above %>%
  #tidy(matrix = "rotation") %>%
  #pivot_wider(
    #names_from = "PC", values_from = "value",
    #names_prefix = "PC"
  #) %>%
  #ggplot(aes(PC1, PC2)) + ggtitle("Above") +
  #geom_segment(
    #xend = 0, yend = 0,
    #arrow = arrow_style
  #) +
  #geom_text(aes(label = column), hjust = 1) +
  #xlim(-1.5, 0.5) + ylim(-1, 1) +
 # coord_fixed()
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

#nuts_pca_cache %>%
  # extract eigenvalues
  #tidy(matrix = "eigenvalues") %>%
  #ggplot(aes(PC, percent)) +
  #geom_col() +
  #scale_x_continuous(
    # create one axis tick per PC
    #breaks = 1:6
  #) +
  #scale_y_continuous(
    #name = "variance explained",
    # format y axis ticks as percent values
    #label = scales::label_percent(accuracy = 1)
  #)

#nuts_pca_yolo %>%
  # extract eigenvalues
  #tidy(matrix = "eigenvalues") %>%
  #ggplot(aes(PC, percent)) +
  #geom_col() +
  #scale_x_continuous(
    # create one axis tick per PC
    #breaks = 1:6
  #) +
  #scale_y_continuous(
   # name = "variance explained",
    # format y axis ticks as percent values
    #label = scales::label_percent(accuracy = 1)
  #)

#nuts_pca_below %>%
  # extract eigenvalues
  #tidy(matrix = "eigenvalues") %>%
  #ggplot(aes(PC, percent)) +
  #geom_col() +
  #scale_x_continuous(
    # create one axis tick per PC
    #breaks = 1:6
  #) +
  #scale_y_continuous(
    #name = "variance explained",
    # format y axis ticks as percent values
    #label = scales::label_percent(accuracy = 1)
 # )

#nuts_pca_above %>%
  # extract eigenvalues
  #tidy(matrix = "eigenvalues") %>%
  #ggplot(aes(PC, percent)) +
  #geom_col() +
  #scale_x_continuous(
    # create one axis tick per PC
    #breaks = 1:6
  #) +
  #scale_y_continuous(
    #name = "variance explained",
    # format y axis ticks as percent values
    #label = scales::label_percent(accuracy = 1)
  #)

#All regions PCA-------------------------------
# PC1 and PC2 explain ~34 and 22% of variance
fviz_pca_var(nuts_pca) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - all regions (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# for 'cache' first two PCs explain ~39 and 20% (total of ~59%) of the variance (when month added 34 + 18)

fviz_pca_var(nuts_pca_cache) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - cache (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_cache_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# for 'yolo' first two PCs explain ~37% and 27% (total of ~64%) of the variance (when month added 33 + 19)

fviz_pca_var(nuts_pca_yolo) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - yolo (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_yolo_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# for 'below' first two PCs explain ~39% and 26% (total of ~65%) of the variance (when month added 33 + 19)

fviz_pca_var(nuts_pca_below) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - below (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_below_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# for 'above' first two PCs explain ~33% and 29% (total of ~62%) of the variance (when month added 30 + 25)

fviz_pca_var(nuts_pca_above) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients - above (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_above_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

#cbind data out ------------------------------------------------
#dat_final <- cbind(df_for_PCA, nuts_pca$x)

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

#Unique ID - unique ID in location df? for now will left join dat_final and chl_fin by station and date ----------------------------

dat_final_test <- left_join(chl_fin, dat_final, by

#dat_final <- dat_final %>%
  #mutate(data_id = paste0(location_name,
                        #"_", doy1998))

#length(unique(dat_final$data_id))==nrow(dat_final)

#Notes ------------------------------------------
#get var contributing for cache
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


