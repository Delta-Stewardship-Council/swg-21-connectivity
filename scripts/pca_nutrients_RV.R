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

dat_na <- data.frame(sapply(X = explore, FUN = function(x) sum(is.na(x))))

#remove nutrients if not paired with chl-a

new_dat <- explore[,c(2:5, 7,10,12,14,15)]

new_dat_chl <- new_dat[!is.na(new_dat$chlorophyll),]

summary(new_dat_chl)

#sum(is.na(new_dat_chl$din == TRUE))

#break stations up by region - check assignments begins on line 66

RV <- subset(new_dat_chl, latitude>=38.13&latitude<= 38.22)
CSC <- subset(new_dat_chl, latitude>=38.221&latitude<= 38.35)
Yolo <- subset(new_dat_chl, latitude>=38.3501&longitude<=-121.568291)
SacR <- subset(new_dat_chl, longitude>=-121.5727)

#add regions to data frame - lower Sac R data dropped (d/s of Rio Vista)

RV$region <- ("RV")
CSC$region <- ("CSC")
Yolo$region <- ("Yolo")
SacR$region <- ("SacR")

#add back in

new_dat_chl_region <- rbind(RV, CSC, Yolo, SacR)

new_dat_chl_region$region <- as.factor(new_dat_chl_region$region)

#skip to line ~160 for 4/21/22

#look quick at relationship
plot(new_dat_chl$latitude, new_dat_chl$diss_nitrate_nitrite)

plot(new_dat_chl$din, new_dat_chl$chlorophyll)

lm <- lm(new_dat_chl$chlorophyll~new_dat_chl$din)

summary(lm)

#format date

new_dat_chl$date <- as.POSIXct(new_dat_chl$date, format = "%Y-%m-%d")

#break up by regions and assign region character to data frame

unique(new_dat_chl$station_wq_chl)

#Rio Vista

RV <- subset(new_dat_chl, latitude>=38.13&latitude<= 38.22)

unique(RV$station_wq_chl)

plot(RV$latitude, RV$din)

sum(RV$din <10, na.rm = TRUE) #238

RV$region <- ("RV")

#CSC

CSC <- subset(new_dat_chl, latitude>=38.221&latitude<= 38.35)

unique(CSC$station_wq_chl)

plot(CSC$latitude, CSC$din)

plot(CSC$din, CSC$chlorophyll)

summary(CSC$din)

sum(CSC$din <10, na.rm = TRUE) #114

CSC$region <- ("CSC")

#Yolo

Yolo <- subset(new_dat_chl, latitude>=38.3501&longitude<=-121.568291)

unique(Yolo$station_wq_chl)

plot(Yolo$din, Yolo$chlorophyll)

summary(Yolo$din)

sum(Yolo$din <10, na.rm = TRUE) #310

Yolo$region <- ("Yolo")

#Sac R

SacR <- subset(new_dat_chl, longitude>=-121.5727)

unique(SacR$station_wq_chl)

plot(SacR$din, SacR$chlorophyll)

summary(SacR$din)

sum(SacR$din <10, na.rm = TRUE) #100

SacR$region <- ("SacR")

#subset data >= WY 2009 - didn't run yet

Yolo_2009 <- subset(Yolo, date>="2008-10-01")
CSC_2009 <- subset(CSC, date>="2008-10-01")
SacR_2009 <- subset(SacR, date>="2008-10-01")
RV_2009 <- subset(RV, date>="2008-10-01")

df_for_PCA <- rbind(RV, CSC, Yolo, SacR)

sum(df_for_PCA$din <10, na.rm = TRUE)#765
sum(df_for_PCA$chlorophyll <100, na.rm = TRUE)#1803
plot(df_for_PCA$date, df_for_PCA$chlorophyll)
plot(df_for_PCA$date, df_for_PCA$din)

#Ryan helped here

# select only quantitative data v2
nut_quant <- new_dat_chl_region %>%
  select(where(is.numeric)) %>%
  select_if(~ any(is.na(.)))

summary(nut_quant)

nut_completed <- imputePCA(nut_quant, ncp = 2, scale = TRUE)

nut_completed_df <- nut_completed$completeObs

summary(nut_completed_df)

#stich together full df - include columns missing from imputed data

df_for_PCA <- cbind(nut_completed_df, new_dat_chl_region[,c(1, 3, 4, 6)])

summary(df_for_PCA)

# PCA: With FactoExtra
PCA(df_for_PCA)

# PCA: With tidy
# arrow style
arrow_style <- arrow(
  angle = 20, length = grid::unit(8, "pt"),
  ends = "first", type = "closed"
)

# select, scale data (mean of zero, sd =1), and pca
nuts_pca <- df_for_PCA %>% as_tibble() %>%
  scale() %>%
  prcomp()

#add PC coordinates and PLOT: PC1 vs P2
nuts_pca %>%
  augment(df_for_PCA) %>% #head()
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(color=latitude), alpha=0.8, size=4.5) +
  + theme_classic() +
  scale_color_viridis_d() +
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
  ggplot(aes(PC1, PC2)) +
  geom_segment(
    xend = 0, yend = 0,
    arrow = arrow_style
  ) +
  geom_text(aes(label = column), hjust = 1) +
  #xlim(-1.5, 0.5) + ylim(-1, 1) +
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

# so first two PCs explain ~40% and 29% (total of ~23%) of the variance

fviz_pca_var(nuts_pca) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# get var contributing
var <- get_pca_var(nuts_pca)
var_exp <- var$contrib %>% as.data.frame %>% rownames_to_column() #error
head(var_exp, 6)

## so PC1 is largely driven by  (~40%)
## and PC2 is largely driven by  (~24%)
