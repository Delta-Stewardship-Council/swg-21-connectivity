# PCA of nutrient data
# R. Peek


# Libraries ---------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
options(scipen = 999) # turn off scientific notation
library(missMDA) # estimate missing values for PCA
library(FactoMineR)
library(factoextra) # clustering visualization/stats
library(broom) # stats functions

# Data --------------------------------------------------------------------

# chla data
source("scripts/functions/f_load_wq_chla_nuts.R")
chla_nuts <- f_load_wq_chla_nuts()

# Impute Missing Values ---------------------------------------------------

# a few methods here, but generally the approaches are:
## use mean of the variable with NAs to impute
## impute missing values using linear regression

# select only quantitative data
chla_quant <- chla_nuts %>%
  select(where(is.numeric)) %>%
  select_if(~ any(is.na(.)))



library(missMDA) # estimate missing values for PCA

# estimate number of princ components
#nPCs <- estim_ncpPCA(chla_quant)
#nPCs$ncp

# calc missing
chla_completed <- imputePCA(chla_quant, ncp = 5, scale = TRUE)

# PCA: With FactoExtra ----------------------------------------------------

PCA(chla_completed$completeObs)

# PCA: With tidy ----------------------------------------------------------

# arrow style
arrow_style <- arrow(
  angle = 20, length = grid::unit(8, "pt"),
  ends = "first", type = "closed"
)

# select, scale data (mean of zero, sd =1), and pca
nuts_pca <- chla_completed$completeObs %>% as_tibble() %>%
  scale() %>%
  prcomp()

# add PC coordinates and PLOT: PC1 vs P2
nuts_pca %>%
  augment(chla_nuts) %>% #head()
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_point(aes(color=source), alpha=0.8, size=4.5) +
  theme_classic() +
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

# so first two PCs explain 41% and 32% (total of ~73%) of the variance

fviz_pca_var(nuts_pca) +
  theme(plot.background = element_rect(fill="white")) +
  labs(title="PCA of Nutrients (imputed missing values)")

ggsave(filename = "figures/pca_chla_nutrients_rotation_matrix.png", width = 9, height = 7, units = "in", dpi=300)

# get var contributing
var <- get_pca_var(nuts_pca)
var_exp <- var$contrib %>% as.data.frame %>% rownames_to_column()
head(var_exp, 6)

## so PC1 is largely driven by depth and diss_ammonia (~50%)
## and PC2 is largely driven by diss_nitrate_nitrite and diss_orthophos (~50%)
