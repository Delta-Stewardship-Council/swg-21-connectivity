# Explore and compile phytoplankton data for SAM model

library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

# Import data
emp <- read_csv("data/notQAd_1975-2020_phyto.zip")

