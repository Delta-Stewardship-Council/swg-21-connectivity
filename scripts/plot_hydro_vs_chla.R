
# plot Rio Vista, Verona and Yolo hydrographs with chl-a data

# libraries
library(tidyverse)
library(ggthemes)
library(scales)
library(cowplot)
library(ggdark)
options(scipen = 999) # turn off scientific notation


# Get Flow Data --------------------------------------------------------------------

# get RioVista
# from f_clean_flow_usgs_11455420.R
flow_rv <- read_csv("data_clean/clean_flow_usgs_11455420.csv") %>%
  # drop row ids
  select(-1) %>%
  filter(lubridate::year(date)>1997)

# Get Verona
source("scripts/functions/f_load_flow_verona.R")
flow_ver <- f_load_flow_verona() %>%
  filter(water_year>1997)

# Get Chla data -----------------------------------------------------------

source("scripts/functions/f_load_wq_chla_nuts.R")
chla_nuts <- f_load_wq_chla_nuts()


# Hydrograph --------------------------------------------------------------

# hydroplot
ggplot() +
  geom_line(data=flow_rv, aes(x=date, y=Q_tf)) +
  ggdark::dark_theme_classic() +
  labs(x="", subtitle="Rio Vista Flow") +
  scale_y_continuous(name="Q tidally filtered (cfs)",
                     labels=comma)


# chla vs. flow -----------------------------------------------------------

# combine data
chla_flow <- left_join(chla_nuts, flow_rv, by="date")
# make a scalefactor
scaleFactor <- max(chla_flow$Q_tf, na.rm = T) / max(chla_flow$chlorophyll, na.rm = T)

ggplot() + geom_line(data=chla_flow, aes(x=date, y=Q_tf)) +
  ggdark::dark_theme_classic() +
  labs(x="", subtitle="Rio Vista Flow") +
  scale_y_continuous(name="Q tidally filtered (cfs)",
                     labels=comma,
                     sec.axis =
                       sec_axis(~.*scaleFactor, name = "Chl-a"))+
                     #labels=c(0, 2, 4, 6, 8, 10)) +
  geom_point(data=chla_flow, aes(x=date, y=chlorophyll*scaleFactor), alpha=0.3, pch=24, fill="forestgreen", size=2) +
  theme(
    axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "forestgreen"))



# Just flow vs chla no date -----------------------------------------------


ggplot() + geom_point(data=chla_flow %>% filter(chlorophyll>0.02, Q_tf > 0.02), aes(x=chlorophyll, y=Q_tf), pch=21) +
  ggdark::dark_theme_classic() +
  labs(x="", subtitle="Rio Vista Flow") +
  scale_y_continuous(name="Chl-a vs flow",
                     labels=comma) +
  facet_wrap(~station_wq_chl)
