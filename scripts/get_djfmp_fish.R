# Download DJFMP Data
# data pull

library(tidyverse)
library(glue) # to paste things together
library(here) # directory
library(devtools)
install_github("sbashevkin/LTMRdata") # for DJFMP data
require(LTMRdata)

# Download Data -------------------------------------------------------------

# download the files
#download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=71c16ead9b8ffa4da7a52da180f601f4", destfile = here("data/DJFMP_Fish_Trawl_1976_2001_Data.csv"))

#download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=0edf413c39ac8b111a576d894306a60f", destfile = here("data/DJFMP_Fish_Trawl_2002_2020_Data.csv"))

#download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=a3e94e8f0cf6f675d716151c1ca17b4f", destfile = here("data/DJFMP_Fish_Seine_1976_2020_Data.csv"))

# Get djfmp data files from LTMRdata package instead:  https://github.com/sbashevkin/LTMRdata
DJFMP_all <- LTMRdata::DJFMP

# download the stations locations (if a separate file is desired)
djfmp_stations_76_20 <- download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=99a038d691f27cd306ff93fdcbc03b77", destfile = here("data/DJFMP_Fish_stations.csv"))

# Explore beach seine data from 1998 to present
DJFMP_seine_98_now <- DJFMP_all %>% filter(Method == "Beach seine") %>% filter(Datetime > "1997-12-31")

# Look specifically at cyprinids: Sac blackfish (Orthodon microlepidotus), Sac pikeminnow (Ptychocheilus grandis), splittail (Pogonichthys macrolepidotus), and Sac sucker (Catostomus occidentalis), within latitudes 38.156314 to 39

DJFMP_seine_98_now_4cyp <- DJFMP_seine_98_now %>%
  filter(Taxa %in% c("Orthodon microlepidotus","Ptychocheilus grandis", "Pogonichthys macrolepidotus", "Catostomus occidentalis")) %>%
  filter(Latitude > 38.156 & Latitude < 39) %>%
  filter(Station != "SF014E")

# Liberty Island
LI_stations <- unique(DJFMP_seine_98_now_4cyp$Station[grep("LI", DJFMP_seine_98_now_4cyp$Station)])

# American River
AM_stations <- unique(DJFMP_seine_98_now_4cyp$Station[grep("AM", DJFMP_seine_98_now_4cyp$Station)])

# Georgiana Slough
GS_stations <- unique(DJFMP_seine_98_now_4cyp$Station[grep("GS", DJFMP_seine_98_now_4cyp$Station)])

# Sac River
SR_stations <- unique(DJFMP_seine_98_now_4cyp$Station[grep("SR", DJFMP_seine_98_now_4cyp$Station)])

# Steamboat Slough
SS_stations <- unique(DJFMP_seine_98_now_4cyp$Station[grep("SS", DJFMP_seine_98_now_4cyp$Station)])

# Delta Cross Channel
XC_stations <- unique(DJFMP_seine_98_now_4cyp$Station[grep("XC", DJFMP_seine_98_now_4cyp$Station)])

DJFMP_seine_98_now_4cyp <- DJFMP_seine_98_now_4cyp %>%
  dplyr::mutate(Region = dplyr::case_when(Station %in% c(LI_stations) ~ "Liberty_Island", Station %in% c(GS_stations) ~ "Georgiana_Slough", Station %in% c(AM_stations) ~ "American_River", Station %in% c(SR_stations) ~ "Sac_River", Station %in% c(SS_stations) ~ "Steamboat_Slough", Station %in% c(XC_stations) ~ "Delta_XC"))
# plot the 4 cyprinid species over time period of interest
cyp_plot <- ggplot(DJFMP_seine_98_now_4cyp, aes(x = Date, y = Count, group = Taxa, color = Taxa)) + geom_point() + theme_bw() + facet_wrap(vars(Region))
