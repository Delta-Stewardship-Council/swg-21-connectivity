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




