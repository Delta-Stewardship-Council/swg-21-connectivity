# Download DJFMP Data
# data pull from this data repository:
# https://emp.baydeltalive.com/projects/11282

library(tidyverse)
library(glue) # to paste things together
library(here) # directory

# Download Data -------------------------------------------------------------

# download the files
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=71c16ead9b8ffa4da7a52da180f601f4", destfile = here("data/DJFMP_Fish_Trawl_1976_2001_Data.csv"))

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=0edf413c39ac8b111a576d894306a60f", destfile = here("data/DJFMP_Fish_Trawl_2002_2020_Data.csv"))

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=a3e94e8f0cf6f675d716151c1ca17b4f", destfile = here("data/DJFMP_Fish_Seine_1976_2020_Data.csv"))

# download the stations locations
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=99a038d691f27cd306ff93fdcbc03b77", destfile = here("data/DJFMP_Fish_stations.csv"))


# Tidy Data and Join Together ---------------------------------------------

library(readxl)

#djfmp_76_01 <- read_csv(here("data/DJFMP_Fish_1976_2001.csv"), col_types = c(""))
#djfmp_02_20 <- read_xlsx(here("data/DJFMP_Fish_2002_2020_Data.xlsx"), )
#djfmp_seine_76_20 <- read_xlsx(here("data/DJFMP_Fish_Seine_1976_2020_Data.xlsx"))
#djfmp_stations_76_20 <- read_xlsx(here("data/DJFMP_Fish_Seine_1976_2020_Data.xlsx"))

## combine the files
