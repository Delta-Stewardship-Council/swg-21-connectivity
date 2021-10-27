# Download Phytoplankton Data
# data pull from this data repository:
# https://emp.baydeltalive.com/projects/11282

library(tidyverse)
library(glue) # to paste things together
library(here) # directory

# Download Data -------------------------------------------------------------

# download the files
download.file("https://emp.baydeltalive.com/assets/e106ca2a359a122e74e33ef183a0fb4a/application/vnd.ms-excel/EMP_Phytoplankton_2017_2018_Data.xlsx", destfile = here("data/EMP_Phytoplankton_2017_2018_Data.xlsx"))

download.file("https://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/Phytoplankton_Algal_Type_Data_1975_-2016.csv", destfile = here("data/EMP_Phytoplankton_1975_2016_Data.csv"))

download.file("https://emp.baydeltalive.com/assets/e106ca2a359a122e74e33ef183a0fb4a/application/vnd.ms-excel/EMP_Phytoplankton_2019_Data.xlsx", destfile = here("data/EMP_Phytoplankton_2019_Data.xlsx"))

# download the stations locations
download.file("https://wwww.water.ca.gov/bdma/docs/150722.Phytoplankton_Stations.xls", destfile = here("data/EMP_Phytoplankton_Stations.xlsx"))


# Tidy Data and Join Together ---------------------------------------------

library(readxl)

ph75 <- read_csv(here("data/EMP_Phytoplankton_1975_2016_Data.csv"))
ph17 <- read_xlsx(here("data/EMP_Phytoplankton_2017_2018_Data.xlsx"), sheet = "2017 Algal Type Data")
ph18 <- read_xlsx(here("data/EMP_Phytoplankton_2017_2018_Data.xlsx"), sheet = "2018 Algal Type Data")
ph19 <- read_xlsx(here("data/EMP_Phytoplankton_2019_Data.xlsx"), sheet = "2019 Data by Algal Type")

## combine the files

