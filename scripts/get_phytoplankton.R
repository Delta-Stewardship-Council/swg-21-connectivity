# Download Phytoplankton Data
# data pull from this data repository:
# https://emp.baydeltalive.com/projects/11282

library(tidyverse)
library(glue) # to paste things together
library(here) # directory
library(readxl)

# Download Data -------------------------------------------------------------

# download the files
download.file("https://emp.baydeltalive.com/assets/e106ca2a359a122e74e33ef183a0fb4a/application/vnd.ms-excel/EMP_Phytoplankton_2017_2018_Data.xlsx", destfile = here("data/EMP_Phytoplankton_2017_2018_Data.xlsx"))

download.file("https://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/Phytoplankton_Algal_Type_Data_1975_-2016.csv", destfile = here("data/EMP_Phytoplankton_1975_2016_Data.csv"))

download.file("https://emp.baydeltalive.com/assets/e106ca2a359a122e74e33ef183a0fb4a/application/vnd.ms-excel/EMP_Phytoplankton_2019_Data.xlsx", destfile = here("data/EMP_Phytoplankton_2019_Data.xlsx"))
