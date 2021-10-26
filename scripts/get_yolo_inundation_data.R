# get inundation data

# Download Yolo Inundation Data
# data pull from this data repository:
# https://portal.edirepository.org/nis/mapbrowse?packageid=edi.840.1

library(tidyverse)
library(glue) # to paste things together
library(here) # directory

# Download Data -------------------------------------------------------------

# download the files
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.840.1&entityid=186964642c42e5a8b8e44fc87ff10bbf", destfile = here("data/Yolo_Bypass_Inundation1998-2021.csv"))



# Read in Data ------------------------------------------------------------

yolo_inund <- read_csv("data/Yolo_Bypass_Inundation1998-2021.csv")
