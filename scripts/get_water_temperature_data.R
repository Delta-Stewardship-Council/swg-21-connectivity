#### get_water_temperature_data.R
#### Download continuous water temperature data from EDI

library(here) # directory
library(readr)

WaterTemp <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.591.2&entityid=fb147771773b8354667a0b43e3f8e0e4")

save(WaterTemp, file = "ContinuousWaterTemp_1985_2019.rda", compress = "xz")

