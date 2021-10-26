# Explore and compile phytoplankton data for SAM model

library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

# Import data
emp1 <- read_csv("data/EMP_Phytoplankton_1975_2016_Data.csv") %>%
  mutate(SampleDate = as.POSIXct(SampleDate, format = "%m/%d/%y",
                                 tz = "US/Pacific"))

emp2 <- read_xlsx("data/EMP_Phytoplankton_2017_2018_Data.xlsx",
                  sheet = "2017 Algal Type Data") %>%
  mutate(Date = as.POSIXct(Date, tz = "US/Pacific"))

emp3 <- read_xlsx("data/EMP_Phytoplankton_2017_2018_Data.xlsx",
                  sheet = "2018 Algal Type Data") %>%
  mutate(Date = as.POSIXct(Date, tz = "US/Pacific"))

emp4 <- read_xlsx("data/EMP_Phytoplankton_2019_Data.xlsx",
                   "2019 Data by Algal Type") %>%
  mutate(Date = as.POSIXct(Date, tz = "US/Pacific"))

cnames <- list(colnames(emp1),
               colnames(emp2),
               colnames(emp3),
               colnames(emp4))

tnames <- c("Date", "Station", "Pennate", "Centric",
            "Chrysophyte", "Cyanobacter", "Cryptophyt", "Dinoflag",
            "Euglenoid", "Eustigmato", "Green", "Xantho",
            "Cilliat", "Hapto")

mat <- matrix(NA, nrow = 4, ncol = length(tnames))
for(i in 1:4){
  vind <- c()
  for(j in 1:length(tnames)){
    temp <- ifelse(length(grep(tnames[j], cnames[[i]])) == 0,
                   NA,
                   grep(tnames[j], cnames[[i]]))
    vind <- c(vind, temp)
  }
  mat[i,] <- vind
}

# which are present for all 4 datasets
tind <- which(colSums(mat) > 0)

# combine
emp <- as.data.frame(mapply(c,
                            emp1[, mat[1, tind]],
                            emp2[, mat[2, tind]],
                            emp3[, mat[3, tind]],
                            emp4[, mat[4, tind]])) %>%
  mutate_at(3:10, as.numeric) %>%
  mutate(SampleDate = as.POSIXct(as.numeric(SampleDate),
                                 origin = "1970-01-01",
                                 tz = "US/Pacific"),
         totBioVol = select(., 'Pennate Diatoms':'Green Algae') %>% rowSums())

# Filter by sites of interest
sites <- c("C3A", "D24", "D4")
emp_sub <- emp %>%
  filter(StationCode %in% sites)

emp_sub %>%
  filter(SampleDate >= as.POSIXct("2005-01-01")) %>%
  ggplot(aes(x = SampleDate, y = totBioVol)) +
    geom_point() +
    facet_wrap(~StationCode, scale = "free_y")
