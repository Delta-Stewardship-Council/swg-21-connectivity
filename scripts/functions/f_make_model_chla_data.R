# make chla_all data for models


# Libraries ---------------------------------------------------------------

library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(tidyr)
library(ggplot2)

f_make_model_chla_data <- function(){

# Read in integrated data
  wq <- read_csv("data_clean/clean_integrated_wq.csv")

  table(wq$station)

  # Select sites near Rio vista bridge
  wq_sel <- wq %>%
    filter(station %in% c("USGS 657", "USGS 659", "USGS 662",
                          "USBR 16", "USBR 44", "USBR 44", "USBR 34")) %>%
    select(station, date, chla = chlorophyll)

  # Read in USGS CAWSC data: scripts/functions/f_clean_usgs_cawsc_chla.R
  usgs_chla <- read_csv("data_clean/clean_chla_usgs_cawsc.csv") %>%
    filter(site_no %in% c(11455350, 11455385, 11455315, 11455420)) %>%
    rename(station = site_no, chla = chla_value) %>%
    mutate(station = as.character(station))

  # merge two by matching/renaming and cbinding
  chla_comb <- bind_rows(wq_sel, usgs_chla) %>%
    fill(chla_units, .direction = "updown")

  # Read in YOLO inundation and verona flow
  #df_old <- read_csv("data/yolo_daymet_for_model.csv") # original
  inun <- read_csv("data_clean/clean_inundation_days.csv")
  # Calculate # of topped days in past 30 days
  inun$past_topped <- NA
  for(i in 31:nrow(inun)) {
    inun$past_topped[i] <- sum(inun$inundation[(i-31):(i-1)])
  }

  verona <- read_csv("data_clean/clean_flow_usgs_11425500.csv")
  daymet <- read_csv("data_clean/clean_daymet_yolo_1994-2020.csv") #%>%

  # merge data
  df_mod <- left_join(chla_comb, daymet) %>%
    left_join(inun) %>%
    left_join(verona) %>%
    tidyr::drop_na(daymet_tmax)

  summary(df_mod)

  range(df_mod$date) # n = 612, 1997-01-28 to 2020-12-03

  # Visualize
  ggplot() +
    geom_rect(data = df_mod %>% filter(inundation == 1),
              aes(xmin = date, xmax = date,
                  ymin = -Inf, ymax = Inf,
                  color = inund.days)) +
    geom_point(data = df_mod, aes(x = date, y = chla*300, color = chla)) +
    geom_point(data = df_mod, aes(x = date, y = flow), color="skyblue") +
    labs(y = expression(paste("Q (", ft^3, " ", s^-1, ")"))) +
    scale_y_continuous(sec.axis = sec_axis(~./300,
                                           name = expression(paste("Chlorophyll (", mu, "g/mL)")))) +
    #scale_x_date(limits = c(as.Date("1998-10-01"), as.Date("2021-10-01"))) +
    scale_color_viridis_c() +
    theme_bw(base_size = 12) +
    theme(axis.title.x = element_blank()) #+
    #guides(color = "none")


  # write out as rds
  write_rds(df_mod, file = "data_model/chla_w_all_data.rds")
}
