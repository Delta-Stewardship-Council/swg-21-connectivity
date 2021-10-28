# Switch to chlorophyll data


library(readr)
library(dplyr)
library(ggplot2)

library(sf)
library(mapview)

# Read in all data
delta <- read_csv("data/Delta_Integrated_WQ.csv.zip") %>%
  filter(!is.na(Chlorophyll))

table(delta$Station)

delta %>%
  filter(grepl("EMP", Station)) %>%
  ggplot(aes(x = Date, y = Chlorophyll)) +
  geom_point() +
  facet_wrap(~Station, scales = "free_y")


# make map
delta_map <- delta %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326, remove = FALSE)
mapviewOptions(fgb=TRUE)
mapview(delta_map, zcol = "Source", burst = TRUE)

# Isolate USGS 36, match to Lisbon or TOE

ch <- delta %>%
  filter(Station == "USGS 36" & !is.na(Chlorophyll))

ggplot(ch, aes(x = Date, y = Chlorophyll)) +
  geom_point()

ggplot(ch, aes(x = Salinity, y = Chlorophyll)) +
  geom_point()

# Bring in dayflow for TOE
cch <- read_csv("../swg-21-flow/data/daily_CCH.csv") %>%
  rename(Date = date)

# Bring in overtopping for YOLO
yolo <- read_csv("data/Yolo_Bypass_Inundation1998-2021.csv")


# Combine

all <- left_join(ch, cch, by = "Date") %>%
  rename(cch_discharge_daily = mean_discharge) %>%
  left_join(yolo, by = "Date")

ggplot(all, aes(x = Date)) +
  geom_point(aes(y = yolo_Q)) +
  geom_point(aes(y = Chlorophyll, color = "Chloro"))


ggplot()+
  geom_rect(data = yolo %>% filter(Inundation == TRUE),
            aes(xmin = Date, xmax = Date,
                ymin = -Inf, ymax = Inf,
                color = Inundation)) +
  geom_point(data = all, aes(x = Date, y = Chlorophyll*650, col = "Chlorophyll")) +
  geom_point(data = all, aes(x = Date, y = cch_discharge_daily, col = "Q")) +
  labs(y = expression(paste("Q (", ft^3, " ", s^-1, ")"))) +
  scale_y_continuous(sec.axis = sec_axis(~./650, name = "Chlorophyll")) +
  scale_x_date(limits = c(as.Date("1998-01-01"), as.Date("2020-01-01"))) +
  scale_color_manual(values = c("forestgreen", "skyblue", "orchid")) +
  theme_bw(base_size = 12) +
  guides(color = "none")


ggplot(all, aes(x = cch_discharge_daily, y = Chlorophyll)) +
  geom_point()


# New site, USGS CAWSC
ch2 <- read_csv("data/USGS_CAWSC_discrete_connectivity_clean.csv") %>%
  filter(site_no %in% c(11455350, 11455385)) %>%
  select(site_no, sample_dt, contains("result_va_Chla")) %>%
  filter(!is.na(result_va_Chla_ugL) == TRUE) %>%
  left_join(cch, by = c("sample_dt" = "Date")) %>%
  rename(Date = sample_dt,
         chloro_a = result_va_Chla_ugL,
         cch_discharge_daily = mean_discharge)
ggplot()+
  geom_rect(data = yolo %>% filter(Overtopping_FW == "Yes"),
            aes(xmin = Date, xmax = Date,
                ymin = -Inf, ymax = Inf,
                color = Overtopping_FW)) +
  geom_point(data = ch2, aes(x = Date, y = chloro_a*3750, col = "Chlorophyll")) +
  geom_point(data = ch2, aes(x = Date, y = cch_discharge_daily, col = "Q")) +
  labs(y = expression(paste("Q (", ft^3, " ", s^-1, ")"))) +
  scale_y_continuous(sec.axis = sec_axis(~./3750, name = "Chlorophyll")) +
  scale_x_date(limits = c(as.Date("2015-01-01"), as.Date("2020-01-01"))) +
  scale_color_manual(values = c("forestgreen", "skyblue", "purple")) +
  theme_bw(base_size = 12)

ggplot(ch2, aes(x = cch_discharge_daily, y = chloro_a)) +
  geom_point()


# Read out simple data for quick SAM model
# Retain USGS 36 + CCH daily Q + overtopping at Lisbon weir
save(all, file = "scripts/sam_models/USGS36_all.Rda")
