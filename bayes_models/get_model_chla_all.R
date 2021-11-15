# get chla_all

# Switch to chlorophyll data
# Combine different sources, match to inundation and Q variables

library(readr)
library(dplyr)
library(ggplot2)

library(sf)
library(mapview)

# Read in integrated data
delta <- read_csv("data/Delta_Integrated_WQ.csv.zip") %>%
  filter(!is.na(Chlorophyll))

table(delta$Station)

# make map
delta_map <- delta %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326, remove = FALSE) %>%
  distinct(Station, Latitude, Longitude, .keep_all = TRUE)
mapviewOptions(fgb=FALSE)
mapview(delta_map, zcol = "Source", burst = TRUE)

# Select sites near Rio vista bridge
int <- delta %>%
  filter(Station %in% c("EMP C10", "EMP C10A",
                        "USGS 36", "USGS 657", "USGS 659", "USGS 662",
                        "USBR 16", "USBR 44", "USBR 44", "USBR 34")) %>%
  select(Station, Date, Chlorophyll)

# Read in USGS CAWSC data
usgs <- read_csv("data/USGS_CAWSC_discrete_clean.csv") %>%
  filter(site_no %in% c(11455350, 11455385, 11455315, 11455420)) %>%
  select(site_no, sample_dt, contains("result_va_Chla")) %>%
  filter(!is.na(result_va_Chla_ugL) == TRUE)

# Read in YOLO inundation and verona flow
flo <- read_csv("data/yolo_daymet_for_model.csv") %>%
  mutate(inun = ifelse(Topped.days == 0, 0, 1))
# Calculate # of topped days in past 30 days
flo$past_topped <- NA
for(i in 31:nrow(flo)) {
  flo$past_topped[i] <- sum(flo$inun[(i-31):(i-1)])
}

# Stack the two data sources
total <- data.frame(station = c(int$Station, usgs$site_no),
                    date = c(int$Date, usgs$sample_dt),
                    chl = c(int$Chlorophyll, usgs$result_va_Chla_ugL)) %>%
  left_join(flo, by = c("date" = "Date")) %>%
  tidyr::drop_na()

range(total$date) # n = 1076, 1998-02-11 to 2020-08-17
range(flo$Date) # 1998-01-01 to 2020-12-31 for verona
range(flo$Date[complete.cases(flo)]) # 1998-01-31 to 2020-09-30 for climate variables, but perhaps some NA

table(total$station) # includes C10, C10A, and USGS 36 in south bay

# check data locations:
delta_map %>% filter(Station %in% total$station) %>%
  mapview(zcol="Station")

# filter out stations not in area of interest
total_filt <- total %>%
  filter(!station %in% c("EMP C10", "EMP C10A", "USGS 36"))
table(total_filt$station)

# USGS stations
# cache slough: 11455315
# cache slough at Ryer: 11455350
# cache slough above Ryer Near Rio Vista: 11455385
# sac at Rio Vista: 11455420
# get USGS data locations
library(dataRetrieval)
usgs_stations <- c("11455315", "11455350",
                   "11455385", "11455420")
usgs_loc <- purrr::map_df(usgs_stations, ~dataRetrieval::findNLDI(nwis=.x))
usgs_loc <- usgs_loc %>%
  rename(Station = identifier, Longitude=X, Latitude=Y) %>%
  select(Station, Longitude, Latitude, geometry)

# combine with filtered stations
chla_map <- delta_map %>% filter(Station %in% total_filt$station) %>%
  select(Station, Latitude, Longitude, geometry) %>%
  bind_rows(., usgs_loc)

# map to check: good!
chla_map %>%
  mapview(zcol="Station")


# Visualize
ggplot()+
  geom_rect(data = total_filt %>% filter(inun == 1),
            aes(xmin = date, xmax = date,
                ymin = -Inf, ymax = Inf,
                color = factor(inun))) +
  geom_point(data = total_filt, aes(x = date, y = chl*300, col = "Chlorophyll")) +
  geom_point(data = total_filt, aes(x = date, y = Flow_usgs_verona, col = "Q")) +
  labs(y = expression(paste("Q (", ft^3, " ", s^-1, ")"))) +
  scale_y_continuous(sec.axis = sec_axis(~./300,
                                         name = expression(paste("Chlorophyll", mu, "g/mL")))) +
  scale_x_date(limits = c(as.Date("1999-01-01"), as.Date("2021-01-01"))) +
  scale_color_manual(values = c("pink", "forestgreen", "darkblue")) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank()) +
  guides(color = "none")


# write out as rds
write_rds(total_filt, file = "bayes_models/Chla_all.rds")
