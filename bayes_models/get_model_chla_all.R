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
           crs = 4326, remove = FALSE)
mapviewOptions(fgb=TRUE)
mapview(delta_map, zcol = "Source", burst = TRUE)

# Select sites near Rio vista bridge
int <- delta %>%
  filter(Station %in% c("EMP C10", "EMP C10A",
                        "USGS 36", "USGS 657", "USGS 659", "USGS 662",
                        "USBR 16", "USBR 44", "USBR 44", "USBR 34")) %>%
  select(Station, Date, Chlorophyll)

# Read in USGS CAWSC data
usgs <- read_csv("data/USGS_CAWSC_discrete_connectivity_clean.csv") %>%
  filter(site_no %in% c(11455350, 11455385, 11455315, 11455420)) %>%
  select(site_no, sample_dt, contains("result_va_Chla")) %>%
  filter(!is.na(result_va_Chla_ugL) == TRUE)

# Read in YOLO inundation and verona flow
flo <- read_csv("data/yolo_data_for_model.csv") %>%
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
  drop_na()

range(total$date) # n = 1041, 1999-01-05 to 2020-07-23
range(flo$Date) # 1996-10-01 to 2020-09-30 for verona
range(flo$Date[complete.cases(flo)]) # 1998-12-10 to 2020-09-30 for climate variables, but perhaps some NA


# Visualize
ggplot()+
  geom_rect(data = total %>% filter(inun == 1),
            aes(xmin = date, xmax = date,
                ymin = -Inf, ymax = Inf,
                color = factor(inun))) +
  geom_point(data = total, aes(x = date, y = chl*300, col = "Chlorophyll")) +
  geom_point(data = total, aes(x = date, y = Flow_usgs_verona, col = "Q")) +
  labs(y = expression(paste("Q (", ft^3, " ", s^-1, ")"))) +
  scale_y_continuous(sec.axis = sec_axis(~./300,
                                         name = expression(paste("Chlorophyll", mu, "g/mL")))) +
  scale_x_date(limits = c(as.Date("1999-01-01"), as.Date("2021-01-01"))) +
  scale_color_manual(values = c("pink", "forestgreen", "darkblue")) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank()) +
  guides(color = "none")


save(total, file = "scripts/sam_models/Chla_all.Rda")
