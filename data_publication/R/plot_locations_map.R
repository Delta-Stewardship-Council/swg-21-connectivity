# Code for making map

# dayflow data inputs are from USGS sites:
# 1 - USGS 11426000 SACRAMENTO WEIR SPILL TO YOLO BYPASS (Latitude 38°36'25", Longitude 121°33'15" NAD27),
# 2 - USGS 11453000 YOLO BYPASS NR WOODLAND CA (Latitude 38°40'40", Longitude 121°38'35" NAD27) and
# 3 - South Fork Putah Creek-Davis (A0-9115/11-4550.50)
# South Putah Creek (but in Winters): Latitude 38°29'34", Longitude 122°00'07" NAD27

library(viridis)
library(sf)
library(dplyr)
library(ggplot2)
library(spData)
library(here)
library(readr)
library(janitor)
library(deltamapr) # devtools::install_github("InteragencyEcologicalProgram/deltamapr")
library(cowplot)
library(ggspatial)
library(tigris)

# Read in data --------------------------------------

## California polygon
data("us_states", package = "spData")
California = filter(us_states, NAME == "California")
st_crs(California)

## delta waterways shapefile
data(WW_Watershed)
WW_Watershed_4269 <- st_transform(WW_Watershed, crs = 4269)

## weirs
weirs <- data.frame(station = c("Sacramento Weir", "Fremont Weir"),
                    region = NA,
                    data_type = "weir",
                    station_name = c("Sacramento Weir", "Fremont Weir"),
                    latitude = c(38.6049049, 38.759444),
                    longitude = c(-121.5566237,-121.666389))
weirs_sf <- st_as_sf(weirs, coords = c("longitude", "latitude"), crs = 4326)
weirs_4269 <- st_transform(weirs_sf, crs = 4269)

## yolo polygon
yolo_sf <- sf::st_read(here("data_raw", "yolo_shapefile", "Flood_Bypasses.shp"))
yolo_4269 <- st_transform(yolo_sf, crs = 4269) %>%
  dplyr::filter(FID == 1)

## regions polygon (created from Rosie's app; edited in ArcMap)
regions_sf <- sf::st_read(here("data_raw", "regions_shapefile", "shapefile_with_ez2_updated.shp"))
regions_4269 <- st_transform(regions_sf, crs = 4269) %>%
  mutate(id = rownames(.),
         region = case_when(id == 1 ~ "downstream",
                            id == 2 ~ "tidal slough complex",
                            id == 3 ~ "floodplain",
                            id == 4 ~ "mainstem",
                            TRUE ~ as.character("NA")))

regionnames <- data.frame(region = c("downstream", "tidal slough complex", "floodplain", "mainstem"))
regions_final <- cbind(regions_4269, regionnames) %>%
  filter(region!="tidal slough complex") %>%
  select(-notes)
regions_buffer <- st_buffer(regions_final, 0.001)

# stations
stations <- read_csv(here::here("data_publication", "data_clean", "stations.csv"))
stations_sf <- st_as_sf(stations, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
stations_sf_4269 <- stations_sf %>% st_transform(crs = st_crs(California)) %>%
  filter(longitude < -121.5)
stations_sf_filt <- stations_sf_4269  %>% filter(station %in% c("RIV"))

stations_weirs_sf_4269 <- bind_rows(stations_sf_4269, weirs_4269)
station_labels <- stations_weirs_sf_4269 %>% select(-data_type) %>% distinct()

# try ne_coastline from rnaturalearth
ca <- states(cb=TRUE, progress_bar = FALSE) %>%
  dplyr::filter(STUSPS %in% c("CA"))

## CA Inset -----------------

insetbbox = st_as_sfc(st_bbox(c(xmin = -122.6, xmax = -121,
                                 ymin = 37.4, ymax = 38.9), crs = 4269))
insetbbox2 = st_as_sfc(st_bbox(stations_sf_4269))

(inset <- ggplot() +
    geom_sf(data = California, fill = "white") +
    geom_sf(data = WW_Delta, colour = "skyblue2", size = 0.3) +
    geom_sf(data = insetbbox, fill = NA, color = "navy", size = 1) +
    geom_sf(data = insetbbox2, fill = NA, color = "indianred4", size = 1) +
    annotate(geom = "text", x = -122, y = 41.5, size = 2.5, label = "California", fontface = "italic") +
    annotate(geom = "text", x = -122.3, y = 39.2, label = "A", size = 2, fontface = "italic") +
    annotate(geom = "text", x = -122.3, y = 38.65, label = "B", size = 2, fontface = "italic") +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))

## Cache inset ---------------

### Crop everything--------
sf::sf_use_s2(FALSE)
WW_Watershed_crop <- st_crop(WW_Watershed_4269,xmin = -122, xmax = -121.5, ymin = 38, ymax = 38.8)
Sloughs <- WW_Watershed_crop %>% filter(HNAME %in% c("Steamboat Slough", "Miner Slough", "Sutter Slough",
                                                     "Elk Slough", "Lindsey Slough", "PUTAH CREEK",
                                                     "SOUTH FORK PUTAH CREEK", "Prospect Slough", "Liberty Cut"))
### Map -------------------

## Delta inset ----------------------
### Make a sacramento river and toe drain highlight ---------------
mapview::mapview(WW_Watershed_crop)
WW_Watershed_crop3 <- st_crop(WW_Watershed_4269,xmin = -124, xmax = -121, ymin = 37, ymax = 38.9)
SacR <- WW_Watershed_crop3 %>% filter(HNAME %in% c("Sacramento River", "SACRAMENTO RIVER") )
ToeD <- WW_Watershed_crop3 %>% filter(HNAME %in% c("Toe Drain"))
SanJ <- WW_Watershed_crop3 %>% filter(HNAME %in% c("San Joaquin River", "SAN JOAQUIN RIVER"))
Putah <- WW_Watershed_crop3 %>% filter(HNAME == "PUTAH CREEK")
Suisun <- WW_Watershed_crop3 %>% filter(HNAME %in% c("SUISUN CUTOFF", "SUISUN BAY"))

sort(unique(WW_Watershed$HNAME))

(delta_map <- ggplot() +
    geom_sf(data = yolo_4269, fill = "#009E73", colour = "#009E73", alpha = 0.1) +
    geom_sf(data = WW_Watershed_crop3, fill = "lightgrey", colour = "lightgrey", alpha = 0.4, inherit.aes = FALSE) +
    geom_sf(data = ca, inherit.aes=FALSE, fill = NA, color = "grey40", alpha = 0.4) +
    geom_sf(data = SacR, colour = "#56B4E9", fill = "#56B4E9", alpha = 0.6, inherit.aes = FALSE)+
    geom_sf(data = SanJ, colour = "#E69F00", fill = "#E69F00", alpha = 0.6, inherit.aes = FALSE)+
    geom_sf(data = ToeD, colour = "#D55E00", fill = "#D55E00", alpha = 0.2, inherit.aes = FALSE)+
    geom_sf(data = Putah, colour = "lightskyblue3", fill = "lightskyblue3", alpha = 0.5, inherit.aes = FALSE)+
    geom_sf(data = weirs_4269, colour = "black", shape = 18, size = 2.4, inherit.aes = FALSE)+
    geom_sf_label(data = weirs_4269, aes(label = station_name),
                  nudge_x = c(0.24, 0.2), #Sac, Fremont
                  nudge_y = c(0.05, 0.06),
                  colour = "black",  size = 4, inherit.aes = FALSE)+
    annotate(geom = "text", x = -121.25, y = 38.5, label = "Sacramento River", fontface = "italic", size = 3.8) +
    annotate(geom = "text", x = -121.7, y = 37.93, label = "San Joaquin River", fontface = "italic", size = 3.8) +
    annotate(geom = "text", x = -122.05, y = 38.17, label = "Suisun Marsh", fontface = "italic", size = 4) +
    annotate(geom = "text", x = -122.04, y = 38.08, label = "Suisun Bay", fontface = "italic", size = 4) +
    annotate(geom = "text", x = -121.79, y = 38.42, label = "Yolo Bypass", fontface = "italic", size = 4) +
    annotate(geom = "text", x = -122.2, y = 37.7, label = "San Francisco Bay", fontface = "italic", size = 4) +
    annotate(geom = "text", x = -121.6, y = 38.39, angle = 72, size = 3, label = "Toe Drain", fontface = "italic") +
    annotate(geom = "text", x = -121.83, y = 38.592, label = "Putah Creek", fontface = "italic", size = 3.8) +
    annotate("segment", x = -121.666389, xend = -121.666389, y = 38.75, yend = 38.71,
             arrow = arrow(ends = "last", type = "closed", length = unit(0.25, "cm")),alpha = 0.8,  color = "#009E73") +
    annotate("segment", x = -121.5566237, xend = -121.62, y = 38.6049049, yend = 38.58,
             arrow = arrow(ends = "last", type = "closed", length = unit(0.25, "cm")),alpha = 0.8,  color = "#009E73") +
    annotate("segment", x = -121.66, xend = -121.66, y = 38.3, yend = 38.2,
             arrow = arrow(ends = "last", type = "closed", length = unit(0.25, "cm")),alpha = 0.8,  color = "#0072B2") +
    annotate("segment", x = -121.58, xend = -121.6, y = 38.19, yend = 38.16,
             arrow = arrow(ends = "last", type = "closed", length = unit(0.25, "cm")),alpha = 0.8,  color = "#0072B2") +
    annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = unit(.005, "in"), pad_y = unit(0.15, "in"),
                           style = north_arrow_fancy_orienteering) +
    annotation_scale(location = "bl", bar_cols = c("darkgray", "white", "darkgray", "white"), text_cex = 1.1)+
    coord_sf(xlim = c(-122.5, -121),
                ylim= c(37.5, 38.9))+
    scale_x_continuous(breaks = seq(-122.5, -121, by = 0.5)) +
  labs(title = "A) San Francisco Estuary")+
  theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 13) ,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "navy", linewidth = 1.5)))
delta_map
## Main map --------------------


### Make map --------------
pal_vals = c("#CC79A7","#D55E00", "#0072B2" )

(map_stations <- ggplot() +
   geom_sf(data = yolo_4269, colour = "grey36", alpha = 0.3) +
   geom_sf(data = WW_Watershed_crop,  colour = "lightgrey", alpha = 0.35, inherit.aes = FALSE) +
   # geom_sf(data = Sloughs, fill = "grey36", colour = "grey36", alpha = 0.5, inherit.aes = FALSE) +
   #geom_sf(data = regions_buffer, alpha = 0.2, color = "steelblue2") +
   #geom_sf(data = regions_final, fill = "steelblue1", color = NA,size = 1,alpha = 0.2) +
   geom_sf(data = regions_buffer, aes(fill = region, color = region), alpha = 0.2, inherit.aes = FALSE) +
   geom_sf(data = regions_final, aes(fill = region), color = NA,size = 1,alpha = 0.2, inherit.aes = FALSE) +
   geom_sf(data = stations_sf_4269 %>% filter(data_type != "srad"), aes(shape = data_type, size = data_type, color = region, fill = region),alpha = 0.85, inherit.aes = FALSE) +
   geom_sf_label(data = stations_sf_filt , aes(label = station_name ), hjust = 1, vjust = -0.3, inherit.aes = FALSE) +
   annotation_scale(location = "bl", bar_cols = c("darkgray", "white", "darkgray", "white"), text_cex = 1.1)+
   annotate(geom = "text", x = -121.59, y = 38.82, label = "Mainstem (NC)", fontface = "italic") +
   annotate(geom = "text", x = -121.82, y = 38.1, label = "Downstream\n(C)", fontface = "italic") +
   annotate(geom = "text", x = -121.74, y = 38.4, label = "Floodplain\n(C*)", fontface = "italic") +
   labs(title = "B) Study Regions and Stations") +
   scale_shape_manual(values = c(8, 17, 16)) +
   scale_size_manual(values = c(4, 3, 3)) +
   scale_fill_manual(values = c("gray40", "#009E73", "#56B4E9"))+
   scale_colour_manual(values = c("gray40", "#009E73", "#56B4E9"))+
   # scale_fill_manual(values = viridis(7, option = "mako")[c(2,4,6)])+
   # scale_colour_manual(values = viridis(9, option = "turbo")[c(8,1,9)])+
   theme_bw() +
   theme(panel.border=element_rect(colour="indianred4", linewidth = 1.5))+
   theme(axis.title = element_blank(),
         axis.text = element_text(size = 16),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.ticks = element_blank(),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         legend.text = element_text(size = 10),
         legend.title = element_blank(),
         legend.position = c(0.25, .82),
         legend.box.background = element_rect(colour = "black"),
         legend.background = element_blank(),
         plot.title = element_text(margin=margin(0,0,-75,0)),
         legend.margin = margin(0, 0.1, 0.1, 0.1, "cm")))

## California inset--------------------------------------------------
(gg_inset_map = ggdraw() +
   draw_plot(delta_map) +
   draw_plot(inset, x = 0.14, y = 0.53, width = 0.29, height = 0.4))


## Combine with patchwork -------------------------------------


library(patchwork)
# Most current
patchmap <- gg_inset_map +
  plot_spacer() +
  map_stations  + plot_layout(widths = c(4.1, 0.02, 2.2), heights = 7.2)
patchmap
ggsave(here("data_publication/figures/manuscript_map_test.png"), width = 8.5, height = 7.5, units = "in", device = 'png', dpi = 300)

## Save map png--------------------------------------------

# Only
map_stations
ggsave("figures/manuscript_map_regions_only.png", width = 6, height = 6, units = "in", device = 'png', dpi = 300)
patchmap
ggsave("figures/manuscript_map.png", width = 8, height = 11, units = "in", device = 'png', dpi = 300)

# interactive map -----
chl <- stations_sf_4269 %>% filter(data_type =="chl & nut")

mapview::mapview(chl, zcol = "region")


















