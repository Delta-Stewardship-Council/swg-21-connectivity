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
library(deltamapr)
library(cowplot)
library(ggspatial)

# Read in data --------------------------------------

  ## chlorophyll, nuts stations
  data_id <- contentid::store(here("data_model", "model_chla_covars_gam.csv") )
  data_file <- contentid::resolve(data_id)
  model_data <- readr::read_csv(data_file)

  # water temp, flow stations
  wt_stations_id <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.591.2&entityid=a059f5eea4f8500fe1a43566451ec593")
  wt_stations_file <- contentid::resolve(wt_stations_id)
  wt_stations <- readr::read_csv(wt_stations_file)

  # flow : Verona: 38.774444 -121.597222 NAD 27 = 38.7743454,-121.5982925 WGS 1984
  flow_stations <- data.frame(latitude = 38.7743454, longitude = -121.5982925, station = "USGS_11455420", region = "above", data_type = "flow")

  ## California polygon
  data("us_states", package = "spData")
  California = filter(us_states, NAME == "California")
  st_crs(California)

  ## delta waterways shapefile
  data(WW_Watershed)
  WW_Watershed_4269 <- st_transform(WW_Watershed, crs = 4269)

  ## weirs
  weirs <- data.frame(station = c("Sacramento Weir", "Fremont Weir"),
                      latitude = c(38.6049049, 38.759444),
                      longitude = c(-121.5566237,-121.666389))
  weirs_sf <- st_as_sf(weirs, coords = c("longitude", "latitude"), crs = 4326)
  weirs_4269 <- st_transform(weirs_sf, crs = 4269)

  ## yolo polygon
  yolo_sf <- sf::st_read(here("data_raw", "yolo_shapefile", "Flood_Bypasses.shp"))
  yolo_4269 <- st_transform(yolo_sf, crs = 4269) %>%
    dplyr::filter(FID == 1)

  ## regions polygon (created from Rosie's app; edited in ArcMap)
  regions_sf <- sf::st_read(here("data_raw", "regions_shapefile", "shpExport.shp"))
  regions_4269 <- st_transform(regions_sf, crs = 4269)

  regionnames <- data.frame(region = c("mainsteam river downstream", "tidal slough complex", "floodplain bypass", "mainstem river upstream"))
  regions_final <- cbind(regions_4269, regionnames) %>%
    select(-notes)

  ## regions buffer
  regions_buffer <- st_buffer(regions_final, 0.01)
  plot(regions_buffer)

# Convert to stations data frames to bind -----------------------------
wt_stations_filt <- wt_stations %>%
  janitor::clean_names() %>%
  dplyr::filter(station %in% c("LIB", "RIV", "SRV")) %>%
  dplyr::select(latitude, longitude, station, station_name) %>%
  dplyr::mutate(region = case_when(station == "LIB" ~ "cache",
                                   station %in% c("RIV", "SRV") ~ "below"),
                data_type = "")

stations_chl <- model_data %>%
  dplyr::select(latitude, longitude, station = station_wq_chl, region) %>%
  unique() %>%
  mutate(data_type = "chl & nut")

#South Putah Creek (but in Winters): Latitude 38°29'34", Longitude 122°00'07" NAD27
dayflow <- data.frame(latitude = c(38.6069444444, 38.6777777778, 38.49278),
                      longitude = c(-121.5541666667,-121.6430555556, -122.0019),
                      station = c("USGS_11426000", "USGS_11453000", "USGS_11454210"),
                      region = "yolo",
                      data_type = "flow")

# Assign data types -------------------------------------------------
stations_all <- stations_chl %>%
  bind_rows(wt_stations_filt) %>%
  bind_rows(flow_stations) %>%
  bind_rows(dayflow) %>%
  mutate(data_type = case_when(station == "STTD" ~ "chl & nut,wtemp,sradiation",
                               station == "LIS" ~ "chl & nut,wtemp",
                               station == "SHR" ~ "chl & nut,wtemp,sradiation",
                               station == "Pro" ~ "chl & nut,sradiation",
                               station == "RIV" ~ "wtemp",
                               station == "LIB" ~ "wtemp,flow",
                               station == "SRV" ~ "flow",
                               station == "USGS_11425500" ~ "flow",
                               station == "657" ~ "sradiation,flow",
                               TRUE ~ data_type),
         station_name = case_when(station == "34" ~ "",
                                  station == "653" ~ "",
                                  station == "657" ~ "",
                                  station == "D22" ~"",
                                  #station == "LIB" ~"Liberty at Approx Centr S End",
                                  station == "LIS" ~"Toe Drain at Lisbon Weir",
                                  station == "NZ068" ~"",
                                  station == "Pro" ~"",
                                  #station == "RIV" ~,
                                  station == "SHR" ~ "Sacramento River at Sherwood Harbor",
                                  station == "SRV" ~ "Sacramento River at Vieira's Marina",
                                  station == "STTD" ~ "Screw Trap in Toe Drain",
                                  station == "USGS-11447650" ~"",
                                  station == "USGS-11455139" ~"",
                                  station == "USGS-11455140" ~"",
                                  station == "USGS-11455143" ~"",
                                  station == "USGS-11455146" ~"",
                                  station == "USGS-11455315" ~"",
                                  station == "USGS-11455350" ~"",
                                  station == "USGS-11455385" ~"",
                                  station == "USGS-11455478" ~"",
                                  station == "USGS-382006121401601" ~"",
                                  station == "USGS_11426000" ~"",
                                  station == "USGS_11453000" ~"",
                                  station == "USGS_11455420" ~"",

                                  TRUE ~ station_name))

# Export for fill-in
#write_csv(stations_all, "station_names.csv")

# Change region names
stations_all_reg <- stations_all %>%
  mutate(region = case_when(region == "yolo" ~ "floodplain bypass",
                            region == "upstream" ~ "mainstem river upstream",
                            region == "downstream" ~ "mainstem river downstream",
                            region == "cache" ~ "tidal slough complex"))

# Separate out each different data type
stations_mult <- stations_all %>%
  tidyr::separate_rows(data_type, sep = ",")


# Convert to sf ---------------------------------------------
stations_sf <- st_as_sf(stations_mult, coords = c("longitude", "latitude"), crs = 4326)
stations_sf_4269 <- st_transform(stations_sf, crs = st_crs(California))
station_labels <- stations_sf_4269 %>% select(-data_type) %>% distinct()


# Save shapefile - used this to create regions shapefile
#write_rds(stations_sf, file = "data_raw/stations_in_dataset.rds")


# Mapview

mapview::mapview(WW_Watershed)



# Make maps -----------------------------------------------



    ## CA Inset -----------------
    insetbbox0 = st_as_sfc(st_bbox(stations_sf_4269))
    insetbbox = st_buffer(insetbbox0, dist = 0)

    (inset <- ggplot() +
      geom_sf(data = California, fill = "white") +
      geom_sf(data = WW_Delta, colour = "steelblue4", size = 0.3) +
        geom_sf(data = insetbbox0, fill = NA, color = "red", size = 1) +

      theme_bw() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()))
      #theme_void()


    ## Cache inset ---------------

      ### Crop everything--------
sf::sf_use_s2(FALSE)
WW_Watershed_crop <- st_crop(WW_Watershed_4269,xmin = -122, xmax = -121.5, ymin = 38, ymax = 38.8)
cacheyoloinset <-  st_as_sfc(st_bbox(filter(stations_sf_4269, station == "STTD" | station == "RIV")))
    cachebbox <- st_buffer(cacheyoloinset, dist = c(0.06, 0.02, 0.01, 0.02))
    stations_crop <- st_crop(stations_sf_4269, cachebbox)
    regions_crop <- st_crop(regions_final, cachebbox)
    stnlabel_crop <- st_crop(station_labels, cachebbox)
    WW_Watershed_crop2 <- st_crop(WW_Watershed_4269, cachebbox)


Sloughs <- WW_Watershed_crop %>% filter(HNAME %in% c("Steamboat Slough", "Miner Slough", "Sutter Slough",
                                                         "Elk Slough", "Lindsey Slough", "PUTAH CREEK",
                                                         "SOUTH FORK PUTAH CREEK"))
Sloughs_crop <- st_crop(Sloughs, cachebbox)

      ### Map -------------------
    (cacheinset <- ggplot() +
        geom_sf(data = WW_Watershed_crop2,  colour = "darkgrey", alpha = 0.3, inherit.aes = FALSE) +
       geom_sf(data = Sloughs_crop, fill = "grey36", colour = "grey36", alpha = 0.5, inherit.aes = FALSE) +
       geom_sf(data = regions_crop, aes(fill = region, color = region, linetype = region), size = 1, alpha = 0.5) +
      geom_sf(data = stations_crop, aes(shape = data_type), size = 2, alpha = 0.8, inherit.aes = FALSE) +

      #  geom_sf_label(data = stnlabel_crop, aes(x = , y = , label = station), nudge_x = 0.03, inherit.aes = FALSE,) +
      #geom_sf(data = cachebbox, fill = NA, color = "red", size = 1.1) +
       scale_fill_manual(values = viridis(6, option = "mako")[2:5])+
       scale_colour_manual(values = viridis(5, option = "mako")[2:5])+
       scale_linetype_manual(values = c(5, 1, 2, 3)) +
        scale_shape_manual(values = c(8, 6, 16, 0)) +
      theme_bw() +
      theme(axis.text = element_blank(),
            #axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none"))

  ## Delta inset ----------------------
  ### Make a sacramento river and toe drain highlight ---------------
WW_Watershed_crop3 <- st_crop(WW_Watershed_4269,xmin = -123, xmax = -121, ymin = 37, ymax = 38.9)
    SacR <- WW_Watershed_crop3 %>% filter(HNAME %in% c("Sacramento River", "SACRAMENTO RIVER") )
    ToeD <- WW_Watershed_crop3 %>% filter(HNAME %in% c("Toe Drain"))
    SanJ <- WW_Watershed_crop3 %>% filter(HNAME %in% c("San Joaquin River", "SAN JOAQUIN RIVER"))
    Suisun <- WW_Watershed_crop3 %>% filter(HNAME %in% c("SUISUN CUTOFF", "SUISUN BAY"))

sort(unique(WW_Watershed$HNAME))

    (delta_map <- ggplot() +
        geom_sf(data = yolo_4269, fill = "grey36", colour = "grey36", alpha = 0.3) +
        geom_sf(data = WW_Watershed_crop3, fill = "lightgrey", colour = "lightgrey", alpha = 0.35, inherit.aes = FALSE) +
        geom_sf(data = SacR, colour = "steelblue", fill = "steelblue", alpha = 0.6, inherit.aes = FALSE)+
        geom_sf(data = SanJ, colour = "orange", fill = "orange", alpha = 0.6, inherit.aes = FALSE)+
        geom_sf(data = Suisun, colour = "red", fill = "red", alpha = 0.6, inherit.aes = FALSE)+
        geom_sf(data = ToeD, colour = "indianred4", fill = "lightsalmon2", alpha = 0.2, inherit.aes = FALSE)+
        annotation_north_arrow(location = "tr", which_north = "true",
                               pad_x = unit(.005, "in"), pad_y = unit(0.15, "in"),
                               style = north_arrow_fancy_orienteering) +
        annotation_scale(location = "bl", bar_cols = c("darkgray", "white", "darkgray", "white"), text_cex = 1.1)+
        #annotate(geom = "text", x = -121.76, y = 38.8, label = "upstream", fontface = "italic") +
        #annotate(geom = "text", x = -121.84, y = 38.15, label = "downstream", fontface = "italic") +
        #scale_colour_viridis(discrete = TRUE, option = "plasma") +
        #scale_shape_manual(values = c(8, 6, 16, 0)) +
        #scale_linetype_manual(values = c(5, 1, 2, 3)) +
        #scale_colour_manual(values = c("#00AFA1", "#00AEDB", "navy","palegreen2"))+
        #scale_fill_manual(values = c("#00AFA1", "#00AEDB", "lightslateblue","palegreen2"))+
        #scale_fill_manual(values = viridis(6, option = "mako")[2:5])+
        #scale_colour_manual(values = viridis(5, option = "mako")[2:5])+
        #scale_fill_manual(values = c("#00AFA1", "#00AEDB", "lightslateblue","palegreen2"))+
        #scale_fill_viridis(discrete = TRUE, option = "mako", direction = -1) +
        #scale_colour_viridis(discrete = TRUE, option = "mako", direction = -1) +
        theme_bw() +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank()))

  ## Main map --------------------


    ### Make map --------------

    (map_stations <- ggplot() +
      geom_sf(data = yolo_4269, fill = "grey36", colour = "grey36", alpha = 0.3) +
      geom_sf(data = WW_Watershed_crop, fill = "lightgrey", colour = "lightgrey", alpha = 0.35, inherit.aes = FALSE) +
      geom_sf(data = Sloughs, fill = "grey36", colour = "grey36", alpha = 0.5, inherit.aes = FALSE) +
      geom_sf(data = regions_buffer, aes(fill = region), size = 0.9,alpha = 0.2, color = "transparent") +
      geom_sf(data = regions_final, aes(fill = region, color = region, linetype = region), size = 0.9,alpha = 0.5) +
      geom_sf(data = weirs_4269, colour = "red", shape = 18, size = 2.5, inherit.aes = FALSE)+
      geom_sf(data = stations_sf_4269, aes(shape = data_type), size = 2, alpha = 0.8, inherit.aes = FALSE) +
      annotation_north_arrow(location = "tr", which_north = "true",
                             pad_x = unit(.005, "in"), pad_y = unit(0.15, "in"),
                             style = north_arrow_fancy_orienteering) +
      annotation_scale(location = "bl", bar_cols = c("darkgray", "white", "darkgray", "white"), text_cex = 1.1)+
      annotate(geom = "text", x = -121.76, y = 38.8, label = "upstream", fontface = "italic") +
      annotate(geom = "text", x = -121.84, y = 38.15, label = "downstream", fontface = "italic") +
      annotate(geom = "text", x = -121.74, y = 38.6, label = "yolo", fontface = "italic") +
      #scale_colour_viridis(discrete = TRUE, option = "plasma") +
      scale_shape_manual(values = c(8, 6, 16, 0)) +
      scale_linetype_manual(values = c(5, 1, 2, 3)) +
      #scale_colour_manual(values = c("#00AFA1", "#00AEDB", "navy","palegreen2"))+
      #scale_fill_manual(values = c("#00AFA1", "#00AEDB", "lightslateblue","palegreen2"))+
      scale_fill_manual(values = viridis(7, option = "mako")[c(2,3,5,7)])+
      scale_colour_manual(values = viridis(5, option = "mako")[2:5])+
      #scale_fill_manual(values = c("#00AFA1", "#00AEDB", "lightslateblue","palegreen2"))+
      #scale_fill_viridis(discrete = TRUE, option = "mako", direction = -1) +
      #scale_colour_viridis(discrete = TRUE, option = "mako", direction = -1) +
      theme_bw() +
      theme(axis.title = element_blank(),
            axis.text = element_text(size = 16),
            axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5, size = 10),
            axis.text.y = element_text(size = 10),
            #legend.position = "top",
            #legend.box = "vertical",
            legend.text = element_text(size = 10),
            legend.title = element_blank(),
            #legend.position = c(0.84, .65),
            legend.margin = margin(0, 0.1, 0.1, 0.1, "cm") ))



    ## California inset--------------------------------------------------
    (gg_inset_map = ggdraw() +
      draw_plot(delta_map) +
      draw_plot(inset, x = 0.07, y = 0.38, width = 0.4, height = 0.45))


     ## Combine with patchwork -------------------------------------

    library(patchwork)
    patchmap <- (map_stations | (inset/cacheinset)) + plot_layout(guides = 'collect')

    patchmap

    patchmap2 <- (map_stations | gg_inset_map) + plot_layout(widths = c(2, 3.42), guides = 'collect')
    patchmap2



    ## Save map png--------------------------------------------
    patchmap2
    ggsave("figures/manuscript_map2.png", width = 8, height = 10, units = "in", device = 'png', dpi = 300)

    # Only
    map_stations
    ggsave("figures/manuscript_map_regions_only.png", width = 6, height = 6, units = "in", device = 'png', dpi = 300)


    patchmap
    ggsave("figures/manuscript_map.png", width = 8, height = 11, units = "in", device = 'png', dpi = 300)




    gg_inset_map
    ggsave("figures/manuscript_map.png", width = 5.5, height = 9, units = "in", device = 'png', dpi = 300)




