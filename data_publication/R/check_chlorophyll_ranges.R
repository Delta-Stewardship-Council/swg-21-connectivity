chla <- read_csv(chla_file) %>%
  dplyr::mutate(region = case_when(location == "main_below" ~ "downstream",
                                   location == "main_above" ~ "upstream",
                                   location == "yolo" ~ "yolo",
                                   TRUE~as.character(location))) %>%
  dplyr::filter(region != "off_channel_below") %>%
  dplyr::filter(!is.na(chlorophyll)) %>%
  dplyr::filter(date > as.Date("1999-02-22") & year(date) < 2020) %>%
  dplyr::select(-location)

datasum <- chla %>%
  group_by(date, region) %>%
  summarize(n = n(),
            max = max(chlorophyll),
            min = min(chlorophyll),
            mean = mean(chlorophyll)) %>%
  filter(n>1) %>%
  ungroup() %>%
  mutate(year = year(date),
         diff_10ormore = if_else(max-min > 10, "yes", "no"))

png("data_publication/figures/chla_diffs.png", width = 8, height = 7, units = "in", res = 300)
ggplot(datasum %>% filter(year > 2011)) +
  geom_point(aes(date, mean), size = 2.2) +
  geom_errorbar(aes(x = date, ymin = min, ymax = max, color = diff_10ormore), size = 0.7) +
  facet_wrap(~region, nrow = 3) +
  scale_color_manual(values = c("gray50", "magenta4")) +
  theme_bw() +
  theme(strip.text = element_text(size = 12))
dev.off()

ggplot(chla) + geom_boxplot(aes(region, chlorophyll))
ggplot(chla) + geom_point(aes(date, chlorophyll)) + facet_wrap(~region)
