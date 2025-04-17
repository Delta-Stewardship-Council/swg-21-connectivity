library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(viridis)

alldata <- readr::read_csv(here::here("data_publication/data_clean/model_chla_covars.csv")) %>%
  mutate(station = factor(station),
         inund_factor = factor(inund_factor, levels = c("none", "short", "long")),
         year = year(date),
         year = factor(year),
         water_year = factor(water_year),
         month = factor(month, levels = c(12, 1, 2, 3, 4, 5)))

alldata_summary <- alldata %>%
  group_by(year, month) %>%
  summarize(n = n()) %>%
  ungroup()

(plot_samples <- ggplot(alldata_summary) + geom_tile(aes(x = year, month, fill = n)) +
  scale_fill_viridis(option = "plasma") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(angle = 90))
)

png(here::here("data_publication", "figures","plot_sample_numbers.png"), width = 6, height = 5, units = "in", res = 300)
plot_samples
dev.off()

unique(alldata$station)
