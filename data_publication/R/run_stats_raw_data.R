# run_stats_raw_data.R ######
# Run stats for raw data flow


alldata <- readr::read_csv(here::here("data_publication/data_clean/model_chla_covars.csv")) %>%
  mutate(station = factor(station),
         inund_factor = factor(inund_factor, levels = c("none", "short", "long")))

flood <- alldata %>% filter(region == "yolo")
upstream <- alldata %>% filter(region == "upstream")
downstream <- alldata %>% filter(region == "downstream")

kruskal.test(log_qsdy~inund_factor, data = alldata)
hist(alldata$log_qsdy)


library(dunn.test)
dunn.test(alldata$Q_sday, alldata$inund_factor, method = "holm", kw = TRUE)
dunn.test(alldata$log_qsdy, alldata$inund_factor, method = "holm", kw = TRUE)

dunn.test(downstream$Q_sday, downstream$inund_factor, method = "holm", kw = TRUE)

dunn.test(upstream$Q_sday, upstream$inund_factor, method = "holm", kw = TRUE)

dunn.test(yolo$Q_sday, yolo$inund_factor, method = "holm", kw = TRUE)

reg <- alldata$Q_sday
