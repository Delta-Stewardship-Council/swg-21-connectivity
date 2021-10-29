# Explore and match to USGS daily flow data
# Clone swg-21-flow to same folder as swg-21-connectivity

# Match daily real Q (from swg-21-flow) and modeled Q (YOLO from dayflow)
# to gapfill cch


library(readr)

# import data
days <- data.frame(Date = seq(as.Date("2003-01-01"),
                              as.Date("2020-01-01"), by = 1))

rio <- read_csv("../swg-21-flow/data/daily_RIO.csv") %>%
  rename(Date = date)

cch <- read_csv("../swg-21-flow/data/daily_CCH.csv") %>%
  rename(Date = date)

dayflow <- read_csv("data/dayflow-results-1997-2020.csv") %>%
  select(Date, YOLO) %>%
  mutate(Date = as.Date(Date))

allQ <- left_join(days, rio) %>%
  select(-mean_Temp, -mean_stage) %>%
  rename(rio = mean_discharge) %>%
  left_join(cch) %>%
  select(-mean_Temp, -mean_stage) %>%
  rename(cch = mean_discharge) %>%
  left_join(dayflow)

ggplot(allQ, aes(x = Date)) +
  geom_point(aes(y = rio, color = "Rio")) +
  geom_point(aes(y = cch, color = "CCH")) +
  geom_point(aes(y = YOLO, color = "YOLO"))

allQ %>%
  filter(YOLO > 0) %>%
  ggplot(aes(x = YOLO, y = cch)) +
  geom_point()

m1 <- lm(cch ~ YOLO, data = allQ)
summary(m1)

Qpred <- allQ %>%
  mutate(cch_pred = predict(m1, allQ),
         cch_fill = ifelse(!is.na(cch), cch, cch_pred),
         fill = ifelse(is.na(cch), TRUE, FALSE))

ggplot(Qpred, aes(x = cch, y = cch_pred)) +
  geom_point()

ggplot(Qpred, aes(x = Date)) +
  geom_point(aes(y = cch, color = "CCH")) +
  geom_point(aes(y = cch_pred, color = "pred"))

ggplot(Qpred, aes(x = Date)) +
  geom_point(aes(y = cch_fill, color = fill))

#output gapfilled CCH from YOLO dayflow
save(Qpred, file = "scripts/sam_models/Qpred.Rda")
