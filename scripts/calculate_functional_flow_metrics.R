# calculate functional flows for Verona
library(tidyverse)
library(ffcAPIClient)

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", ""))
## ffc
ffcAPIClient::clean_account(token = ffctoken)

# Gage
verona <- "11425500"
COMID <- "15039097"

# Predicted Flows ---------------------------------------------------------

# get predicted data for specific water year types
predicted_flows <- ffcAPIClient::get_predicted_flow_metrics(comid = COMID, online = TRUE, wyt = "any")
# get predicted data for all water year types
predicted_flows <- ffcAPIClient::get_predicted_flow_metrics(comid = COMID, online = TRUE, wyt = "all")

# return everything
ffm <- FFCProcessor$new()
ffm$set_up(gage_id = verona, token = ffctoken, comid=COMID)
ffm$run()


# view observed percentiles
ffc_percentiles <- ffm$ffc_percentiles %>% as.data.frame()

# view calculated metrics from observed data (raw metrics by year)
ffc_results <- ffm$ffc_results %>% as.data.frame()

# view predicted percentiles
ffc_pred_percentiles <- ffm$predicted_percentiles %>% as.data.frame()
