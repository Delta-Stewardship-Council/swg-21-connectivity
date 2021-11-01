# calculate functional flows for Verona
library(tidyverse)
library(ffcAPIClient)

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", ""))

# Gage
verona <- "11425500"
COMID <- "15039097"



# Predicted Flows ---------------------------------------------------------

# get predicted data:
predicted_flows <- ffcAPIClient::get_predicted_flow_metrics(comid = COMID, online = TRUE)

# return everything
tst <- FFCProcessor$new()
tst$set_up(gage_id = COMID, token = ffctoken, comid=COMID)
tst$flow_field = "Flow"
tst$date_field = "Date"
tst$date_format_string <- "%Y-%m-%d"

# using your own data:
tst$step_one_functional_flow_results(timeseries = g_1, comid=COMID,
                                     token = ffctoken, output_folder = "tst")

# observe percentiles
as_tibble(tst$alteration)
as_tibble(tst$ffc_percentiles)
(tst$ffc_results) %>% View()

# run and get obs percentiles
ffc$gage_start_date = "1979-10-01"

ffc$run()
ffc$ffc_percentiles %>% as.data.frame()
