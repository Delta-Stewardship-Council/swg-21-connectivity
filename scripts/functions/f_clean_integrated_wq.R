library(readr)
library(janitor)

f_clean_integrated_wq <- function(){
  source_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.1&entityid=6c5f35b1d316e39c8de0bfadfb3c9692"

  wq <- source_url  %>%
    read_csv(show_col_types = FALSE) %>%
    clean_names()

  # other cleaning steps

  write_csv(wq, "data_clean/clean_integrated_wq.csv")

}
