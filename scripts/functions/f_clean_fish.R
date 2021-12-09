# clean fish data for modeling

library(dplyr)
library(readr)
library(glue)
#library(contentid)
library(janitor)

f_clean_fish <- function(fishdat) {

  # read in raw data with cleaned names, and remove undesired variables
  fishdat_df <- fishdat %>%
    select(-datetime, -sample_id, -tow_direction, -length_na_flag) %>%
    rename(fish_taxa = taxa, fish_length = length) # rename variables in case need to combine with zoops in the future

  # placeholder for now. Hold cleaned dataframe as variable in workspace instead of writing to disk, given large file size
  fishdat_df <- fishdat_df

  return(fishdat)
}
