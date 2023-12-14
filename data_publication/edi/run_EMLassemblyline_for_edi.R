# This script executes an EMLassemblyline workflow.

# Initialize workspace --------------------------------------------------------

# Update EMLassemblyline and load

#remotes::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

# Define paths for your metadata templates, data, and EML
path_templates <- "data_publication/edi/metadata_templates"
path_data <- "data_publication/edi/data_objects"
path_eml <- "data_publication/edi/eml"

# Create metadata templates ---------------------------------------------------

# Below is a list of boiler plate function calls for creating metadata templates.
# They are meant to be a reminder and save you a little time. Remove the
# functions and arguments you don't need AND ... don't forget to read the docs!
# E.g. ?template_core_metadata

# Create core templates (required for all data packages)

EMLassemblyline::template_core_metadata(
  path = path_templates,
  license = "CCBY",
  file.type = ".md")

# Create table attributes template (required when data tables are present)

EMLassemblyline::template_table_attributes(
  path = path_templates,
  data.path = path_data,
  data.table = c("model_chla_covars.csv",
                 "stations.csv"))
# Look at standard units in the package. Need to create custom units if they aren't in here.
# Run view_unit_dictionary() to look at the units EML has standard. See custom units example for filling out custom units.

# Create categorical variables template (required when attributes templates
# contains variables with a "categorical" class)

EMLassemblyline::template_categorical_variables(
  path = path_templates,
  data.path = path_data)

# Create geographic coverage (required when more than one geographic location
# is to be reported in the metadata).

EMLassemblyline::template_geographic_coverage(
  path = path_templates,
  data.path = path_data,
  data.table = "stations.csv",
  lat.col = "latitude",
  lon.col = "longitude",
  site.col = "station")

# Create taxonomic coverage template (Not-required. Use this to report
# taxonomic entities in the metadata)

# no taxonomy -----------------------------------
# remotes::install_github("EDIorg/taxonomyCleanr")
# library(taxonomyCleanr)

# taxonomyCleanr::view_taxa_authorities()

# EMLassemblyline::template_taxonomic_coverage(
#   path = path_templates,
#   data.path = path_data,
#   taxa.table = "",
#   taxa.col = "",
#   taxa.name.type = "",
#   taxa.authority = 3)

# Add provenance information
template_provenance(
  path = path_templates
)

# Make EML from metadata templates --------------------------------------------

# Once all your metadata templates are complete call this function to create
# the EML.Removed the geographic options because using the geographic template above.

EMLassemblyline::make_eml(
  path = path_templates,
  data.path = path_data,
  eml.path = path_eml,
  dataset.title = "Discrete water temperature, flow, solar radiation, chlorophyll-a and inundation, Sacramento-San Joaquin Delta, CA, 1999-2019",
  temporal.coverage = c("1999-03-09", "2019-12-27"),
  #geographic,coordinates
  #geographic.description
  maintenance.description = "not updated, associated with completed study",
  data.table = c("model_chla_covars.csv",
                 "stations.csv"),
  data.table.name = c("Chlorophyll and Covariate Data", "Station locations"),
  data.table.description = c("Chlorophyll, water temperature, solar radiation, and inundation data across regions", "Station locations and metadata"),
  data.table.quote.character = c('"', '"'), # If you have columns that have commas in the text, you will need to use "quote = TRUE" when you write your R file (write.csv), and then use this to tell make_eml what is going around your character cells. c(apostrophe, quote, apostrophe, comma, etc...)
  other.entity = c("Metadata_Connectivity_v1.1.pdf"),
  other.entity.name = c("Metadata for Connectivity Study"),
  other.entity.description = c("Metadata for Connectivity Study"),
  user.id = "catarinapien",
  user.domain = "EDI",
  package.id = "edi.1537.4")
