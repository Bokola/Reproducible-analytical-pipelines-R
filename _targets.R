library(targets)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(data_summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts

# data() returns a promise, to use it in target we need to define a pure function

read_data <- function(data_name, package_name){
  temp <- new.env(parent = emptyenv())
  
  data(list = data_name,
       package = package_name,
       envir = temp)
  # get searches an object by  name and returns it
  get(data_name, envir = temp)
}

# Set target-specific options such as packages:

tar_option_set(packages = c("rapR")) # nolint

# End this file with a list of target objects.
list(
  tar_target(
    commune_level_data,
    read_data("commune_level_data", "rapR")
  )
  ,tar_target(
    country_level_data, 
    read_data("country_level_data", "rapR")
  )
  ,tar_target(
    commune_data,
    get_laspeyeres(commune_level_data)
  )
  ,tar_target(
    country_data,
    get_laspeyeres(country_level_data)
  )
  ,tar_target(
    communes,
    c(
      "Luxembourg",
      "Esch-sur-Alzette",
      "Mamer",
      "Schengen",
      "Wincrange"
    )
  ),
  tar_render(
    analyse_data,
    "analyse_data.Rmd"
  )
)
