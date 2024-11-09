if(!require(glmsummary)) devtools::install_github("Bokola/glmsummary")
pks <- c(
  "tidyverse"
  ,"readxl"
  ,"janitor"
  ,"here"
)
install_load_packages(pks)

d_file <- list.files(
  file.path(here(), "data")
  ,full.names = T
)

sheets <- excel_sheets(d_file)
# read & clean data object
read_clean <- function(..., sheet){
  read_excel(..., sheet = sheet) %>% mutate(year = sheet)
}

raw_data <- map(
  sheets
  ,~read_clean(
    d_file
    ,skip = 10
    ,sheet = .
  )
) |> bind_rows() |> clean_names()

raw_data <- raw_data |>
  rename(
    locality = commune,
    n_offers = nombre_doffres,
    average_price_nominal_euros = prix_moyen_annonce_en_courant,
    average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant,
    average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant
  ) |>
  mutate(locality = str_trim(locality)) |>
  select(year, locality, n_offers, starts_with("average"))
# clean

raw_data <- raw_data |>
  mutate(
    locality = ifelse(grepl("Luxembourg-Ville", locality),
                      "Luxembourg",
                      locality),
    locality = ifelse(grepl("P.tange", locality),
                      "Pétange",
                      locality)
  ) |>
  mutate(across(starts_with("average"),
                as.numeric))

raw_data <- raw_data |>
  filter(!grepl("Source", locality))

# keep communes in our data
raw_data <- raw_data |>
  filter(!grepl("Source", locality))
# create national data
country_level <- raw_data |>
  filter(grepl("nationale", locality)) |>
  select(-n_offers)

offers_country <- raw_data |>
  filter(grepl("Total d.offres", locality)) |>
  select(year, n_offers)

country_level_data <- full_join(country_level, offers_country) |>
  select(year, locality, n_offers, everything()) |>
  mutate(locality = "Grand-Duchy of Luxembourg")

# scrape communes table
current_communes <- "https://is.gd/lux_communes" |>
  rvest::read_html() |>
  rvest::html_table() |>
  purrr::pluck(2) |>
  janitor::clean_names() |>
  dplyr::filter(name_2 != "Name") |>
  dplyr::rename(commune = name_2) |>
  dplyr::mutate(commune = stringr::str_remove(commune, " .$"))
