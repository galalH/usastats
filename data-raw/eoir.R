library(tidyverse)

eoir2hcr <-
  # Extracted from the July-2021 EOIR FOIA release
  read_tsv("data-raw/tblLookupNationality.csv") |>
  filter(blnActive == 1) |>
  mutate(iso = countrycode::countrycode(NAT_COUNTRY_NAME, origin = "country.name.en", destination = "iso3c")) |>
  left_join(popdata::pd_countries) |>
  # Fix codes for a few countries that we missed
  mutate(code = case_when(NAT_COUNTRY_NAME == "HOLLAND" ~ "NET",
                          NAT_COUNTRY_NAME == "REUNIOUN" ~ "REU",
                          NAT_COUNTRY_NAME == "UZEBEKISTAN" ~ "UZB",
                          NAT_COUNTRY_NAME == "NO NATIONALITY" |
                          NAT_COUNTRY_NAME == "STATELESS - ALIEN UNABLE TO NAME A COUNTRY" ~ "STA",
                          is.na(code) ~ "UNK",
                          TRUE ~ code)) |>
  select(coo = NAT_COUNTRY_NAME, code)

usethis::use_data(eoir2hcr, internal = TRUE, overwrite = TRUE)
