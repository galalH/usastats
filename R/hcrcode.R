#' @importFrom countrycode countrycode
#' @importFrom tibble tibble
#' @importFrom dplyr left_join mutate case_when
#' @importFrom stringr str_detect
#' @importFrom purrr set_names
hcrcode <- function(x, src) {
  dict <-
    tibble(coo = unique(x),
           iso = countrycode(coo, origin = "country.name.en", destination = "iso3c")) |>
    left_join(popdata::pd_countries) |>
    # manually fix some entries that were missed by the automatic mapping
    mutate(code = case_when(src == "EOIR" & coo == "HOLLAND" ~ "NET",
                            src == "EOIR" & coo == "REUNIOUN" ~ "REU",
                            src == "EOIR" & coo == "UZEBEKISTAN" ~ "UZB",
                            # FIXME: hackish...
                            src == "EOIR" &  str_detect(coo, "(STATELESS|COUNTRY|NO NATIONALITY)") ~ "STA",
                            src == "USCIS" & coo == "COTE D' IVORE" ~ "ICO",
                            src == "USCIS" & coo == "STATELESS" ~ "STA",
                            src == "WRAPS" & coo == "Yemen (Sanaa)" ~ "YEM",
                            is.na(code) ~ "UNK",
                            TRUE ~ code))

  set_names(dict$code, dict$coo)[x]
}
