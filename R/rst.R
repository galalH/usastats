#' @importFrom countrycode countrycode
#' @importFrom tibble tibble
#' @importFrom dplyr left_join mutate case_when
#' @importFrom stringr str_detect
#' @importFrom purrr set_names compose
wraps2hcr <- function(x) {
  dict <-
    tibble(coo = unique(x),
           iso = countrycode(coo, origin = "country.name.en", destination = "iso3c")) |>
    left_join(popdata::pd_countries) |>
    # manually fix some entries that were missed by the automatic mapping
    mutate(code = case_when(coo == "Yemen (Sanaa)" ~ "YEM",
                            is.na(code) ~ "UNK",
                            TRUE ~ code))
  dict <- set_names(dict$code, dict$coo)
  dict[x]
}

#' @importFrom fs path_temp
#' @importFrom rvest session html_elements html_attr
#' @importFrom httr GET write_disk
#' @importFrom readxl excel_sheets read_excel
#' @importFrom purrr set_names map_dfr map_dbl
#' @importFrom dplyr select filter mutate if_else transmute
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_detect str_to_upper
read_wrapsnet_chatty <- function(...) {
  tmpf <- path_temp("admissions.xlsx")

  report_path <-
    session("https://www.wrapsnet.org/admissions-and-arrivals/") |>
    html_elements("ul.links-list a[href$='.xlsx']") |>
    html_attr("href")

  GET("https://www.wrapsnet.org", path = URLencode(report_path), write_disk(tmpf, overwrite = TRUE))

  data <-
    excel_sheets(tmpf) |>
    tail(-1) |>
    set_names() |>
    map_dfr(read_excel, path = tmpf, skip = 7, .id = "year")

  data <-
    data |>
    select(year, coo = `...2`, OCT:SEP) |>
    filter(!is.na(coo), !str_detect(coo, "Total")) |>
    pivot_longer(OCT:SEP, names_to = "month", values_to = "n") |>
    mutate(year = if_else(month %in% c("OCT", "NOV", "DEC"), as.numeric(year)-1, as.numeric(year)))

  data <-
    data |>
    transmute(dataset = "WRAPS",
              year, month = map_dbl(month, agrep, str_to_upper(month.abb), max.distance = 0),
              coo = wraps2hcr(coo),
              flow = "admissions",
              n)

  list(flows = data)
}

#' RST data processing functions
#'
#' Read RST data
#'
#' @param ... Not used
#' @return A list of tibbles.
#' @importFrom purrr quietly
#' @rdname rst
#' @export
read_wrapsnet <- function(...) { quietly(read_wrapsnet_chatty)(...)$result }
