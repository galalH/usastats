#' @importFrom fs path_temp
#' @importFrom rvest session html_elements html_attr
#' @importFrom httr GET write_disk
#' @importFrom readxl excel_sheets read_excel
#' @importFrom purrr set_names map_dfr map_dbl
#' @importFrom dplyr select filter mutate if_else transmute
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_detect str_to_upper
read_wraps_chatty <- function(...) {
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
              coo = hcrcode(coo, src = "WRAPS"),
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
read_wraps <- function(...) { quietly(read_wraps_chatty)(...)$result }
