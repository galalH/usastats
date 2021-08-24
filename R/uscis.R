#' @importFrom countrycode countrycode
#' @importFrom tibble tibble
#' @importFrom dplyr left_join mutate case_when
#' @importFrom stringr str_detect
#' @importFrom purrr set_names
uscis2hcr <- function(x) {
  dict <-
    tibble(coo = unique(x),
           iso = countrycode(coo, origin = "country.name.en", destination = "iso3c")) |>
    left_join(popdata::pd_countries) |>
    # manually fix some entries that were missed by the automatic mapping
    mutate(code = case_when(coo == "COTE D' IVORE" ~ "ICO",
                            coo == "STATELESS" ~ "STA",
                            is.na(code) ~ "UNK",
                            TRUE ~ code))
  dict <- set_names(dict$code, dict$coo)
  dict[x]
}

#' @importFrom fs dir_ls
#' @importFrom stringr str_match str_detect str_c str_to_upper
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr set_names map_dbl map detect_index discard is_empty reduce compose
#' @importFrom dplyr filter mutate if_else coalesce across inner_join filter select transmute count rename_with
#' @importFrom tidyr unnest pivot_longer replace_na
#' @importFrom forcats fct_collapse
#' @importFrom readr parse_number
#' @importFrom readxl read_excel excel_sheets
read_uscis_chatty <- function(path, ...) {
  files <-
    dir_ls(path, recurse = TRUE) |>
    str_match(".+(AFF|CF|RF) Nationality (\\w+) (\\d{4}).xlsx$") |>
    as_tibble(.name_repair = "universal") |>
    set_names("file", "dataset", "month", "year") |>
    filter(!is.na(dataset)) |>
    mutate(month = map_dbl(month, agrep, month.name, max.distance = 0),
           year = as.numeric(year))

  data <-
    files |>
    mutate(data = map(file,
                      function(file) {
                        map(excel_sheets(file),
                            function(sheet) {
                              data <- read_excel(file, sheet = sheet)
                              if (str_detect(names(data)[1], "(?i)citizenship"))
                                return(data)
                              idx <- detect_index(data[[1]],
                                                  ~if_else(is.na(.), FALSE, str_detect(., "(?i)citizenship")),
                                                  .dir = "backward")
                              if (idx == 0)
                                return(tibble())
                              nms <- coalesce(unlist(data[idx-is.na(data[[idx, 2]]),]),
                                              str_c("...", 1:ncol(data)))
                              data <- read_excel(file, sheet = sheet, skip = idx,
                                                 .name_repair = ~ifelse(. == "", nms, .))
                              mutate(data, across(-1, compose(parse_number, as.character)))
                            }) |>
                          discard(is_empty) |>
                          reduce(inner_join)
                      }))

  aff <-
    data |>
    filter(dataset == "AFF") |>
    unnest(data) |>
    mutate(`Cases Received` = if_else(is.na(`Cases Received`), Receipts, `Cases Received`),
           `Final Denial` = if_else(is.na(`Final Denial`),`Final Denials`, `Final Denial`),
           Pending = if_else(is.na(Pending), `Pending Affirmative Asylum Cases`, Pending)) |>
    select(-file, -`Pending Affirmative Asylum Cases`, -Receipts, -`Final Denials`, -starts_with("...")) |>
    rename_with(~"coo", contains("citizenship")) |>
    replace_na(list(coo = "UNKNOWN")) |>
    filter(coo == str_to_upper(coo))

  aff_stock <-
    aff |>
    transmute(dataset, year, month, coo, start = NA_real_, end = Pending) |>
    pivot_longer(start:end, names_to = "stock", values_to = "n")

  aff_flow <-
    aff |>
    select(-Pending) |>
    pivot_longer(-c(dataset, month, year, coo), names_to = "flow", values_to = "n") |>
    mutate(flow = compose(as.character, fct_collapse)(
      flow,
      applications = c("Cases Received", "Reopened"),
      recognitions = c("Grants"),
      rejections = c("Final Denial", "Referrals"),
      closures = c("Administrative Closures"))) |>
    filter(flow %in% c("applications", "recognitions", "rejections", "closures")) |>
    count(dataset, year, month, coo, flow, wt = n)

  fear <-
    data |>
    filter(dataset != "AFF") |>
    unnest(data) |>
    select(-file) |>
    rename_with(~"coo", contains("citizenship")) |>
    replace_na(list(coo = "UNKNOWN")) |>
    filter(coo == str_to_upper(coo))

  fear_stock <-
    fear |>
    transmute(dataset, year, month, coo, start = `Pending At Beginning`, end = `Pending At End`) |>
    pivot_longer(start:end, names_to = "stock", values_to = "n")

  fear_flow <-
    fear |>
    select(-starts_with("Pending")) |>
    pivot_longer(-c(dataset, month, year, coo), names_to = "flow", values_to = "n") |>
    mutate(flow = compose(as.character, fct_collapse)(
      flow,
      applications = "Receipts Count",
      recognitions = "Fear Found",
      rejections = "Fear Not Found",
      other_level = "closure")) |>
    count(dataset, year, month, coo, flow, wt = n)

  list(stock = bind_rows(aff_stock, fear_stock) |> mutate(coo = uscis2hcr(coo)),
       flows = bind_rows(aff_flow, fear_flow) |> mutate(coo = uscis2hcr(coo)))
}

#' RSD data processing functions
#'
#' Read USCIS data
#'
#' @param path Path to directory where the data is located
#' @param ... Not used
#' @return A list of tibbles.
#' @importFrom purrr quietly
#' @rdname rsd
#' @export
read_uscis <- function(path = ".", ...) { quietly(read_uscis_chatty)(path, ...)$result }
