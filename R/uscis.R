#' @importFrom fs dir_ls
#' @importFrom stringr str_match str_detect str_c
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr set_names map_dbl map detect_index discard is_empty reduce
#' @importFrom dplyr filter mutate if_else coalesce across inner_join filter select
#' @importFrom tidyr unnest
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
                              mutate(data, across(-1, as.numeric))
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
    select(-file, -`Pending Affirmative Asylum Cases`, -Receipts, -`Final Denials`, -starts_with("..."))

  fear <- data |> filter(dataset != "AFF") |> unnest(data) |> select(-file)

  list(AFF = aff,
       CF = filter(fear, dataset == "CF"),
       RF = filter(fear, dataset == "RF"))
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
