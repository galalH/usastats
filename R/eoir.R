#' @importFrom fs dir_ls
#' @importFrom pdftools pdf_text
#' @importFrom stringr str_match str_detect str_split
#' @importFrom tibble tribble as_tibble
#' @importFrom dplyr mutate lead select filter if_any across bind_rows transmute
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything last_col
#' @importFrom purrr map_int detect_index pmap map_dfr
#' @importFrom readr read_lines

read_eoir_chatty <- function(path, ...) {
  file <- dir_ls(path, recurse = TRUE, regex = "EOIR.*\\.pdf$")

  pages <- pdf_text(file)

  cy <- str_match(pages[1], "CY (\\d{4})")[,2]

  datasets <-
    tribble(~dataset,                                                 ~ncol,
            "Affirmative Asylum Applications Received",               3,
            "Defensive Asylum Applications Received",                 3,
            "Affirmative Asylum Statistics",                          7,
            "Defensive Asylum Statistics",                            7,
            "Initial Case Completions Withholding of Removal Grants", 2,
            "Initial Case Completions Convention Against Torture",    3) |>
    mutate(startpg = map_int(dataset, \(x) detect_index(pages, \(y) str_detect(y, x))),
           endpg = lead(startpg-1, default = length(pages)))

  data <-
    datasets |>
    mutate(data =
             pmap(list(ncol = ncol, startpg = startpg, endpg = endpg),
                  function(ncol, startpg, endpg) {
                    map_dfr(pages[startpg:endpg],
                            function(page) {
                              read_lines(page) |>
                                str_split("\\s{2,}", simplify = TRUE) |>
                                as_tibble(.name_repair = "universal") |>
                                select(last_col(ncol-1):last_col()) |>
                                filter(!if_any(everything(), ~.==""),
                                       !if_any(-1, ~!str_detect(., "(\\*|\\d+)")),
                                       !across(1, ~str_detect(., "(?i)total"))) |>
                                mutate(across(-1, as.numeric))
                            })

                  }))

  stock <-
    bind_rows(AFF = data$data[[1]], DEF = data$data[[2]], .id = "dataset") |>
    transmute(dataset,
              year = cy, month = NA_real_,
              coo = ...1,
              start = NA_real_, end = ...3) |>
    pivot_longer(start:end, names_to = "stock", values_to = "n")

  flows <-
    bind_rows(AFF = data$data[[3]], DEF = data$data[[4]], .id = "dataset") |>
    transmute(dataset,
              year = cy, month = NA_real_,
              coo = ...1,
              applications = ...2,
              # FIXME: how should we handle censored values?
              recognitions = ...3, rejections = ...4, closures = ...5 + ...6 + ...7) |>
    pivot_longer(applications:closures, names_to = "flow", values_to = "n")

  wth <-
    data$data[[5]] |>
    transmute(dataset = "WTH",
              year = cy, month = NA_real_,
              coo = ...1,
              flow = "recognitions",
              n = ...2)

  cat <-
    data$data[[6]] |>
    transmute(dataset = "CAT",
              year = cy, month = NA_real_,
              coo = ...1,
              flow = "recognitions",
              # FIXME: how should we handle censored values?
              n = ...2 + ...3)

  list(stock = stock,
       flows = bind_rows(flows, wth, cat))
}

#' RSD data processing functions
#'
#' Read EOIR data
#'
#' @param path Path to directory where the data is located
#' @param ... Not used
#' @return A list of tibbles.
#' @importFrom purrr quietly
#' @rdname rsd
#' @export
read_eoir <- function(path = ".", ...) { quietly(read_eoir_chatty)(path, ...)$result }
