#' Fetch arbovirosis data from all cities of a given state in a given timeframe.
#'
#' This function uses InfoDengue API (https://info.dengue.mat.br/services/api)
#' to fetch arbovirosis data from all cities of a given state. It's possible to
#' get data related to `dengue`, `zika` and `chikungunya`.
#'
#' @param state_code string The state prefix
#' @param ey_start integer The start year.
#' @param ey_end integer The end year.
#' @param ew_start integer The first epidemiological week of interest. Ranges
#'   from 1 (default) to 52.
#' @param ew_end integer The last epidemiological week of interest. Ranges from
#'   1 to 52 (default).
#' @param disease string The disease of interest: `chikungunya`,`zika` or
#'   `dengue` (default)
#' @param normalize A boolean.
#'   * `TRUE`: the final cases count column will be normalized as
#'   100(n_of_cases)/max(n_of_cases).
#'   * `FALSE` (the default): the final cases count will be just the sum of
#'   cases in the given week.
#' @return A dataframe with the number of cases in the state per epidemiological
#'   week.
#'
#' @examples
#' \dontrun{
#' df <- fetch_data_from_state("SP", 2019, 2023)
#' }
#' @export
#' @import tidyverse
fetch_data_from_state <- function(state_code,
                                  ey_start,
                                  ey_end,
                                  ew_start = 1,
                                  ew_end = 52,
                                  disease = "dengue",
                                  normalize = FALSE) {
  if (missing(ey_end)) {
    warning("Since `ey_end` was not provided, using the same as `ey_start`.\n")
    ey_end <- ey_start
  }

  # municipalities <- read.csv("data/utils/municipalities.csv")

  cities <- denguetracker::municipalities |>
    dplyr::filter(uf_code == state_code) |>
    dplyr::select("municipio")

  url <- "https://info.dengue.mat.br/api/alertcity?"
  format <- "csv"

  dados <- NULL
  j <- 0
  for (geocode in cities$municipio) {
    j <- j + 1
    cons1 <-
      paste0(
        url, "geocode=", geocode, "&disease=", disease, "&format=",
        format, "&ew_start=", ew_start, "&ew_end=", ew_end, "&ey_start=",
        ey_start, "&ey_end=", ey_end
      )
    data_ <- readr::read_csv(cons1, show_col_types = FALSE) |>
      dplyr::arrange(data_iniSE) |>
      dplyr::select(data_iniSE, SE, casos, casos_est, casos_est_min, casos_est_max)
    data_$data_iniSE <- as.Date(data_$data_iniSE, format = "%Y-%m-%d")
    data_$casos <- as.double(data_$casos)
    data_$casos_est <- as.double(data_$casos_est)
    data_$casos_est_max <- as.double(data_$casos_est_max)
    data_$casos_est_min <- as.double(data_$casos_est_min)
    
    cat("\rProgress (cities downloaded): ", j, "/", length(cities$municipio))
    if (is.null(dados)) {
      dados <- data_
    } else {
      dados <- dplyr::bind_rows(dados, data_)
    }
  }

  aggregated_data <- dados |>
    dplyr::mutate(ew_start = data_iniSE, ew = SE) |>
    dplyr::group_by(ew_start, ew) |>
    dplyr::summarise(sum_of_cases = sum(casos), cases_est_id = sum(casos_est), cases_est_id_min = sum(casos_est_min), cases_est_id_max = sum(casos_est_max)) |>
    dplyr::distinct(ew_start, .keep_all = TRUE) |>
    dplyr::select(ew_start, ew, sum_of_cases, cases_est_id, cases_est_id_min, cases_est_id_max)

  if (normalize) {
    aggregated_data <- aggregated_data |>
      dplyr::mutate(cases_normalized = 100 * (sum_of_cases / max(aggregated_data$sum_of_cases))) |>
      dplyr::select(ew_start, cases_normalized)
  }
  return(aggregated_data)
}


#' Download data from all cities, without aggregating them as one state.
#'
#' @param state_code string The state prefix
#' @param ey_start integer The start year.
#' @param ey_end integer The end year.
#' @param ew_start integer The first epidemiological week of interest. Ranges
#'   from 1 (default) to 52.
#' @param ew_end integer The last epidemiological week of interest. Ranges from
#'   1 to 52 (default).
#' @param disease string The disease of interest: `chikungunya`,`zika` or
#'   `dengue` (default)
#'
#' @return final_data dataframe Dataframe with cases of all cities in the given
#'  state within the given time window separeted by their geocode.
#' @export
#'
fetch_data_from_cities <- function(state_code,
                                   ey_start,
                                   ey_end,
                                   ew_start = 1,
                                   ew_end = 52,
                                   disease = "dengue") {
  
  if (missing(ey_end)) {
    warning("Since `ey_end` was not provided, using the same as `ey_start`.\n")
    ey_end <- ey_start
  }
  
  cities <- denguetracker::municipalities |>
    dplyr::filter(uf_code == state_code) |>
    dplyr::select("municipio")
  
  url <- "https://info.dengue.mat.br/api/alertcity?"
  format <- "csv"
  
  final_data <- NULL
  j <- 0
  for (geocode in cities$municipio) {
    j <- j + 1
    cons1 <-
      paste0(
        url, "geocode=", geocode, "&disease=", disease, "&format=",
        format, "&ew_start=", ew_start, "&ew_end=", ew_end, "&ey_start=",
        ey_start, "&ey_end=", ey_end
      )
    data_ <- readr::read_csv(cons1, show_col_types = FALSE) |>
      dplyr::arrange(data_iniSE) |>
      dplyr::select(data_iniSE, casos)
    data_$geocode <- geocode
    data_$data_iniSE <- as.Date(data_$data_iniSE, format = "%Y-%m-%d")
    data_$casos <- as.double(data_$casos)
    
    cat("\rProgress (cities downloaded): ", j, "/", length(cities$municipio))
    if (is.null(final_data)) {
      final_data <- data_
    } else {
      final_data <- dplyr::bind_rows(final_data, data_)
    }
  }
  
  return(final_data)
}