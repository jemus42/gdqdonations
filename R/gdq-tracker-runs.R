#' Get runs from GDQ tracker
#'
#' @inheritParams get_page_count
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' get_runs(event = "sgdq2021") %>% View()
#' }
get_runs <- function(event = "latest") {

  if (event == "latest") {
    event <- gdqdonations::event_index$event[nrow(gdqdonations::event_index)]
  }

  event <- toupper(event)
  url <- gdqdonations::event_index$tracker_run_url[gdqdonations::event_index$event == event]

  rvest::read_html(paste0("https://gamesdonequick.com/", url)) %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    purrr::set_names(c("run", "players", "description", "run_start", "run_end", "bidwars")) %>%
    dplyr::mutate(
      run_start = lubridate::ymd_hms(.data$run_start),
      run_end = lubridate::ymd_hms(.data$run_end),
      run_duration_s = as.numeric(difftime(.data$run_end, .data$run_start, units = "secs")),
      run_duration_hms = hms::hms(seconds = .data$run_duration_s),
      event = stringr::str_to_upper(stringr::str_extract(stringr::str_to_lower(.env$event), "[as]gdq\\d+")),
      year = stringr::str_extract(.data$event, "\\d+"),
      gdq = stringr::str_remove(.data$event, "\\d+")
    ) %>%
    dplyr::arrange(.data$run_start)
}

#' Summarize GDQ tracker run data by event
#'
#' @param runs Run tibble as returned by [`assemble_runs()`].
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' assemble_runs() %>%
#'   summarize_runs()
#' }
summarize_runs <- function(runs) {
  runs %>%
    dplyr::filter(
      # AGDQ2013 bonus games (??)
      run_duration_s > 0,
      # AGDQ2014 bonus stream
      run_duration_s <= 250000
    ) %>%
    dplyr::group_by(.data$event) %>%
    dplyr::summarize(
      start_runs = min(run_start),
      end_runs = max(run_end),
      duration_d = (start_runs %--% end_runs) / lubridate::ddays(1)
    )
}

#' Update all runs from GDQ tracker
#'
#' @inheritParams assemble_donations
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' assemble_runs()
#' }
assemble_runs <- function(events = NULL, cache = FALSE) {

  if (is.null(events)) {
    events <- fs::dir_ls(
      fs::path(
        getOption("gdq_cache_dir"),
        "gamesdonequick.com"
      ),
      regexp = "runs_[as]gdq\\d+\\.rds"
    )
  }

  runs <- purrr::map_df(events, ~{
    readRDS(.x) %>%
      dplyr::mutate(
        run = as.character(.data$run),
        players = as.character(.data$players),
        description = as.character(.data$description),
        bidwars = as.character(.data$bidwars)
      )
    }) %>%
    dplyr::arrange(.data$run_start)

  if (cache) {
    cache_path <- fs::path(getOption("gdq_cache_dir"), "gdq_runs.rds")
    cli::cli_alert_info("Caching run data at {.emph {cache_path}}")
    saveRDS(runs, cache_path)
  }

  runs
}

#' Update run data from GDQ tracker
#'
#' @inheritParams update_tracker_donations
#'
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{
#' update_tracker_runs(
#'   events = c("agdq2021", "sgdq2021"),
#'   ignore_cache = TRUE,
#'   in_progress = TRUE
#' )
#' }
update_tracker_runs <- function(events, ignore_cache = FALSE, in_progress = FALSE) {
  prg <- cli::cli_progress_bar(name = "Getting runs", total = length(events))
  events <- toupper(events)

  purrr::walk(events, ~{
    cli::cli_progress_update(id = prg)
    cli::cli_text("Current event: {.x}")

    out_file <- fs::path(
      getOption("gdq_cache_dir"),
      "gamesdonequick.com",
      paste0("runs_", tolower(.x), ".rds")
    )

    if (!ignore_cache & file.exists(out_file)) return(tibble::tibble())

    if (!in_progress) {
      if (Sys.Date() < gdqdonations::event_index$end[gdqdonations::event_index$event == .x]) {
        return(tibble::tibble())
      }
    }

    usethis::use_directory(getOption("gdq_cache_dir"))
    get_runs(event = .x) %>%
      saveRDS(out_file)
  })

  cli::cli_alert_success("Got runs from GDQ tracker!")
}