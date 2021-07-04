#' Get runs from GDQ tracker
#'
#' @inheritParams get_page_count
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' get_runs(event = "sgdq2021") %>% View()
#' }
get_runs <- function(event) {

  if (event %in% c("agdq2021", "sgdq2021")) event <- toupper(event)

  rvest::read_html(paste0("https://gamesdonequick.com/tracker/runs/", event)) %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    purrr::set_names(c("run", "players", "description", "run_start", "run_end", "bidwars")) %>%
    dplyr::mutate(
      run_start = lubridate::ymd_hms(run_start),
      run_end = lubridate::ymd_hms(run_end),
      run_duration_s = as.numeric(difftime(run_end, run_start, units = "secs")),
      run_duration_hms = hms::hms(seconds = run_duration_s),
      event = stringr::str_to_upper(stringr::str_extract(stringr::str_to_lower(.env$event), "[as]gdq\\d+")),
      year = stringr::str_extract(.data$event, "\\d+"),
      gdq = stringr::str_remove(.data$event, "\\d+")
    ) %>%
    dplyr::arrange(run_start)
}

#' Update all runs from GDQ tracker
#'
#' @inheritParams assemble_donations
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' assemble_runs(cache = FALSE)
#' }
assemble_runs <- function(events = NULL, cache = TRUE) {

  if (is.null(events)) {
    events <- fs::dir_ls(
      "data/gamesdonequick.com/runs/",
      regexp = "runs_[as]gdq\\d+\\.rds"
    )
  }

  runs <- purrr::map_df(events, readRDS) %>%
    dplyr::arrange(run_start) %>%
    as_tibble()

  if (cache) {
    cli::cli_alert_info("Caching run data at {.emph data/all_runs_gdqtracker.rds}")
    saveRDS(runs, "data/all_runs_gdqtracker.rds")
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

  purrr::walk(events, ~{
    cli::cli_progress_update(id = prg)
    cli::cli_text("Current event: {toupper(.x)}")
    out_file <- paste0("data/gamesdonequick.com/runs/runs_", .x, ".rds")

    if (!ignore_cache & file.exists(out_file)) return(tibble::tibble())

    if (!in_progress) {
      if (Sys.Date() < event_dates$end[tolower(event_dates$event) == .x]) return(tibble::tibble())
    }

    get_runs(event = .x) %>%
      saveRDS(out_file)
  })

  cli::cli_alert_success("Got runs from GDQ tracker!")
}