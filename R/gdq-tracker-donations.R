#' Get page count of GDQ tracker
#'
#' @param event Event such as `"agdq2019"`, lower case.
#'
#' @return Page count as a `numeric(1)`.
#' @export
#'
#' @examples
#' get_page_count(event = "sgdq2021")
get_page_count <- function(event = "sgdq2021") {

  if (event %in% c("agdq2021", "sgdq2021")) event <- toupper(event)

  url_base <- "https://gamesdonequick.com/tracker/donations/"
  url <- paste0(url_base, event)

  rvest::read_html(url) %>%
    rvest::html_node("#page+ label") %>%
    rvest::html_text() %>%
    stringr::str_extract("\\d+") %>%
    as.numeric()
}

#' Get a single page from the tracker
#'
#' @inheritParams get_page_count
#' @param page Page to get, a single number.
#'
#' @return A [tibble][tibble::tibble].
#' @export
#'
#' @examples
#' \dontrun{
#' get_donation_page(event = "sgdq2021", page = 1)
#' }
get_donation_page <- function(event = "sgdq2021", page = 1) {

  if (event %in% c("agdq2021", "sgdq2021")) event <- toupper(event)

  url_base <- "https://gamesdonequick.com/tracker/donations/"
  url <- paste0(url_base, event, "?page=", page)

  rvest::read_html(url) %>%
    rvest::html_table() %>%
    magrittr::extract2(1) %>%
    purrr::set_names(c("name", "time", "amount", "comment")) %>%
    dplyr::mutate(
      time = lubridate::ymd_hms(time),
      amount = stringr::str_remove(amount, "\\$"),
      amount = stringr::str_remove(amount, ","),
      amount = as.numeric(amount)
    ) %>%
    as_tibble()
}

#' Get all donations for an event from the tracker
#'
#' @inheritParams get_page_count
#' @param delay `[0.5]`: Seconds to wait between pages. Don't annoy the webserver.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' donations_sgdq2021 <- get_donations(event = "sgdq2021")
#' }
get_donations <- function(event = "sgdq2021", delay = .5) {

  #if (event %in% c("agdq2021", "sgdq2021")) event <- toupper(event)

  pages <- seq_len(get_page_count(event = event))

  #cli::cli_text("Getting donations for {event}")
  prg <- cli::cli_progress_bar(
    name = glue::glue("Getting {toupper(event)}"),
    type = "iterator",
    # format = glue::glue("{event}: [:bar] :percent (:elapsedfull)"),
    total = length(pages),
    clear = TRUE,
    current = TRUE
  )

  purrr::map_df(pages, ~{
    Sys.sleep(delay)
    cli::cli_progress_update(id = prg)
    get_donation_page(event = event, page = .x)
  })
}

#' Aggregate all donation data from the tracker in one file
#'
#' @param events `[NULL]` Optional vector of file paths to individual `.rds` files.
#' @param cache `[TRUE]` Save aggregated dataset at `"data/all_donations.rds"`.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' assemble_donations()
#' }
assemble_donations <- function(events = NULL, cache = TRUE) {

  if (is.null(events)) {
    events <- fs::dir_ls(
      "data/gamesdonequick.com/donations/",
      regexp = "donations_[as]gdq\\d+\\.rds"
    )
  }

  amount_breaks <- purrr::map(c(5, 10, 25), ~.x * 10^{0:5}) %>%
    purrr::flatten_dbl() %>%
    sort() %>%
    c(0, .)
  amount_c_labels <- paste0("<= ", scales::dollar(amount_breaks[-1]))

  all_donations <- purrr::map_df(events, ~{
    readRDS(.x) %>%
      dplyr::mutate(
        event = stringr::str_extract(.x, "[as]gdq\\d{4}") %>%
          stringr::str_to_upper()
      ) %>%
      as_tibble()
  }) %>%
    dplyr::arrange(time) %>%
    #mutate(day_num = forcats::fct_inorder(day_num, ordered = TRUE)) %>%
    dplyr::left_join(
      event_dates,
      by = "event"
    ) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      day = lubridate::wday(time, label = TRUE),
      day_num = paste0(day, " (", lubridate::day(time), ".)"),
      year = stringr::str_extract(event, "\\d+"),
      gdq = stringr::str_remove(event, "\\d+"),
      amount_c = cut(amount, breaks = amount_breaks, labels = amount_c_labels),
      time_rel = ((start %--% time) / lubridate::dminutes(1)) / ((start %--% end)/lubridate::dminutes(1))
    ) %>%
    dplyr::select(-start, -end, -event_duration)

  if (cache) {
    cli::cli_alert_info("Caching donation data at {.emph data/all_donations.rds}")
    saveRDS(all_donations, "data/all_donations.rds")
  }

  all_donations
}


#' Update donation data from GDQ tracker
#'
#' @param events Events such as `"agdq2019"`, lower case.
#' @param ignore_cache `[FALSE]`: If `TRUE`, ignore cached file and re-retrieve data.
#' @param in_progress `[FALSE]`: If `TRUE`, donations for in-progress events are retrieved.
#'
#' @return Invisibly: tibble of donations.
#' @export
#'
#' @examples
#' \dontrun{
#' update_tracker_donations(
#'   events = c("agdq2021", "sgdq2021"),
#'   ignore_cache = TRUE,
#'   in_progress = TRUE
#' )
#' }
update_tracker_donations <- function(events, ignore_cache = FALSE, in_progress = FALSE) {
  prg <- cli::cli_progress_bar(name = "Getting donations", total = length(events))

  donations <- purrr::walk(events, ~{
    cli::cli_progress_update(id = prg)
    cli::cli_alert_info("Current event: {toupper(.x)}")

    out_file <- file.path("data/gamesdonequick.com/donations/", paste0("donations_", .x, ".rds"))

    if (!ignore_cache & file.exists(out_file)) return(tibble::tibble())

    if (!in_progress) {
      if (Sys.Date() < event_dates$end[tolower(event_dates$event) == .x]) return(tibble::tibble())
    }

    get_donations(event = .x) %>%
      saveRDS(file = out_file)
    beepr::beep(2)
  })

  cli::cli_alert_success("Got donations!")
}