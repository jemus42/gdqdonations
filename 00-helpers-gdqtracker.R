# Helper functions that do the heavy lifting
library(purrr)
library(dplyr)
library(lubridate)
library(stringr)
library(forcats)
library(cliapp)
library(glue)
library(rvest)

# Donations ----
get_page_count <- function(event = "sgdq2019") {

  url_base <- "https://gamesdonequick.com/tracker/donations/"
  url <- paste0(url_base, event)

  read_html(url) %>%
    html_node("#page+ label") %>%
    html_text() %>%
    str_extract("\\d+") %>%
    as.numeric()
}

get_donation_page <- function(event = "sgdq2019", page = 1) {

  url_base <- "https://gamesdonequick.com/tracker/donations/"
  url <- paste0(url_base, event, "?page=", page)

  read_html(url) %>%
    html_table() %>%
    magrittr::extract2(1) %>%
    set_names(c("name", "time", "amount", "comment")) %>%
    mutate(
      time = mdy_hms(time),
      amount = stringr::str_remove(amount, "\\$"),
      amount = stringr::str_remove(amount, ","),
      amount = as.numeric(amount)
    ) %>%
    as_tibble()
}

get_donations <- function(event = "sgdq2018") {
  pages <- seq_len(get_page_count(event = event))

  #cli_h2("Getting donations for {event}")
  prg <- cli_progress_bar(
    format = glue::glue("{event}: [:bar] :percent (:elapsedfull)"),
    total = length(pages),
    clear = TRUE,
    current = emo::ji("moneybag")
  )

  map_df(pages, function(page) {
    Sys.sleep(.5)
    prg$tick()
    get_donation_page(event = event, page = page)
  })
}

assemble_donations <- function(events = NULL) {

  if (is.null(events)) {
    events <- list.files("data", "donations_[as]gdq\\d+\\.rds", full.names = TRUE)
  }

  amount_breaks <- map(c(5, 10, 25), ~.x * 10^{0:4}) %>%
    flatten_dbl() %>%
    sort() %>%
    c(0, .)
  amount_c_labels <- paste0("<= ", scales::dollar(amount_breaks[-1]))

  map_df(events, ~{
    readRDS(.x) %>%
      mutate(event = str_extract(.x, "[as]gdq\\d{4}") %>% str_to_upper()) %>%
      as_tibble()
    }) %>%
    arrange(time) %>%
    #mutate(day_num = forcats::fct_inorder(day_num, ordered = TRUE)) %>%
    left_join(
      event_dates,
      by = "event"
    ) %>%
    arrange(time) %>%
    mutate(
      day = wday(time, label = TRUE),
      day_num = paste0(day, " (", day(time), ".)"),
      year = str_extract(event, "\\d+"),
      gdq = str_remove(event, "\\d+"),
      amount_c = cut(amount, breaks = amount_breaks, labels = amount_c_labels),
      time_rel = ((start %--% time) / dminutes(1)) / ((start %--% end)/dminutes(1))
    ) %>%
    select(-start, -end, -event_duration)
}

# Getting runs from GDQ tracker----

get_runs <- function(event) {
  require(rvest)

  runs <- read_html(paste0("https://gamesdonequick.com/tracker/runs/", event)) %>%
    html_table() %>%
    magritrr::extract2(1) %>%
    set_names(c("run", "players", "description", "run_start", "run_end", "bidwars")) %>%
    mutate(
      run_start = mdy_hms(run_start),
      run_end = mdy_hms(run_end),
      run_duration_s = as.numeric(difftime(run_end, run_start, units = "secs")),
      run_duration_hms = hms::hms(seconds = run_duration_s),
      event = str_to_upper(str_extract(event, "[as]gdq\\d+")),
      year = str_extract(event, "\\d+"),
      gdq = str_remove(runs$event, "\\d+")
    ) %>%
    arrange(run_start) %>%
    as_tibble()
}

assemble_runs <- function(events = NULL) {

  if (is.null(events)) {
    events <- list.files("data", "runs_[as]gdq\\d+\\.rds", full.names = TRUE)
  }

  map_df(events, function(x) {
    readRDS(x)
  }) %>%
    arrange(run_start) %>%
    as_tibble()
}

# Proper event dates ----
# https://en.wikipedia.org/wiki/Games_Done_Quick#List_of_marathons
event_dates <- tribble(
  ~event,     ~start,            ~end,
  "AGDQ2011", ymd("2011-01-06", tz = "UTC"), ymd("2011-01-11", tz = "UTC"),
  "AGDQ2012", ymd("2012-01-04", tz = "UTC"), ymd("2012-01-09", tz = "UTC"),
  "AGDQ2013", ymd("2013-01-06", tz = "UTC"), ymd("2013-01-12", tz = "UTC"),
  "AGDQ2014", ymd("2014-01-05", tz = "UTC"), ymd("2014-01-11", tz = "UTC"),
  "AGDQ2015", ymd("2015-01-04", tz = "UTC"), ymd("2015-01-10", tz = "UTC"),
  "AGDQ2016", ymd("2016-01-03", tz = "UTC"), ymd("2016-01-10", tz = "UTC"),
  "AGDQ2017", ymd("2017-01-08", tz = "UTC"), ymd("2017-01-15", tz = "UTC"),
  "AGDQ2018", ymd("2018-01-07", tz = "UTC"), ymd("2018-01-14", tz = "UTC"),
  "AGDQ2019", ymd("2019-01-06", tz = "UTC"), ymd("2019-01-13", tz = "UTC"),
  "SGDQ2011", ymd("2011-08-04", tz = "UTC"), ymd("2011-08-06", tz = "UTC"),
  "SGDQ2012", ymd("2012-05-24", tz = "UTC"), ymd("2012-05-28", tz = "UTC"),
  "SGDQ2013", ymd("2013-07-25", tz = "UTC"), ymd("2013-07-30", tz = "UTC"),
  "SGDQ2014", ymd("2014-06-22", tz = "UTC"), ymd("2014-06-28", tz = "UTC"),
  "SGDQ2015", ymd("2015-07-26", tz = "UTC"), ymd("2015-08-02", tz = "UTC"),
  "SGDQ2016", ymd("2016-07-03", tz = "UTC"), ymd("2016-07-09", tz = "UTC"),
  "SGDQ2017", ymd("2017-07-02", tz = "UTC"), ymd("2017-07-09", tz = "UTC"),
  "SGDQ2018", ymd("2018-06-24", tz = "UTC"), ymd("2018-07-01", tz = "UTC"),
  "SGDQ2019", ymd("2019-06-23", tz = "UTC"), ymd("2019-06-30", tz = "UTC")
) %>%
  mutate(event_duration = start %--% end / ddays(1))

saveRDS(event_dates, "data/event_dates.rds")
