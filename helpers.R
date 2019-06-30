# Helper functions that do the heavy lifting
library(purrr)
library(dplyr)
library(magrittr)
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

  # cliapp::cli_text(event, ": ", url)

  read_html(url) %>%
    html_table() %>%
    extract2(1) %>%
    set_names(c("name", "time", "amount", "comment")) %>%
    mutate(time = mdy_hms(time),
           #amount_lbl = amount,
           amount = stringr::str_remove(amount, "\\$"),
           amount = stringr::str_remove(amount, ","),
           amount = as.numeric(amount)) %>%
    as_tibble()
}

get_donations <- function(event = "sgdq2018") {
  pages <- seq_len(get_page_count(event = event))

  #cli_h2("Getting donations for {event}")
  prg <- cli_progress_bar(
    format = glue::glue("{event}: [:bar] :percent (:elapsedfull)"),
    total = length(pages),
    clear = TRUE,
    current = emo::ji("moneybag"))

  map_df(pages, function(page) {
    Sys.sleep(.5)
    prg$tick()
    get_donation_page(event = event, page = page)
  })
}

assemble_donations <- function(events = NULL) {

  if (is.null(events)) {
    # events <- rev(paste0(c("a", "s"), rep(paste0("gdq", 2011:2018), each = 2)))
    # events <- paste0("data/", events, ".rds")
    events <- list.files("data", "donations_[as]gdq\\d+\\.rds", full.names = TRUE)
  }

  donations <- map_df(events, ~{
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
      group_by(event) %>%
      mutate(time_rel = start %--% time / dhours(1) / (event_duration * 24))

  donations <- donations %>%
    arrange(time) %>%
    mutate(day = wday(time, label = TRUE),
           day_num = paste0(day, " (", day(time), ".)"))

  amount_breaks <- map(c(5, 10, 25), ~.x * 10^{0:4}) %>%
    flatten_dbl() %>%
    sort() %>%
    c(0, .)

  amount_c_labels <- paste0("<= ", scales::dollar(amount_breaks[-1]))
  donations %>%
    mutate(amount_c = cut(amount, breaks = amount_breaks, labels = amount_c_labels))
}

# Getting runs ----

get_runs <- function(event) {
  require(rvest)

  runs <- read_html(paste0("https://gamesdonequick.com/tracker/runs/", event)) %>%
    html_table() %>%
    extract2(1) %>%
    set_names(c("run", "players", "description", "run_start", "run_end", "bidwars")) %>%
    mutate(run_start = mdy_hms(run_start),
           run_end = mdy_hms(run_end),
           run_duration_s = as.numeric(difftime(run_end, run_start, units = "secs")),
           run_duration_hms = hms::hms(seconds = run_duration_s),
           event = str_to_upper(str_extract(event, "[as]gdq\\d+")),
           year = str_extract(event, "\\d+"),
           gdq = str_remove(runs$event, "\\d+")) %>%
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
  "AGDQ2011", ymd("2011-01-06"), ymd("2011-01-11"),
  "AGDQ2012", ymd("2012-01-04"), ymd("2012-01-09"),
  "AGDQ2013", ymd("2013-01-06"), ymd("2013-01-12"),
  "AGDQ2014", ymd("2014-01-05"), ymd("2014-01-11"),
  "AGDQ2015", ymd("2015-01-04"), ymd("2015-01-10"),
  "AGDQ2016", ymd("2016-01-03"), ymd("2016-01-10"),
  "AGDQ2017", ymd("2017-01-08"), ymd("2017-01-15"),
  "AGDQ2018", ymd("2018-01-07"), ymd("2018-01-14"),
  "AGDQ2019", ymd("2019-01-06"), ymd("2019-01-13"),
  "SGDQ2011", ymd("2011-08-04"), ymd("2011-08-06"),
  "SGDQ2012", ymd("2012-05-24"), ymd("2012-05-28"),
  "SGDQ2013", ymd("2013-07-25"), ymd("2013-07-30"),
  "SGDQ2014", ymd("2014-06-22"), ymd("2014-06-28"),
  "SGDQ2015", ymd("2015-07-26"), ymd("2015-08-02"),
  "SGDQ2016", ymd("2016-07-03"), ymd("2016-07-09"),
  "SGDQ2017", ymd("2017-07-02"), ymd("2017-07-09"),
  "SGDQ2018", ymd("2018-06-24"), ymd("2018-07-01"),
  "SGDQ2019", ymd("2019-06-23"), ymd("2019-06-30")
) %>%
  mutate(event_duration = start %--% end / ddays(1),
         start = as.POSIXct(start),
         end = as.POSIXct(end))

# For documents ----
library(ggplot2)
library(scales)
library(hrbrthemes)
library(knitr)
library(ggbeeswarm)

theme_set(
  theme_ipsum_rc(plot_margin = margin(10, 10, 10, 10)) +
    theme(legend.position = "top")
)

## Plot parts
euro_scale <- unit_format(suffix = "€", sep = "", big.mark = ".", decimal.mark = ",")
euro_axis <- function(...) dup_axis(~.*.85, labels = euro_scale, name = NULL, ...)

p_title <- "Games Done Quick: Donation Breakdown"
p_title_r <- "Games Done Quick: Runs"

p_caption <- "@jemus42 – gdq.tadaa-data.de\nAll data from gamesdonequick.com/tracker"

amount_breaks <- map(c(5, 10, 25), ~.x * 10^{0:4}) %>%
  flatten_dbl() %>%
  sort() %>%
  c(0, .)
amount_c_labels <- paste0("<= ", scales::dollar(amount_breaks[-1]))

## Chunk options
knitr::opts_chunk$set(
  cache.path = "cache",
  fig.path = "plots/",
  fig.retina = 2,
  error = FALSE,
  warning = FALSE,
  message = FALSE
  )
