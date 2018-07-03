# Helper functions that do the heavy lifting
library(purrr)
library(dplyr)
library(rvest)
library(magrittr)
library(lubridate)
library(stringr)
library(forcats)

get_page_count <- function(event = "sgdq2018") {
  cat("\n", event, ": Index\n", sep = "")
  url_base <- "https://gamesdonequick.com/tracker/donations/"
  url <- paste0(url_base, event)

  read_html(url) %>%
    html_node("#page+ label") %>%
    html_text() %>%
    str_extract("\\d+") %>%
    as.numeric()
}

get_page <- function(event = "sgdq2018", page = 1) {
  cat("\n", event, ": ", page, "\n", sep = "")
  url_base <- "https://gamesdonequick.com/tracker/donations/"
  url <- paste0(url_base, event, "?page=", page)

  cat("\n", url, "\n")
  read_html(url) %>%
    html_table() %>%
    extract2(1) %>%
    set_names(c("name", "time", "amount", "comment")) %>%
    mutate(time = mdy_hms(time),
           amount_lbl = amount,
           amount = stringr::str_remove(amount, "\\$"),
           amount = stringr::str_remove(amount, ","),
           amount = as.numeric(amount))
}

get_donations <- function(event = "sgdq2018") {

  pages <- seq_len(get_page_count(event = event))

  donations <- map_df(pages, function(page) {
    get_page(event = event, page = page)
  })

  donations <- donations %>%
    #filter(time > ymd_hms("2018-06-24 00:00:00")) %>%
    arrange(time) %>%
    mutate(day = wday(time, label = TRUE),
           day_num = paste0(day, " (", day(time), ".)"))

  amount_breaks   <- sort(c(0, as.numeric(flatten(map(c(5, 10, 25), ~.x * 10^{0:4})))))
  amount_c_labels <- paste0("<= ", scales::dollar(amount_breaks[-1]))

  donations <- donations %>%
    mutate(amount_c = cut(amount, breaks = amount_breaks, labels = amount_c_labels))

  cat("\nSaving", event, "\n")
  saveRDS(donations, file.path("data", paste0(event, ".rds")))
}


assemble_gdqs <- function(events = NULL) {

  if (is.null(events)) {
    # events <- rev(paste0(c("a", "s"), rep(paste0("gdq", 2011:2018), each = 2)))
    # events <- paste0("data/", events, ".rds")
    events <- list.files("data", "[as]gdq\\d+\\.rds", full.names = TRUE)
  }

  map_df(events, function(x) {
    df <- readRDS(x)
    df$day_num <- as.character(df$day_num)
    df$event <- str_to_upper(str_extract(x, "[as]gdq\\d+"))
    df$year  <- str_extract(x, "\\d+")
    df$gdq   <- str_remove(df$event, "\\d+")
    df
  }) %>%
    arrange(time) %>%
    mutate(day_num = forcats::fct_inorder(day_num, ordered = TRUE))
}

# For documents
library(ggplot2)
library(scales)
library(tadaatoolbox)
library(knitr)
library(ggbeeswarm)
theme_set(theme_tadaa(legend.position = "top"))

## Plot parts
euro_scale <- unit_format("€", sep = "")
euro_axis <- dup_axis(~.*.85, labels = euro_scale)

p_title <- "Games Done Quick: Donation Breakdown"
p_caption <- "gdq.tadaa-data.de\nAll data from gamesdonequick.com/tracker"

amount_breaks   <- sort(c(0, as.numeric(flatten(map(c(5, 10, 25), ~.x * 10^{0:4})))))
amount_c_labels <- paste0("<= ", scales::dollar(amount_breaks[-1]))

## Chunk options
knitr::opts_chunk$set(cache.path = "cache",
                      fig.path = "plots/",
                      fig.retina = 2,
                      error = FALSE,
                      warning = FALSE,
                      message = FALSE)
