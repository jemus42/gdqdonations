# Scraping gdqvods

# Setting a user agent for good practice
httr::set_config(httr::user_agent("gdq@tadaa-data.de; +https://gdq.tadaa-data.de/"))

# gdqvods base url
gdqvods_path <- function(...) {
  httr::modify_url("http://old.gdqvods.com/", path = c(...))
}

# gdqvods_path("category")

# Timezone extraction ----
us_tz_to_utz_offset <- function(tzname) {
  # https://www.timeanddate.com/time/zone/usa
  tzones <- tibble::tribble(
    ~offset, ~tzname, ~tzname_long,
    "-10", "HST",  "Hawaii Standard Time",
    "-08", "AKDT", "Alaska Daylight Time",
    "-07", "PDT",  "Pacific Daylight Time",
    "-07", "MST",  "Mountain Standard Time",
    "-06", "MDT",  "Mountain Daylight Time",
    "-05", "CDT",  "Central Daylight Time",
    "-04", "EDT",  "Eastern Daylight Time",
    "-08", "PST",  "Pacific Standard Time",
    "-06", "CST",  "Central Standard Time",
    "-05", "EST",  "Eastern Standard Time",
  )

  purrr::map_chr(tzname, ~{
    tzones$offset[tzones$tzname == .x]
  })
}

# Get mainline GDQ event runs from gdqvods.com ----
get_gdqvods_runs <- function(events) {

  # event_dates %>%
  #   mutate(
  #     year = str_extract(event, "\\d{4}"),
  #     gdq = str_extract(event, "[A-Z]{4}"),
  #     urlpartial = str_to_lower(paste(gdq, year, sep = "-"))
  #   ) %>%
  #   pull(urlpartial) -> urlpartials

  # "events" are "AGDQ2011" etc, i.e. "?GDQ20??"
  urlpartials <- events %>%
    str_replace("GDQ", "GDQ-") %>%
    str_to_lower()

  prg <- cli_progress_bar(total = length(urlpartials))

  map_dfr(urlpartials, ~{
    url <- gdqvods_path("event", .x)

    prg$tick()

    tz_offset <- read_html(url) %>%
      html_nodes("p") %>%
      html_text() %>%
      str_extract("are shown in [A-Z]*") %>%
      str_extract("\\w+$") %>%
      us_tz_to_utz_offset()

    read_html(url) %>%
      html_node(".table") %>%
      html_table() %>%
      as_tibble() %>%
      transmute(
        event = str_to_upper(str_remove(.x, "-")),
        run_start = `Start Time`,
        run = Game,
        platform = Platform,
        category = Category,
        players = Runners,
        run_time = hms::as_hms(Time),
        run_start = paste(str_extract(event, "\\d{4}"), run_start, tz_offset),
        run_start = parse_date_time(run_start, orders = "Y a, b d I:M p z"),
        run_start = with_tz(run_start, tzone = "UTC"),
        gdq = str_extract(event, "[A-Z]{4}"),
        year = str_extract(event, "\\d{4}"),
        game_year = str_extract(run, "\\(\\d{4}\\)$") %>% str_extract("\\d+") %>% as.numeric(),
        game_decade = as.character((game_year %/% 10) * 10)
      )
  })
}

# Get all GDQ event names + url partials ----
# Turns out it works for other subpages
get_gdqvods_subpage <- function(page = c("event", "category", "genre")) {
  require(rvest)
  require(stringr)
  page <- match.arg(page)

  eventpage <- gdqvods_path(page) %>%
    read_html()

  eventnames <- eventpage %>%
    html_nodes(".content .header") %>%
    html_text() %>%
    str_subset("^All", negate = TRUE)

  urlpartials <- eventpage %>%
    html_nodes(".ui .card") %>%
    html_attr("href") %>%
    str_remove(paste0("/", page, "/")) %>%
    str_remove("/") %>%
    str_subset(pattern = "^all", negate = TRUE)

  names(urlpartials) <- eventnames
  urlpartials
}

# gdqvods categories ----
get_gdqvods_by_category <- function() {
  urls <- get_gdqvods_subpage(page = "category")
  prg <- cli_progress_bar(total = length(urls))

  map_dfr(urls, ~{
    url <- gdqvods_path("category", .x)

    prg$tick()

    read_html(url) %>%
      html_node(".table") %>%
      html_table() %>%
      as_tibble() %>%
      transmute(
        event = str_to_upper(str_remove(Event, " ")),
        run = Game,
        platform = Platform,
        category = Category,
        players = Runners,
        run_time = hms::as_hms(Time),
        gdq = str_extract(event, "[A-Z]{4}"),
        year = str_extract(event, "\\d{4}"),
        game_year = str_extract(run, "\\(\\d{4}\\)$") %>% str_extract("\\d+") %>% as.numeric(),
        game_decade = as.character((game_year %/% 10) * 10)
      )
  }, .id = "subcategory")
}

# gdqvods genres ----
get_gdqvods_by_genre <- function() {
  urls <- get_gdqvods_subpage(page = "genre")
  prg <- cli_progress_bar(total = length(urls))

  map_dfr(urls, ~{
    url <- gdqvods_path("genre", .x)

    prg$tick()

    read_html(url) %>%
      html_node(".table") %>%
      html_table() %>%
      as_tibble() %>%
      transmute(
        event = str_to_upper(str_remove(Event, " ")),
        run = Game,
        platform = Platform,
        category = Category,
        players = Runners,
        run_time = hms::as_hms(Time),
        gdq = str_extract(event, "[A-Z]{4}"),
        year = str_extract(event, "\\d{4}"),
        game_year = str_extract(run, "\\(\\d{4}\\)$") %>% str_extract("\\d+") %>% as.numeric(),
        game_decade = as.character((game_year %/% 10) * 10)
      )
  }, .id = "genre")
}
