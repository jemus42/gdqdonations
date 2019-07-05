# Scraping gdqvods

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
get_gdqvods_runs <- function(event_dates) {

  event_dates %>%
    mutate(
      year = str_extract(event, "\\d{4}"),
      gdq = str_extract(event, "[A-Z]{4}"),
      urlpartial = str_to_lower(paste(gdq, year, sep = "-"))
    ) %>%
    pull(urlpartial) -> urlpartials

  prg <- cli_progress_bar(total = length(urlpartials))

  map_dfr(urlpartials, ~{
    baseurl <- "http://gdqvods.com/event/"
    url <- paste0(baseurl, .x)

    prg$tick()

    tz_offset <- read_html(url) %>%
      html_node("p") %>%
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
        time = hms::as.hms(Time),
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
get_gdq_events <- function() {
  require(rvest)

  eventpage <- read_html("http://gdqvods.com/event/")

  eventnames <- eventpage %>%
    html_nodes(".content .header") %>%
    html_text()

  urlpartials <- eventpage %>%
    html_nodes(".ui .card") %>%
    html_attr("href") %>%
    str_remove("/event/") %>%
    str_remove("/")

  names(urlpartials) <- eventnames
  urlpartials
}
