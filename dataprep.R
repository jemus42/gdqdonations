# Data acquisition
library(cliapp)
source("00-helpers-gdqtracker.R")
source("00-helpers-gdqvods.R")

events <- rev(tolower(event_dates$event))

# Donations ----
# prg <- cli_progress_bar(total = length(events))
cli_h1("Getting donations...")

walk(events, ~{
  # prg$tick()

  cli_h2("Current event: {toupper(.x)}")

  out_file <- file.path("data", paste0("donations_", .x, ".rds"))

  if (file.exists(out_file) &
      Sys.Date() > event_dates$end[tolower(event_dates$event) == .x]) {
    return(tibble())
  }

  dntns <- get_donations(event = .x)
  saveRDS(object = dntns, file = out_file)

  beepr::beep(2)
})
cli_alert_success("Got donations!")

# Cache assembled donations dataset
all_donations <- assemble_donations()
saveRDS(all_donations, "data/all_donations.rds")

# Runs (GDQ) ----
prg <- cli_progress_bar(total = length(events))
cli_h1("Getting runs from GDQ tracker...")

walk(events, ~{
  prg$tick()

  out_file <- paste0("data/runs_", .x, ".rds")

  if (file.exists(out_file) | .x == "agdq2011") {
    return(tibble())
  }

  get_runs(event = .x) %>%
    saveRDS(out_file)
})

cli_alert_success("Got runs from GDQ tracker!")

# Cache assembled runs dataset
all_runs <- assemble_runs()
saveRDS(all_runs, "data/all_runs_gdqtracker.rds")

# Saving gdqvods runs ----
if (!file.exists("data/all_runs_gdqvods.rds")) {
  cli_h1("Getting runs from gdqvods...")

  gdqvods <- get_gdqvods_runs(event_dates$event)
  saveRDS(gdqvods, "data//gdqvods.com/gdqvods_runs.rds")

  cli_alert_success("Got & saved gdqvods runs!")
}

if (!file.exists("data/gdqvods.com/gdqvods_genres.rds")) {
  cli_h1("Getting run genres from gdqvods...")

  genres <- get_gdqvods_by_genre()
  saveRDS(genres, "data/gdqvods.com/gdqvods_genres.rds")
}

if (!file.exists("data/gdqvods.com/gdqvods_categories.rds")) {
  cli_h1("Getting run categories from gdqvods...")

  categories <- get_gdqvods_by_category()
  saveRDS(categories, "data/gdqvods.com/gdqvods_categories.rds")
}
