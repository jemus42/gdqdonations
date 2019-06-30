# Data acquisition
library(cliapp)
source("helpers.R")

events <- rev(tolower(event_dates$event))

# Donations ----
# prg <- cli_progress_bar(total = length(events))
cli_h1("Getting donations...")

walk(events, ~{
  # prg$tick()
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

# Runs ----
prg <- cli_progress_bar(total = length(events))
cli_h1("Getting runs...")

walk(events, ~{
  prg$tick()

  out_file <- paste0("data/runs_", event, ".rds")

  if (file.exists(out_file) | event == "agdq2011") {
    return(tibble())
  }

  get_runs(event = event) %>%
    saveRDS(out_file)
})

cli_alert_success("Got runs!")

# Cache assembled runs dataset
all_runs <- assemble_runs()
saveRDS(all_runs, "data/all_runs.rds")
