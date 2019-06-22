# Data acquisition

source("helpers.R")

events <- rev(paste0(c("a", "s"), rep(paste0("gdq", 2011:2018), each = 2)))
events <- c("agdq2019", events)

# Donations ----

for (event in events) {

  if (file.exists(paste0("data/", event, ".rds"))) {
    cat("Skipping: ", event, "\n")
    next
  }

  print(event)
  get_donations(event = event)
  beepr::beep()
}

gdq <- left_join(
  assemble_gdqs(),
  event_dates,
  by = "event"
) %>%
  group_by(event) %>%
  mutate(time_rel = start %--% time / dhours(1) / (event_duration * 24))

saveRDS(gdq, "data/gdq.rds")

# Runs ----

for (event in events) {

  path <- paste0("data/runs_", event, ".rds")

  if (file.exists(path) | event == "agdq2011") {
    cat("Skipping: ", event, "\n")
    next
  }

  print(event)
  get_runs(event = event) %>%
  saveRDS(path)

  beepr::beep()
}

