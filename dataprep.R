# Data acquisition

source("helpers.R")

events <- rev(paste0(c("a", "s"), rep(paste0("gdq", 2011:2018), each = 2)))

for (event in events) {

  if (file.exists(paste0("data/", event, ".rds"))) {
    cat("Skipping ", event)
    next
  }

  print(event)
  get_donations(event = event)
  beepr::beep()
}


# Runs ----

for (event in events) {

  path <- paste0("data/runs_", event, ".rds")

  if (file.exists(path)) {
    cat("Skipping ", event)
    next
  }

  print(event)
  get_runs(event = event) %>%
  saveRDS(path)

  beepr::beep()
}

