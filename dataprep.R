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

read_html("https://gamesdonequick.com/tracker/runs/") %>%
  html_table() %>%
  extract2(1) %>%
  set_names(c("run", "players", "description", "run_start", "run_end", "bidwars")) %>%
  mutate(run_start = mdy_hms(run_start),
         run_end = mdy_hms(run_end),
         run_duration_s = as.numeric(difftime(run_end, run_start, units = "secs")),
         run_duration_hms = hms::hms(seconds = run_duration_s)) %>%
  arrange(run_start) %>%
  filter(!(run %in% c("Bonus Stream", "Preshow"))) %>%
  saveRDS("data/runs.rds")
