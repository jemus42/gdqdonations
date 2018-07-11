#! /usr/bin/env Rscript

rmarkdown::render_site(".")

library(slackr)
slackr_setup(config_file = "/opt/tadaadata/.slackr")

msg <- paste0(lubridate::now(tzone = "CET"), ": Built https://gdq.tadaa-data.de")
text_slackr(msg, channel = "#gdq", username = "tadaabot", preformatted = FALSE)
