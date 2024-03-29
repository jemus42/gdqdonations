# For documents ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(hrbrthemes)
library(firasans)
library(kableExtra)
library(ggbeeswarm)
library(purrr)
library(stringr)
library(lubridate)
library(forcats)

theme_gdqd <- function(grid = "Yy", legend.position = "top", ...) {
  theme_ipsum_fsc(
    grid = grid,
    plot_margin = margin(5, 5, 5, 5)
  ) +
    theme(
      legend.position = legend.position,
      panel.grid.major = element_line(colour = "#aaaaaa"),
      ...
    )
}

theme_set(
 theme_gdqd()
)

## Plot parts ----
euro_scale <- unit_format(
  suffix = "€", sep = "", big.mark = ".",
  decimal.mark = ",", accuracy = 4
)
euro_axis <- function(...) dup_axis(~.*.89, labels = euro_scale, name = NULL, ...)

p_title <- "Games Done Quick: Donation Breakdown"
p_title_r <- "Games Done Quick: Runs"
p_title_runners <- "Games Done Quick: Runners"

p_caption <- glue::glue("Donation data from gamesdonequick.com/tracker, ",
                        "run data from gdqvods.com\n",
                        "@jemus42 – gdq.tadaa-data.de")

# Setting/overriding ggplot2 components ----
labs <- partial(
  ggplot2::labs,
  caption = p_caption
)

minilabs <- partial(
  ggplot2::labs,
  caption = p_caption, y = "", x = "", fill = "", color = ""
)

scale_x_year <- partial(
  scale_x_continuous,
  breaks = seq(0, 3e3, 1),
  minor_breaks = NULL,
  name = ""
)

scale_y_currency <- partial(
  scale_y_continuous,
  labels = dollar_format(),
  sec.axis = euro_axis(),
  name = ""
)

scale_y_dollar <- partial(
  scale_y_continuous,
  labels = dollar_format(),
  name = ""
)

scale_x_dollar <- partial(
  scale_x_continuous,
  labels = dollar_format(),
  name = ""
)

scale_colorfill_gdq <- partial(
  scale_fill_manual,
  values = c("AGDQ" = "#377EB8", "SGDQ" = "#E41A1C"),
  aesthetics = c("color", "fill"),
  name = ""
)

## Chunk options ----
knitr::opts_chunk$set(
  cache.path = "cache",
  fig.path = "plots/",
  fig.retina = 2,
  error = FALSE,
  warning = FALSE,
  message = FALSE
)
