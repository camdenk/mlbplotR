library(hexSticker)
library(tidyverse)
library(mlbplotR)
library(ggplot2)
library(svglite)

team_abbr <- valid_team_names()
# remove conference logos from this example
team_abbr <- team_abbr[!team_abbr %in% c("AL", "NL", "MLB")]

x_loc = c(3, 4, 1:6, 1, 6, rep(1:6,3), 3, 4)
y_loc = c(7, 7, rep(6:6, 6), 5, 5, sort(rep(2:4, 6), decreasing = TRUE), 1, 1)

df <- data.frame(
  a = x_loc,
  b = y_loc,
  teams = team_abbr
)

p <- ggplot(df, aes(x = a, y = b)) +
  geom_mlb_logos(aes(team_abbr = teams), width = 0.08, alpha = 0.75) +
  coord_cartesian(xlim = c(0.5,6.5), ylim = c(0.5,8.5)) +
  theme_void() +
  theme_transparent()

sticker(
  p,
  package = "mlbplotR",
  p_y = 1.25,
  p_size = 6,
  p_color = "white",
  s_x = 1,
  s_y = 1.125,
  s_width = 1.9,
  s_height = 2,
  spotlight = TRUE,
  l_y = 1.25,
  l_alpha = 0.2,
  l_width = 5,
  l_height = 5,
  h_fill = "#575757",
  h_color = "#404040",
  h_size = 0.8,
  filename = "data-raw/logo.svg",
  dpi = 600
)

#usethis::use_logo("./data-raw/logo.png")
