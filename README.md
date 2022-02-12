
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlbplotR <a><img src='man/figures/logo.png' align="right" width="25%" min-width="120px" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg?style=flat-square)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

The code for this package was copied heavily from
[nflplotR](https://nflverse.github.io/nflplotR/index.html) with minor
changes to support Major League Baseball logos.

The goal of mlbplotR is to provide functions and geoms that help with
visualizations of MLB related analysis. It provides ggplot2 geoms that
do the heavy lifting of plotting MLB logos in high quality, with correct
aspect ratio, and possible transparency.

## Installation

mlbplotR is currently not on [CRAN](https://CRAN.R-project.org) but you
can get the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("camdenk/mlbplotR")
```

## Example

This is a basic example with [Baseball
Reference](https://baseball-reference.com) data which compares ERA to
FIP:

``` r
library(mlbplotR)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

teams_colors_logos <- mlbplotR::load_mlb_teams()

df <- readr::read_csv("./data-raw/2021-Team-Pitching-Stats.csv")
  
# Join leaderboard with abbrevations
joined_df <- df %>% 
  left_join(teams_colors_logos, by = c("Tm" = "team_name"))


joined_df %>% 
  ggplot2::ggplot(aes(x = ERA, y = FIP)) +
  mlbplotR::geom_mlb_logos(aes(team_savant_abbr = team_savant_abbr), width = 0.075, alpha = 0.7) +
  ggplot2::labs(
    caption = "Data: Baseball Reference",
    title = "2021: ERA vs. FIP"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold")
  ) +
  ggplot2::scale_x_reverse(breaks = scales::pretty_breaks(), expand = c(.1, .1)) +
  ggplot2::scale_y_reverse(breaks = scales::pretty_breaks(), expand = c(.1, .1))
```

<img src="man/figures/README-scatter-example-1.png" width="100%" />

Here’s another that looks at Home Runs Allowed by team:

``` r
joined_df %>% 
  ggplot2::ggplot(aes(x = team_savant_abbr, y = HR)) +
  ggplot2::geom_col(aes(color = team_savant_abbr, fill = team_savant_abbr), width = 0.5) +
  mlbplotR::geom_mlb_logos(aes(team_savant_abbr = team_savant_abbr), width = 0.075, alpha = 0.9) +
  mlbplotR::scale_color_mlb(type = "secondary") +
  mlbplotR::scale_fill_mlb(alpha = 0.4) +
  ggplot2::labs(
    caption = "Data: Baseball Reference",
    title = "2021: Home Runs Allowed"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank()
  ) +
  ggplot2::scale_x_discrete(expand = c(0.075, 0.075))
```

<img src="man/figures/README-bar-example-1.png" width="100%" />

Lastly, here’s an example using Baseball Savant Statcast data:

``` r
library(qs)

BAL_2021 <- qread("./data-raw/BAL-2021.qs") %>% 
  dplyr::mutate(
    fielding_team = ifelse(inning_topbot == "Bot", away_team, home_team),
    batting_team = ifelse(inning_topbot == "Bot", home_team, away_team)
  )

Team_FB_Rate <- BAL_2021 %>%
  dplyr::mutate(fastball = ifelse(pitch_type %in% c("FF", "FA", "SI", "FT", "FC"), 1, 0)) %>% 
  dplyr::group_by(fielding_team) %>%
  dplyr::summarise(n = n(), fb_per = mean(fastball, na.rm = TRUE))

Team_FB_Rate %>% 
  ggplot2::ggplot(aes(x = fielding_team, y = fb_per)) +
  ggplot2::geom_col(aes(color = fielding_team, fill = fielding_team), width = 0.5) +
  mlbplotR::scale_color_mlb(type = "secondary") +
  mlbplotR::scale_fill_mlb(alpha = 0.4) +
  ggplot2::labs(
    title = "2021 Fastball Percentage in Orioles Games",
    y = "Fastball %"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # this line triggers the replacement of team abbreviations with logos
    axis.text.x = element_mlb_logo()
  )
```

<img src="man/figures/README-statcast-example-1.png" width="100%" />

## Contributing

Many hands make light work! Here are some ways you can contribute to
this project:

-   You can [open an
    issue](https://github.com/camdenk/mlbplotR/issues/new/choose) if
    you’d like to request specific data or report a bug/error.

## To Do

-   Add ability to set axis labels to be logos
-   Add functions to automatically set theme
-   Add in player headshots (will likely have to wait until after the
    lockout)
