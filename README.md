
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

## Usage

Let’s plot every team on a grid with some extra customization:

``` r
library(mlbplotR)
library(ggplot2)
library(dplyr)

teams_colors_logos <- mlbplotR::load_mlb_teams() |> 
  dplyr::filter(!team_abbr %in% c("AL", "NL", "MLB")) |> 
  dplyr::mutate(
    a = rep(1:6, 5),
    b = sort(rep(1:5, 6), decreasing = TRUE),
    alpha = ifelse(grepl("A", team_abbr), 1, 0.75), # Keep alpha == 1 for teams that have an "A"
    color = ifelse(grepl("E", team_abbr), "b/w", NA) # Set teams that have an "E" to black & white
  )


 ggplot2::ggplot(teams_colors_logos, aes(x = a, y = b)) +
   mlbplotR::geom_mlb_logos(aes(team_abbr = team_abbr, color = color, alpha = alpha), width = 0.075) +
   ggplot2::geom_label(aes(label = team_abbr), nudge_y = -0.35, alpha = 0.5) +
   ggplot2::scale_color_identity() +
   ggplot2::scale_alpha_identity() +
   ggplot2::theme_void() 
```

<img src="man/figures/README-every-team-1.png" width="100%" />

There is a [getting started
guide](https://camdenk.github.io/mlbplotR/articles/mlbplotR.html) that
will walk you through more use cases for the package.

## Contributing

Many hands make light work! Here are some ways you can contribute to
this project:

-   You can [open an
    issue](https://github.com/camdenk/mlbplotR/issues/new/choose) if
    you’d like to request specific data or report a bug/error.

## To Do

-   Create more package tutorials
-   Add in mean/median line geoms
-   Add tests
-   Continue to add player ids for headshots for those who haven’t
    played in the Statcast era
