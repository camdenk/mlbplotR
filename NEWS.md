# mlbplotR 1.2.0

* Add `load_milb_teams()` with Minor League data (v1.1.0.9001)
* Added ability to use dot headshots with `geom_mlb_dot_headshots()`, `gt_fmt_mlb_dot_headshot()`, and extra functionality within preexisting functions (v1.1.0.9002)
* Update AZ logos and team colors (v1.1.0.9003)
* Update `gt_*` functions to be used in gt row group labels and accept unquoted column names, add `gt_fmt_milb_logo()` (v1.1.0.9004)
* Add `gt_` and `geom_` functions for MiLB dot team logos and headshots (v1.1.0.9005)
* Documentation updates (v1.1.0.9006)
* Added "ATH" -> "OAK" in `clean_team_abbrs()` (v1.1.0.9007)
* Flipped "OAK" -> "ATH" in `clean_team_abbrs()` and had `gt_fmt_mlb_dot_logo` return the MLB dot logo if there's a mismatch (v1.1.0.9008)
* Updated documentation. Rewrite `geom_mlb_headshots()` and `gt_fmt_mlb_headshot()` to use MLB headshots, not ESPN, which makes the parameter `na_headshot_to_logo` unnecessary. Deprecate `load_headshots()` (v1.1.0.9009)

# mlbplotR 1.1.0

* Corrected Arizona's team abbreviation in `mlb_team_factor()` (v1.0.1.9001)
* Updated Twins logo, updated Brewers Scoreboard Logo, added more names to be cleaned (ANA to LAA and LA to LAD), added new dot logos and corresponding functions (`geom_mlb_dot_logos()`, `gt_fmt_mlb_dot_logo()`, and `element_mlb_dot_logo()`) (v1.0.1.9002)
* Updated `valid_team_names()` to allow for truncated outputs without league level abbreviations (v1.0.1.9003)

# mlbplotR 1.0.1

* Added the `geom_mlb_logos()` geom.
* Added the color and fill scales `scale_color_mlb()` and `scale_fill_mlb()` along with the axis scales `scale_x_mlb()` and `scale_y_mlb()` in combination with the theme update functions `theme_x_mlb()` and `theme_y_mlb()`. Also added the ggplot2 theme-element `element_mlb_logo()` which translates MLB team abbreviations into team logos (v0.0.0.9002)
* Added the `geom_mlb_headshots()` geom that plots headshots for valid MLB IDs. Along with the function `ggpreview()` which allows to preview a ggplot in it's actual dimensions. (v0.0.0.9003)
* Added the ggplot2 theme-element `element_mlb_headshot()` and  which translates player IDs into player headshots. (v0.0.0.9004)
* Added the function `ggpreview()` which allows to preview a ggplot in it's actual dimensions. (v0.0.0.9005)
* Added the `geom_from_path()` geom that plots images from urls, local paths, and more. Also added `element_path()` which translates supplied links into images for axes labeling (v0.0.0.9006)
* Added the `geom_mlb_scoreboard_logos()`, `geom_mlb_dark_cap_logos()`, `geom_mlb_light_cap_logos()` geoms which translate MLB team abbreviations into team logos. Also added corresponding element functions (`element_mlb_scoreboard_logo()`, `element_mlb_dark_cap_logo()`, `element_mlb_light_cap_logo()`) which translates supplied abbreviations into images for theming. Also updated function parameter names. (v0.0.0.9007)
* Added the `load_headshots()` function to the exported functions. (v0.0.0.9008)
* Added the `clean_team_abbrs()` function and implemented it with proper geoms. (v0.0.0.9009)
* Added the `gt_fmt_mlb_logo()`, `gt_fmt_mlb_scoreboard_logo()`, `gt_fmt_mlb_headshot()` and `gt_merge_stack_team_color()` functions to aid in making gt tables. (v0.0.0.9010)
* Updated internal data to use "AZ" as Diamondbacks new team abbreviation. (v0.0.0.9011)
* Removed `glue` dependency, updated "Getting Started" vignette, and set `scale_x_mlb()`, `scale_y_mlb()`, `theme_x_mlb()`, and `theme_y_mlb()` as superseded (v0.0.0.9012)
* Updated gt helper functions to prepare for an eventual CRAN submission (v0.0.0.9013)
* Updated `na_headshot()`, `gt_fmt_mlb_headshot()`, and `geom_mlb_headshots()` to allow for use of a general gray headshot instead of just returning the MLB logo (v0.0.0.9014)
* Added `gt_mlb_column_labels()` to easily replace column labels with images (v0.0.0.9015)
* Reworked `gt_mlb_column_labels()` to more cleanly accept customization (v0.0.0.9016)
* Added `mlb_team_tiers()` and `mlb_player_tiers()` (v0.0.0.9017)
* Added `mlb_team_factor()` (v0.0.0.9018)
* Added `geom_mean_lines()` and `geom_median_lines()` (v0.0.0.9019)
* Added ability to use `nudge_x` and `nudge_y` to the geom family of functions and cleaned up documentation to prepare for CRAN release (v0.0.0.9020)
