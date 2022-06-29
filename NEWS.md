# mlbplotR (development version)

* Added the `geom_mlb_logos()` geom.
* Added the color and fill scales `scale_color_mlb()` and `scale_fill_mlb()` along with the axis scales `scale_x_mlb()` and `scale_y_mlb()` in combination with the theme update functions `theme_x_mlb()` and `theme_y_mlb()`. Also added the ggplot2 theme-element `element_mlb_logo()` which translates MLB team abbreviations into team logos (v0.0.0.9002)
* Added the `geom_mlb_headshots()` geom that plots headshots for valid MLB IDs. Along with the function `ggpreview()` which allows to preview a ggplot in it's actual dimensions. (v0.0.9003)
* Added the ggplot2 theme-element `element_mlb_headshot()` and  which translates player IDs into player headshots. (v0.0.0.9004)
* Added the function `ggpreview()` which allows to preview a ggplot in it's actual dimensions. (v.0.0.9005)
* Added the `geom_from_path()` geom that plots images from urls, local paths, and more. Also added `element_path()` which translates supplied links into images for axes labeling (v.0.0.9006)
* Added the `geom_mlb_scoreboard_logos()`, `geom_mlb_dark_cap_logos()`, `geom_mlb_light_cap_logos()` geoms which translate MLB team abbreviations into team logos. Also added corresponding element functions (`element_mlb_scoreboard_logo()`, `element_mlb_dark_cap_logo()`, `element_mlb_light_cap_logo()`) which translates supplied abbreviations into images for theming. Also updated function parameter names. (v.0.0.9007)
* Added the `load_headshots()` function to the exported functions. (v.0.0.9008)
* Added the `clean_team_abbrs()` function and implemented it with proper geoms. (v.0.0.9009)

