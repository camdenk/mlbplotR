#' @name geom_mlb_logos
#'
#' @title
#' ggplot2 Layer for Visualizing MLB Team Logos
#'
#' @description `geom_mlb_logos()`, `geom_mlb_scoreboard_logos()`, and `geom_mlb_dot_logos()` are used to
#'   plot MLB team and league logos instead of points in a ggplot. It requires
#'   x, y aesthetics as well as a valid MLB team abbreviation. The latter can be
#'   checked with [`valid_team_names()`] but is also cleaned before being plotted.
#'
#' @inheritParams ggplot2::geom_point
#' @section Aesthetics:
#' `geom_mlb_logos()`, `geom_mlb_scoreboard_logos()`, and `geom_mlb_dot_logos()` understand the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item{**x**}{ - The x-coordinate.}
#'   \item{**y**}{ - The y-coordinate.}
#'   \item{**team_abbr**}{ - The team abbreviation. Need to use Savant's abbreviation.}
#'   \item{`alpha = NULL`}{ - The alpha channel, i.e. transparency level, as a numerical value between 0 and 1.}
#'   \item{`colour = NULL`}{ - The image will be colourized with this colour. Use the special character `"b/w"` to set it to black and white. For more information on valid colour names in ggplot2 see <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>}
#'   \item{`angle = 0`}{ - The angle of the image as a numerical value between 0° and 360°.}
#'   \item{`hjust = 0.5`}{ - The horizontal adjustment relative to the given x coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`vjust = 0.5`}{ - The vertical adjustment relative to the given y coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`width = 1.0`}{ - The desired width of the image in `npc` (Normalised Parent Coordinates).
#'                           The default value is set to 1.0 which is *big* but it is necessary
#'                           because all used values are computed relative to the default.
#'                           A typical size is `width = 0.075` (see below examples).}
#'   \item{`height = 1.0`}{ - The desired height of the image in `npc` (Normalised Parent Coordinates).
#'                            The default value is set to 1.0 which is *big* but it is necessary
#'                            because all used values are computed relative to the default.
#'                            A typical size is `height = 0.1` (see below examples).}
#' }
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value. See the below
#'   section "Aesthetics" for a full list of possible arguments.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#'   Cannot be jointly specified with `position`.
#' @return A ggplot2 layer ([ggplot2::layer()]) that can be added to a plot
#'   created with [ggplot2::ggplot()].
#' @examples
#' \donttest{
#' library(mlbplotR)
#' library(ggplot2)
#'
#' team_abbr <- valid_team_names()
#' # remove conference logos from this example
#' team_abbr <- team_abbr[!team_abbr %in% c("NL", "AL", "MLB")]
#'
#' df <- data.frame(
#'   a = rep(1:6, 5),
#'   b = sort(rep(1:5, 6), decreasing = TRUE),
#'   teams = team_abbr
#' )
#'
#' # keep alpha == 1 for all teams including an "A"
#' matches <- grepl("A", team_abbr)
#' df$alpha <- ifelse(matches, 1, 0.2)
#' # also set a custom fill colour for the non "A" teams
#' df$colour <- ifelse(matches, NA, "gray")
#'
#' # scatterplot of all logos
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_mlb_logos(aes(team_abbr = teams), width = 0.075) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   theme_void()
#'
#' # apply alpha via an aesthetic from inside the dataset `df`
#' # please note that you have to add scale_alpha_identity() to use the alpha
#' # values in your dataset!
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_mlb_scoreboard_logos(aes(team_abbr = teams, alpha = alpha), width = 0.075) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   scale_alpha_identity() +
#'   theme_void()
#'
#' # apply alpha and colour via an aesthetic from inside the dataset `df`
#' # please note that you have to add scale_alpha_identity() as well as
#' # scale_colour_identity() to use the alpha and colour values in your dataset!
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_mlb_logos(aes(team_abbr = teams, alpha = alpha, colour = colour), width = 0.075) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   scale_alpha_identity() +
#'   scale_colour_identity() +
#'   theme_void()
#'
#' # apply alpha as constant for all logos
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_mlb_dot_logos(aes(team_abbr = teams), width = 0.075, alpha = 0.6) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   theme_void()
#'
#' # it's also possible to plot league logos
#' league <- data.frame(a = 1:3, b = 0, teams = c("AL", "NL", "MLB"))
#' ggplot(league, aes(x = a, y = b)) +
#'   geom_mlb_logos(aes(team_abbr = teams), width = 0.3) +
#'   geom_label(aes(label = teams), nudge_y = -0.4, alpha = 0.5) +
#'   coord_cartesian(xlim = c(0.5,3.5), ylim = c(-0.75,.75)) +
#'   theme_void()
#' }
NULL

#' @rdname geom_mlb_logos
#' @export
geom_mlb_logos <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           nudge_x = 0,
                           nudge_y = 0,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied",
        "i" = "Only use one approach to alter the position"
      ))
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMLBlogo,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname mlbplotR-package
#' @export
GeomMLBlogo <- ggplot2::ggproto(
  "GeomMLBlogo", ggplot2::Geom,
  required_aes = c("x", "y", "team_abbr"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)

    data$team_abbr <- clean_team_abbrs(as.character(data$team_abbr), keep_non_matches = FALSE)

    grobs <- lapply(seq_along(data$team_abbr), build_grobs, alpha = data$alpha, colour = data$colour, data = data, type = "teams")

    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)



#' @rdname geom_mlb_logos
#' @export
geom_mlb_scoreboard_logos <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           nudge_x = 0,
                           nudge_y = 0,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = TRUE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied",
        "i" = "Only use one approach to alter the position"
      ))
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMLBscoreboardlogo,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname mlbplotR-package
#' @export
GeomMLBscoreboardlogo <- ggplot2::ggproto(
  "GeomMLBscoreboardlogo", ggplot2::Geom,
  required_aes = c("x", "y", "team_abbr"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)

    data$team_abbr <- clean_team_abbrs(as.character(data$team_abbr), keep_non_matches = FALSE)

    grobs <- lapply(seq_along(data$team_abbr), build_grobs, alpha = data$alpha, colour = data$colour, data = data, type = "scoreboard")

    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)




#' @rdname geom_mlb_logos
#' @export
geom_mlb_dot_logos <- function(mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               ...,
                               nudge_x = 0,
                               nudge_y = 0,
                               na.rm = FALSE,
                               show.legend = FALSE,
                               inherit.aes = TRUE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied",
        "i" = "Only use one approach to alter the position"
      ))
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMLBdotlogo,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname mlbplotR-package
#' @export
GeomMLBdotlogo <- ggplot2::ggproto(
  "GeomMLBdotlogo", ggplot2::Geom,
  required_aes = c("x", "y", "team_abbr"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)

    data$team_abbr <- clean_team_abbrs(as.character(data$team_abbr), keep_non_matches = FALSE)

    grobs <- lapply(seq_along(data$team_abbr), build_grobs, alpha = data$alpha, colour = data$colour, data = data, type = "dot")

    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)
