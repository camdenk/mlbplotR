#' @name geom_milb_logos
#'
#' @title ggplot2 Layer for Visualizing MiLB Team Logos
#'
#' @description `geom_milb_logos()`, `geom_milb_light_cap_logos()`, `geom_milb_dot_logos()` are used to
#'   plot MiLB team instead of points in a ggplot. It requires
#'   x, y aesthetics as well as a valid MiLB team name
#'
#' @inheritParams ggplot2::geom_point
#' @section Aesthetics:
#' `geom_milb_logos()`, `geom_milb_light_cap_logos()`, `geom_milb_dot_logos()` understand the following aesthetics (required aesthetics are in bold):
#' \describe{
#'   \item{**x**}{ - The x-coordinate.}
#'   \item{**y**}{ - The y-coordinate.}
#'   \item{**team_name**}{ - The team name. Need to use the full team name.}
#'   \item{`alpha = NULL`}{ - The alpha channel, i.e. transparency level, as a numerical value between 0 and 1.}
#'   \item{`colour = NULL`}{ - The image will be colourized with this colour. Use the special character `"b/w"` to set it to black and white. For more information on valid colour names in ggplot2 see <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>}
#'   \item{`angle = 0`}{ - The angle of the image as a numerical value between 0° and 360°.}
#'   \item{`hjust = 0.5`}{ - The horizontal adjustment relative to the given x coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`vjust = 0.5`}{ - The vertical adjustment relative to the given y coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`height = 1.0`}{ - The desired height of the image in `npc` (Normalised Parent Coordinates).
#'                            The default value is set to 1.0 which is *big* but it is necessary
#'                            because all used values are computed relative to the default.
#'                            A typical size is `height = 0.1` (see below examples).
#'                            For cap logos, the scaling works better when adjusting height and not width.}
#'   \item{`width = 1.0`}{ - The desired width of the image in `npc` (Normalised Parent Coordinates).
#'                           The default value is set to 1.0 which is *big* but it is necessary
#'                           because all used values are computed relative to the default.
#'                           A typical size is `height = 0.075` (see below examples).
#'                           For cap logos, the scaling works better when adjusting height and not width.}
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
#' team_names <- c("Kannapolis Cannon Ballers", "Charlotte Knights",
#'                 "Bowie Baysox", "Durham Bulls", "Montgomery Biscuits", "Las Vegas Aviators",
#'                 "Lehigh Valley IronPigs", "Richmond Flying Squirrels", "Round Rock Express",
#'                 "Frisco RoughRiders", "Hickory Crawdads", "Down East Wood Ducks")
#'
#' df <- data.frame(
#'   a = rep(1:4, 3),
#'   b = sort(rep(1:3, 4), decreasing = TRUE),
#'   teams = team_names
#' )
#'
#' # keep alpha == 1 for all teams including an "A"
#' matches <- grepl("A|a", team_names)
#' df$alpha <- ifelse(matches, 1, 0.2)
#' # also set a custom fill colour for the non "A" teams
#' df$colour <- ifelse(matches, NA, "gray")
#'
#' # scatterplot of all logos
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_milb_logos(aes(team_name = teams), height = 0.1) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   theme_void()
#'
#'
#' # apply alpha and colour via an aesthetic from inside the dataset `df`
#' # please note that you have to add scale_alpha_identity() as well as
#' # scale_colour_identity() to use the alpha and colour values in your dataset!
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_milb_light_cap_logos(aes(team_name = teams, alpha = alpha, colour = colour), height = 0.1) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   scale_alpha_identity() +
#'   scale_colour_identity() +
#'   theme_void()
#'
#' # apply alpha as constant for all logos
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_milb_dot_logos(aes(team_name = teams), height = 0.15, alpha = 0.6) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   theme_void()
#'
#' }
NULL

#' @rdname geom_milb_logos
#' @export
geom_milb_logos <- function(mapping = NULL, data = NULL,
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
    geom = GeomMiLBlogo,
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
GeomMiLBlogo <- ggplot2::ggproto(
  "GeomMiLBlogo", ggplot2::Geom,
  required_aes = c("x", "y", "team_name"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)

    grobs <- lapply(seq_along(data$team_name), build_grobs, alpha = data$alpha, colour = data$colour, data = data, type = "milb_logo")

    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)










#' @rdname geom_milb_logos
#' @export
geom_milb_light_cap_logos <- function(mapping = NULL, data = NULL,
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
    geom = GeomMiLBlightcaplogo,
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
GeomMiLBlightcaplogo <- ggplot2::ggproto(
  "GeomMiLBlightcaplogo", ggplot2::Geom,
  required_aes = c("x", "y", "team_name"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)

    grobs <- lapply(seq_along(data$team_name), build_grobs, alpha = data$alpha, colour = data$colour, data = data, type = "milb_light_cap")

    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)






#' @rdname geom_milb_logos
#' @export
geom_milb_dot_logos <- function(mapping = NULL, data = NULL,
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
    geom = GeomMiLBdotlogo,
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
GeomMiLBdotlogo <- ggplot2::ggproto(
  "GeomMiLBdotlogo", ggplot2::Geom,
  required_aes = c("x", "y", "team_name"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    data <- coord$transform(data, panel_params)

    grobs <- lapply(seq_along(data$team_name), build_grobs, alpha = data$alpha, colour = data$colour, data = data, type = "milb_dot")

    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)
