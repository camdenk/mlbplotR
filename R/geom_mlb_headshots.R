#' ggplot2 Layer for Visualizing MLB Player Headshots
#'
#' @description This geom is used to plot MLB player headshots instead
#'   of points in a ggplot. It requires x, y aesthetics as well as a valid MLBAM
#'   id (The same ID associated with their Baseball Savant page).
#'
#' @inheritParams ggplot2::geom_point
#' @section Aesthetics:
#' `geom_mlb_headshots()` understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item{**x**}{ - The x-coordinate.}
#'   \item{**y**}{ - The y-coordinate.}
#'   \item{**player_id**}{ - The players' MLB (Baseball Savant) id.}
#'   \item{`na_headshot_to_logo = TRUE`}{ - Should NA/non-matches return the MLB logo
#'                                          instead of a grayed out blank headshot? Defaults to `TRUE`}
#'   \item{`alpha = NULL`}{ - The alpha channel, i.e. transparency level, as a numerical value between 0 and 1.}
#'   \item{`colour = NULL`}{ - The image will be colorized with this colour. Use the special character `"b/w"` to set it to black and white. For more information on valid colour names in ggplot2 see <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>}
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
#' @export
#' @examples
#' \donttest{
#' library(mlbplotR)
#' library(ggplot2)
#'
#' df <- data.frame(
#'   a = c(rep(1:3, 3), 1.5, 2.5),
#'   b = c(sort(rep(1:3, 3), decreasing = TRUE), 2.5, 2.5),
#'   player_id = c("660670",
#'                 "545361",
#'                 "605141",
#'                 "571448",
#'                 "594798",
#'                 "518692",
#'                 "0",
#'                 "521692",
#'                 "120074",
#'                 "665487",
#'                 "518934"),
#'   player_name = c("Ronald Acuña Jr.",
#'                   "Mike Trout",
#'                   "Mookie Betts",
#'                   "Nolan Arenado",
#'                   "Jacob deGrom",
#'                   "Freddie Freeman",
#'                   "Non Match",
#'                   "Salvador Perez",
#'                   "David Ortiz",
#'                   "Fernando Tatis Jr.",
#'                   "DJ LeMahieu")
#' )
#'
#' # set a custom fill colour for one player
#' df$colour <- ifelse(df$a == 2 & df$b == 2, NA, "b/w")
#'
#' # scatterplot of the headshots
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_mlb_headshots(aes(player_id = player_id), height = 0.2) +
#'   geom_label(aes(label = player_name), nudge_y = -0.35, alpha = 0.5) +
#'   coord_cartesian(xlim = c(0.75, 3.25), ylim = c(0.7, 3.25)) +
#'   theme_void()
#'
#' # apply alpha as constant and use non default na replacement
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_mlb_headshots(aes(player_id = player_id), height = 0.2, alpha = 0.5,
#'                      na_headshot_to_logo = FALSE) +
#'   geom_label(aes(label = player_name), nudge_y = -0.35, alpha = 0.5) +
#'   coord_cartesian(xlim = c(0.75, 3.25), ylim = c(0.7, 3.25)) +
#'   theme_void()
#'
#' # apply colour as an aesthetic
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_mlb_headshots(aes(player_id = player_id, colour = colour), height = 0.2) +
#'   geom_label(aes(label = player_name), nudge_y = -0.35, alpha = 0.5) +
#'   coord_cartesian(xlim = c(0.75, 3.25), ylim = c(0.7, 3.25)) +
#'   scale_colour_identity() +
#'   theme_void()
#'
#' }
geom_mlb_headshots <- function(mapping = NULL, data = NULL,
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
    geom = GeomMLBheads,
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
GeomMLBheads <- ggplot2::ggproto(
  "GeomMLBheads", ggplot2::Geom,
  required_aes = c("x", "y", "player_id"),
  # non_missing_aes = c(""),
  default_aes = ggplot2::aes(
    alpha = NULL, colour = NULL, angle = 0, hjust = 0.5,
    vjust = 0.5, width = 1.0, height = 1.0, na_headshot_to_logo = TRUE
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE, na_headshot_to_logo = TRUE) {
    data <- coord$transform(data, panel_params)

    if (na_headshot_to_logo) {
      grobs <- lapply(seq_along(data$player_id), build_grobs, alpha = data$alpha, colour = data$colour, data = data, type = "logo_headshots")
    } else {
      grobs <- lapply(seq_along(data$player_id), build_grobs, alpha = data$alpha, colour = data$colour, data = data, type = "gray_headshots")
    }



    class(grobs) <- "gList"

    grid::gTree(children = grobs)
  },
  draw_key = function(...) grid::nullGrob()
)
