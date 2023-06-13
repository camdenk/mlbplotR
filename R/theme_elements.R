#' Theme Elements for Image Grobs
#'
#' @description
#' In conjunction with the [ggplot2::theme] system, the following `element_`
#' functions enable images in non-data components of the plot, e.g. axis text.
#'
#'   - `element_mlb_logo()`, `element_mlb_scoreboard_logo()`, `element_mlb_dot_logo()`: draws MLB team logos instead of their abbreviations.
#'   - `element_mlb_dark_cap_logo()` and `element_mlb_light_cap_logo()`: draws MLB team cap logos instead of their abbreviations.
#'   - `element_mlb_headshot()`: draws MLB player headshots instead of their MLB IDs
#'   - `element_path()`: draws images from valid image URLs instead of the URL.
#'
#' @details The elements translate MLB team abbreviations or MLB player IDs
#'   into logo images or headshots, respectively.
#' @param alpha The alpha channel, i.e. transparency level, as a numerical value
#'   between 0 and 1.
#' @param colour,color The image will be colorized with this color. Use the
#'   special character `"b/w"` to set it to black and white. For more information
#'   on valid color names in ggplot2 see
#'   <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>.
#' @param hjust,vjust The horizontal and vertical adjustment respectively.
#'   Must be a numerical value between 0 and 1.
#' @param size The output grob size in `cm` (!).
#' @seealso [geom_mlb_logos()], [geom_mlb_headshots()], and [geom_from_path()]
#'   for more information on valid team abbreviations, player ids, and other
#'   parameters.
#' @return An S3 object of class `element`.
#' @examples
#' \donttest{
#' library(mlbplotR)
#' library(ggplot2)
#'
#' team_abbr <- valid_team_names()
#' # remove league logos from this example
#' team_abbr <- team_abbr[!team_abbr %in% c("AL", "NL", "MLB")]
#'
#' df <- data.frame(
#'   random_value = runif(length(team_abbr), 0, 1),
#'   teams = team_abbr
#' )
#'
#' # use logos for x-axis
#' ggplot(df, aes(x = teams, y = random_value)) +
#'   geom_col(aes(color = teams, fill = teams), width = 0.5) +
#'   scale_color_mlb(type = "secondary") +
#'   scale_fill_mlb(alpha = 0.4) +
#'   theme_minimal() +
#'   theme(axis.text.x = element_mlb_logo())
#'
#' # use logos for y-axis
#' ggplot(df, aes(y = teams, x = random_value)) +
#'   geom_col(aes(color = teams, fill = teams), width = 0.5) +
#'   scale_color_mlb(type = "secondary") +
#'   scale_fill_mlb(alpha = 0.4) +
#'   theme_minimal() +
#'   theme(axis.text.y = element_mlb_logo())
#'
#' #############################################################################
#' # Headshot Examples
#' #############################################################################
#' library(mlbplotR)
#' library(ggplot2)
#'
#'
#' dfh <- data.frame(
#'   random_value = runif(9, 0, 1),
#'   player_id = c("594798",
#'                   "592450",
#'                   "605141",
#'                   "665742",
#'                   "545361",
#'                   "665487",
#'                   "571448",
#'                   "0",
#'                   "543037")
#' )
#'
#' # use headshots for x-axis
#' ggplot(dfh, aes(x = player_id, y = random_value)) +
#'   geom_col(width = 0.5) +
#'   theme_minimal() +
#'   theme(axis.text.x = element_mlb_headshot())
#'
#' # use headshots for y-axis
#' ggplot(dfh, aes(y = player_id, x = random_value)) +
#'   geom_col(width = 0.5) +
#'   theme_minimal() +
#'   theme(axis.text.y = element_mlb_headshot())
#' }
#' @name element
#' @aliases NULL
NULL

#' @export
#' @rdname element
element_mlb_logo <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                             color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_mlb_logo", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_mlb_scoreboard_logo <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                                        color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_mlb_scoreboard_logo", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_mlb_dot_logo <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                                 color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_mlb_dot_logo", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_mlb_dark_cap_logo <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                                      color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_mlb_dark_cap_logo", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_mlb_light_cap_logo <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                                       color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_mlb_light_cap_logo", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_mlb_headshot <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                                 color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_mlb_headshot", "element_text", "element")
  )
}

#' @export
#' @rdname element
element_path <- function(alpha = NULL, colour = NA, hjust = NULL, vjust = NULL,
                         color = NULL, size = 0.5) {
  if (!is.null(color))  colour <- color
  structure(
    list(alpha = alpha, colour = colour, hjust = hjust, vjust = vjust, size = size),
    class = c("element_path", "element_text", "element")
  )
}

#' @export
element_grob.element_mlb_logo <- function(element, label = "", x = NULL, y = NULL,
                                          alpha = NULL, colour = NULL,
                                          hjust = NULL, vjust = NULL,
                                          size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- element$vjust %||% vjust
  hj <- element$hjust %||% hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "teams"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}

#' @export
element_grob.element_mlb_scoreboard_logo <- function(element, label = "", x = NULL, y = NULL,
                                          alpha = NULL, colour = NULL,
                                          hjust = NULL, vjust = NULL,
                                          size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- element$vjust %||% vjust
  hj <- element$hjust %||% hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "scoreboard"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}

#' @export
element_grob.element_mlb_dot_logo <- function(element, label = "", x = NULL, y = NULL,
                                              alpha = NULL, colour = NULL,
                                              hjust = NULL, vjust = NULL,
                                              size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- element$vjust %||% vjust
  hj <- element$hjust %||% hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "dot"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}

#' @export
element_grob.element_mlb_dark_cap_logo <- function(element, label = "", x = NULL, y = NULL,
                                                     alpha = NULL, colour = NULL,
                                                     hjust = NULL, vjust = NULL,
                                                     size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- element$vjust %||% vjust
  hj <- element$hjust %||% hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "dark_cap"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}

#' @export
element_grob.element_mlb_light_cap_logo <- function(element, label = "", x = NULL, y = NULL,
                                                   alpha = NULL, colour = NULL,
                                                   hjust = NULL, vjust = NULL,
                                                   size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- element$vjust %||% vjust
  hj <- element$hjust %||% hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "light_cap"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}


#' @export
element_grob.element_mlb_headshot <- function(element, label = "", x = NULL, y = NULL,
                                              alpha = NULL, colour = NULL,
                                              hjust = NULL, vjust = NULL,
                                              size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- element$vjust %||% vjust
  hj <- element$hjust %||% hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "headshots"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}

#' @export
element_grob.element_path <- function(element, label = "", x = NULL, y = NULL,
                                      alpha = NULL, colour = NULL,
                                      hjust = NULL, vjust = NULL,
                                      size = NULL, ...) {

  if (is.null(label)) return(ggplot2::zeroGrob())

  n <- max(length(x), length(y), 1)
  vj <- element$vjust %||% vjust
  hj <- element$hjust %||% hjust
  x <- x %||% unit(rep(hj, n), "npc")
  y <- y %||% unit(rep(vj, n), "npc")
  alpha <- alpha %||% element$alpha
  colour <- colour %||% rep(element$colour, n)
  size <- size %||% element$size

  grobs <- lapply(
    seq_along(label),
    axisImageGrob,
    alpha = alpha,
    colour = colour,
    label = label,
    x = x,
    y = y,
    hjust = hj,
    vjust = vj,
    type = "path"
  )

  class(grobs) <- "gList"

  grid::gTree(
    gp = grid::gpar(),
    children = grobs,
    size = size,
    cl = "axisImageGrob"
  )
}


axisImageGrob <- function(i, label, alpha, colour, data, x, y, hjust, vjust,
                          width = 1, height = 1,
                          type = c("teams", "light_cap", "dark_cap", "scoreboard", "dot", "headshots", "path")) {
  make_null <- FALSE
  type <- rlang::arg_match(type)
  if(type == "teams") {
    team_abbr <- label[i]
    image_to_read <- logo_list[[team_abbr]]
    if (is.na(team_abbr)) make_null <- TRUE
  } else if(type == "dark_cap"){
    team_abbr <- label[i]
    image_to_read <- dark_logo_list[[team_abbr]]
    if (is.na(team_abbr)) make_null <- TRUE
  } else if(type == "light_cap"){
    team_abbr <- label[i]
    image_to_read <- light_logo_list[[team_abbr]]
    if (is.na(team_abbr)) make_null <- TRUE
  } else if(type == "scoreboard"){
    team_abbr <- label[i]
    image_to_read <- scoreboard_logo_list[[team_abbr]]
    if (is.na(team_abbr)) make_null <- TRUE
  } else if(type == "dot"){
    team_abbr <- label[i]
    image_to_read <- dot_logo_list[[team_abbr]]
    if (is.na(team_abbr)) make_null <- TRUE
  } else if (type == "path"){
    image_to_read <- label[i]
  } else {
    id <- label[i]
    headshot_map <- load_headshots()
    image_to_read <- headshot_map$espn_headshot[headshot_map$savant_id == id]
    if(length(image_to_read) == 0){
      image_to_read <- na_headshot()
    } else if (is.na(image_to_read)){
      image_to_read <- na_headshot()
    }
  }

  if (is.na(make_null)){
    return(grid::nullGrob())
  } else if (is.null(alpha[i])) {
    img <- reader_function(image_to_read)
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w"){
      new <- magick::image_quantize(img, colorspace = 'gray')
    } else {
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(img, opa, col)
    }
  } else if (length(alpha) == 1L) {
    if (as.numeric(alpha) <= 0 || as.numeric(alpha) >= 1) {
      cli::cli_abort("aesthetic {.var alpha} requires a value between {.val 0} and {.val 1}")
    }
    img <- reader_function(image_to_read)
    new <- magick::image_fx(img, expression = paste0(alpha, "*a"), channel = "alpha")
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w"){
      new <- magick::image_quantize(new, colorspace = 'gray')
    } else {
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(new, opa, col)
    }
  } else {
    if (any(as.numeric(alpha) < 0) || any(as.numeric(alpha) > 1)) {
      cli::cli_abort("aesthetics {.var alpha} require values between {.val 0} and {.val 1}")
    }
    img <- reader_function(image_to_read)
    new <- magick::image_fx(img, expression = paste0(alpha[i], "*a"), channel = "alpha")
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w"){
      new <- magick::image_quantize(new, colorspace = 'gray')
    } else{
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(new, opa, col)
    }
  }

  grid <- grid::rasterGrob(new, just = c(hjust, vjust))

  grid$vp <- grid::viewport(
    x = grid::unit(x[i], "npc"),
    y = grid::unit(y[i], "npc"),
    width = grid::unit(width, "npc"),
    height = grid::unit(height, "npc")
    # angle = data$angle[i]
  )

  grid
}

#' @export
grobHeight.axisImageGrob <- function(x, ...) grid::unit(x$size, "cm")

#' @export
grobWidth.axisImageGrob <- function(x, ...) grid::unit(x$size, "cm")
