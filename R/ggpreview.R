#' Preview ggplot in Specified Dimensions
#'
#' This function previews a ggplot in its actual dimensions in order to see how
#' it will look when saved. It is also significantly faster than the default
#' preview in RStudio for ggplots created using mlbplotR.
#'
#' @inheritParams ggplot2::ggsave
#' @param asp The aspect ratio of the plot calculated as `width / height`. If
#'   this is a numeric value (and not `NULL`) the `height` of the plot will be
#'   recalculated to `height = width / asp`.
#' @return No return value, called for side effects.
#' @export
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
#' # note that the plot is assigned to the object "p"
#' p <- ggplot(df, aes(x = teams, y = random_value)) +
#'   geom_col(aes(color = teams, fill = teams), width = 0.5) +
#'   scale_color_mlb(type = "secondary") +
#'   scale_fill_mlb(alpha = 0.4) +
#'   theme_minimal() +
#'   theme(axis.text.x = element_mlb_logo())
#'
#' # preview p with defined width and aspect ratio (only available in RStudio)
#' if (rstudioapi::isAvailable()){
#'   ggpreview(p, width = 5, asp = 16/9)
#' }
#' }
ggpreview <- function(plot = ggplot2::last_plot(),
                      width = NA,
                      height = NA,
                      asp = NULL,
                      dpi = 300,
                      device = "png",
                      units = c("in", "cm", "mm", "px"),
                      scale = 1,
                      limitsize = TRUE,
                      bg = NULL,
                      ...){
  rlang::check_installed("rstudioapi", reason = "to preview a ggplot file")
  file <- tempfile()
  if (is.numeric(asp)) height <- width / asp
  ggplot2::ggsave(
    file,
    plot = plot,
    device = device,
    scale = scale,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    limitsize = limitsize,
    bg = bg,
    ...
  )
  rstudioapi::viewer(file)
}
