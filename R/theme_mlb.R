#' Theme for MLB Team Logos
#'
#' @description
#'   `r lifecycle::badge("superseded")`
#'   `theme_x_mlb()` and `theme_y_mlb()` have been superceded in favor of `element_*_logo()` functions
#'   These functions are convenience wrappers around a theme call
#'   that activates markdown in x-axis and y-axis labels
#'   using [`ggtext::element_markdown()`].
#' @details These functions are a wrapper around the function calls
#'   `ggplot2::theme(axis.text.x = ggtext::element_markdown())` as well as
#'   `ggplot2::theme(axis.text.y = ggtext::element_markdown())`.
#'   They are made to be used in conjunction with [`scale_x_mlb()`] and
#'   [`scale_y_mlb()`] respectively.
#' @name theme_mlb
#' @aliases NULL
#' @return A ggplot2 theme created with [ggplot2::theme()].
#' @seealso [`theme_x_mlb()`], [`theme_y_mlb()`]
#' @examples
#' \donttest{
#' library(mlbplotR)
#' library(ggplot2)
#'
#' team_abbr <- valid_team_names()
#' # remove conference logos from this example
#' team_abbr <- team_abbr[!team_abbr %in% c("AL", "NL", "MLB")]
#'
#' df <- data.frame(
#'   random_value = runif(length(team_abbr), 0, 1),
#'   teams = team_abbr
#' )
#' if (utils::packageVersion("gridtext") > "0.1.4"){
#'   ggplot(df, aes(x = teams, y = random_value)) +
#'     geom_col(aes(color = teams, fill = teams), width = 0.5) +
#'     scale_color_mlb(type = "secondary") +
#'     scale_fill_mlb(alpha = 0.4) +
#'     scale_x_mlb() +
#'     theme_minimal() +
#'     # theme_*_mlb requires gridtext version > 0.1.4
#'     theme_x_mlb()
#' }
#' }
NULL

#' @rdname theme_mlb
#' @export
theme_x_mlb <- function(){

  if (!is_installed("ggtext")) {
    cli::cli_abort(c(
      "Package {.val ggtext} required to create this scale.",
      'Please install it with {.var install.packages("ggtext")}'
    ))
  }
  loadNamespace("gridtext", versionCheck = list(op = ">", version = "0.1.4"))
  ggplot2::theme(axis.text.x = ggtext::element_markdown())
}

#' @rdname theme_mlb
#' @export
theme_y_mlb <- function(){
  if (!is_installed("ggtext")) {
    cli::cli_abort(c(
      "Package {.val ggtext} required to create this scale.",
      'Please install it with {.var install.packages("ggtext")}'
    ))
  }
  loadNamespace("gridtext", versionCheck = list(op = ">", version = "0.1.4"))
  ggplot2::theme(axis.text.y = ggtext::element_markdown())
}
