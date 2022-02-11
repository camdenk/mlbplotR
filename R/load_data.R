#' Load MLB Team Colors, and Logos
#'
#' @description Loads team colors, and logos - useful for plots!
#'
#' @examples
#' \donttest{
#'   load__mlb_teams()
#' }
#'
#' @return A tibble of team-level abbreviations, image URLs, and hex color codes.
#'
#' @seealso Issues with this data should be filed here: <https://github.com/camdenk/mlbplotR>
#'
#' @export
load_mlb_teams <- function(){
  out <- readRDS("./R/MLB_Colors_Logos.rds")
  #class(out) <- c("tbl_df","tbl","data.table","data.frame")
  out
}
