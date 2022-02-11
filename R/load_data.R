#' Load MLB Team Colors, and Logos
#'
#' @description Loads team colors, and logos - useful for plots!
#'
#' @examples
#' \donttest{
#'   load_mlb_teams()
#' }
#'
#' @return A tibble of team-level abbreviations, image URLs, and hex color codes.
#'
#' @seealso Issues with this data should be filed here: <https://github.com/camdenk/mlbplotR>
#'
#' @export
load_mlb_teams <- function(){
  out <- teams_colors_logos
  class(out) <- c("tbl_df","tbl","data.table","data.frame")
  out
}

#' Output Valid MLB Team Abbreviations
#'
#' @export
#' @return A vector of type `"character"`.
#' @examples
#' # List valid team abbreviations excluding duplicates
#' valid_team_names()
#'
valid_team_names <- function(){
  sort(names(logo_list))
}

