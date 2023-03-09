#' Load MLB Team Colors, and Logos
#'
#' @description Loads team colors, and logos - useful for plots!
#'
#' @examples
#' \donttest{
#' load_mlb_teams()
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
#'
#' @return A vector of type `"character"`.
#' @examples
#' \donttest{
#' # List valid team abbreviations excluding duplicates
#' valid_team_names()
#' }
#'
#' @export
valid_team_names <- function(){
  sort(names(logo_list))
}


#' Standardize MLB Team Abbreviations
#'
#' This function standardizes MLB team abbreviations to Baseball Savant defaults.
#' This helps for joins and plotting
#'
#' @param abbr a character vector of abbreviations
#' @param keep_non_matches will non-matches be kept in the vector?
#'
#' @return A character vector with the length of `abbr` and cleaned team abbreviations
#'   if they are included in `team_data`. Non matches may be replaced
#'   with `NA` (depending on the value of `keep_non_matches`).
#' @examples
#' \donttest{
#' x <- c("PIE", "STL", "WSN", "CWS", "CHW")
#' # use current location and keep non matches
#' clean_team_abbrs(x)
#' }
#' @export
clean_team_abbrs <- function(abbr, keep_non_matches = TRUE) {
  stopifnot(is.character(abbr))

  abbrs <- unname(team_data[toupper(abbr)])

  if (isTRUE(keep_non_matches)) abbrs <- abbrs %c% abbr

  abbrs
}



#' Output MLB Team Abbreviations
#'
#' @export
#' @return A tibble of player names and ids from various sources.
#' @examples
#' \donttest{
#' load_headshots()
#' }
#'
load_headshots <- function() rds_from_url("https://github.com/camdenk/mlbplotR-data/raw/main/Player-IDs.rds")
