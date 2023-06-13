#' @name gt_mlb
#' @title
#' Add MLB team logos into rows of a `gt` table
#' @description
#' The `gt_fmt_mlb_logo`, `gt_fmt_mlb_scoreboard_logo`, and `gt_fmt_mlb_dot_logo` functions take an existing
#' `gt_tbl` object and converts MLB team names from `valid_team_names()` into team logos.
#' This is a wrapper around
#' [`gtExtras::gt_image_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html)
#' written by Tom Mock, which is a wrapper around `gt::text_transform()` + `gt::web_image()`/
#' `gt::local_image()` with the necessary boilerplate already applied.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param columns The columns wherein changes to cell data colors should occur.
#' @param height The absolute height (px) of the image in the table cell
#' @return An object of class `gt_tbl`.
#' @export
#' @examples
#' library(gt)
#' library(mlbplotR)
#'
#' df <- data.frame(team = valid_team_names()[1:5],
#'                  logo = valid_team_names()[1:5],
#'                  scoreboard_logo = valid_team_names()[1:5],
#'                  dot_logo = valid_team_names()[1:5])
#'
#' gt_logo_example <- df %>%
#'  gt::gt() %>%
#'  gt_fmt_mlb_logo(columns = "logo") %>%
#'  gt_fmt_mlb_scoreboard_logo(columns = "scoreboard_logo") %>%
#'  gt_fmt_mlb_dot_logo(columns = "dot_logo")

gt_fmt_mlb_logo <- function(gt_object, columns, height = 30){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  gt_mlbplotR_image(
    gt_object = gt_object,
    columns = columns,
    height = height,
    type = "mlb_logo"
  )

}

#' @rdname gt_mlb
#' @export

gt_fmt_mlb_scoreboard_logo <- function(gt_object, columns, height = 30){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  gt_mlbplotR_image(
    gt_object = gt_object,
    columns = columns,
    height = height,
    type = "scoreboard_logo"
  )
}


#' @rdname gt_mlb
#' @export

gt_fmt_mlb_dot_logo <- function(gt_object, columns, height = 30){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  gt_mlbplotR_image(
    gt_object = gt_object,
    columns = columns,
    height = height,
    type = "dot_logo"
  )
}


# Taken from nflplotR package and adapted for MLB purposes
gt_mlbplotR_image <- function(gt_object,
                              columns,
                              height = 30,
                              type = c("mlb_logo", "scoreboard_logo", "dot_logo")){

  rlang::check_installed("gt (>= 0.8.0)", "to render images in gt tables.")

  type <- match.arg(type)

  locations <- gt::cells_body({{ columns }})

  if (is.numeric(height)) {
    height <- paste0(height, "px")
  }

  gt::text_transform(
    data = gt_object,
    locations = locations,
    fn = function(x){
      team_abbr <- clean_team_abbrs(as.character(x), keep_non_matches = FALSE)
      # Create the image URI
      uri <- get_image_uri(team_abbr = team_abbr, type = type)
      # Generate the Base64-encoded image and place it within <img> tags
      paste0("<img src=\"", uri, "\" style=\"height:", height, ";\">")
    }
  )

}



# Taken from gt and nflplotR package
# Get image URIs from image lists as a vector Base64-encoded image strings
get_image_uri <- function(team_abbr, type = c("mlb_logo", "scoreboard_logo", "dot_logo")) {

  lookup_list <- switch (type,
                         "mlb_logo" = logo_list,
                         "scoreboard_logo" = scoreboard_logo_list,
                         "dot_logo" = dot_logo_list
  )

  vapply(
    team_abbr,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    FUN = function(team) {
      paste0(
        "data:", "image/png",
        ";base64,", base64enc::base64encode(lookup_list[[team]])
      )
    }
  )
}





#' Render Player Headshots in 'gt' Tables
#'
#' @description
#' `gt_fmt_mlb_headshot` takes an existing `gt_tbl` object and converts player ids into headshots.
#' This is a wrapper around
#' [`gtExtras::gt_image_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html)
#' written by Tom Mock, which is a wrapper around `gt::text_transform()` + `gt::web_image()`/
#' `gt::local_image()` with the necessary boilerplate already applied.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param columns The columns wherein changes to cell data colors should occur.
#'   Has no effect if `locations` is not `NULL`
#' @param height The absolute height (px) of the image in the table cell
#' @param na_headshot_to_logo should NA/non matches return the MLB logo instead
#'   of a grayed out blank headshot? Defaults to `TRUE`
#'
#' @return An object of class `gt_tbl`.
#' @export
#' @examples
#' library(gt)
#' library(mlbplotR)
#' gt_headshot_example <- mlbplotR::load_headshots() %>%
#'   head(5) %>%
#'   dplyr::select(player_name, savant_id) %>%
#'   gt::gt() %>%
#'   gt_fmt_mlb_headshot(columns = "savant_id")
#' @export
gt_fmt_mlb_headshot <- function(gt_object,
                                columns,
                                height = 30,
                                na_headshot_to_logo = TRUE) {

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))


  rlang::check_installed("gt (>= 0.8.0)", "to render images in gt tables.")

  locations <- gt::cells_body({{ columns }})

  gt::text_transform(
    data = gt_object,
    locations = locations,
    fn = function(mlb_id){
      headshot_map <- load_headshots()
      image_urls <- vapply(
        mlb_id,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(id) {
          ret <- headshot_map$espn_headshot[headshot_map$savant_id == id]
          if(length(ret) == 0 || is.na(ret)) ret <- na_headshot(na_headshot_to_logo)
          ret
        }
      )
      gt::web_image(image_urls, height = height)
    }
  )

}



#' Replace Team Abbreviations/Player IDs With Images In Column Labels
#'
#' @description
#' `gt_column_example` takes in a value of a team abbreviation or player id and
#' converts the designated column to the corresponding image.
#'
#' @param value What team abbreviation/player id should be replaced with an image?
#' @param type What type of image is replacing the value?
#' @param height The absolute height (px) of the image
#' @param na_headshot_to_logo should NA/non player id matches return the MLB logo instead
#'   of a grayed out blank headshot? Defaults to `TRUE`
#'
#' @return HTML tag for image
#' @export
#' @examples
#' library(gt)
#' library(mlbplotR)
#'
#' df <- data.frame(BAL = 1,
#'                  TEX = 1,
#'                  "Mike_Trout" = 1,
#'                  "Shohei_Ohtani" = 1
#'                  )
#'
#' gt_column_example <- df %>%
#'   gt::gt() %>%
#'   gt::cols_label(BAL = gt_mlb_column_labels("BAL", "mlb_logo"),
#'                  TEX = gt_mlb_column_labels("TEX", "scoreboard_logo"),
#'                  "Mike_Trout" = gt_mlb_column_labels(545361, "headshot"),
#'                  "Shohei_Ohtani" = gt_mlb_column_labels(660271, "headshot"))
#' @export
gt_mlb_column_labels <- function(value,
                                 type = c("mlb_logo", "scoreboard_logo",
                                          "headshot"),
                                 height = 30,
                                 na_headshot_to_logo = TRUE) {

  if (type == "headshot") {

    headshot_map <- load_headshots()

    hs_url <- headshot_map$espn_headshot[headshot_map$savant_id == as.numeric(value)]
    if(length(hs_url) == 0 || is.na(hs_url)) hs_url <- na_headshot(na_headshot_to_logo)


    html_content <- paste0("<img src=\"", hs_url, "\" style=\"height:", height, "px;\">")

    html_content <- gt::html(html_content)
    html_content

  } else {

    team_abbr <- clean_team_abbrs(as.character(value), keep_non_matches = FALSE)
    # Create the image URI
    uri <- get_image_uri(team_abbr = team_abbr, type = type)
    # Generate the Base64-encoded image and place it within <img> tags
    html_content <- paste0("<img src=\"", uri, "\" style=\"height:", height, "px;\">")

    html_content <- gt::html(html_content)
    html_content
  }


}





#' Merge and stack text from two columns in `gt` and color one with team colors
#'
#' @description
#' The `gt_merge_stack_team_color()` function takes an existing `gt` table and merges
#' column 1 and column 2, stacking column 1's text on top of column 2's.
#' Top text is in all caps while the lower text is bigger, bolded,
#' and colored by the team name in another column.
#' This is a slightly modified version of [`gtExtras::gt_merge_stack()`](https://jthomasmock.github.io/gtExtras/reference/gt_merge_stack.html) written by Tom Mock.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param col1 The column to stack on top.
#' @param col2 The column to merge and place below with the text team color that corresponds to `team_col`.
#' @param team_col The column of team abbreviations (cleaned with `clean_team_abbrs()`) that match `valid_team_names()` for the color of the bottom text.
#' @param font_sizes the font size for the top and bottom text in px. Can be vector of length 1 or 2. Defaults to c(12, 14)
#' @param font_weights the font weight of the top and bottom text. Can be vector of length 1 or 2. Defaults to c("lighter", "bold")
#' @param font_variants the font variant of the top and bottom text. Can be vector of length 1 or 2. Defaults to "small-caps"
#' @param color The color for the top text.
#' @return An object of class `gt_tbl`.
#' @examples
#' library(gt)
#' library(mlbplotR)
#'
#' gt_merge_example <- mlbplotR::load_mlb_teams() %>%
#'   dplyr::slice(1:5) %>%
#'   dplyr::select(team_abbr, team_location, team_mascot) %>%
#'   gt::gt() %>%
#'   gt_merge_stack_team_color(col1 = "team_location",
#'                             col2 = "team_mascot",
#'                             team_col = "team_abbr")
#' @export
gt_merge_stack_team_color <- function (gt_object,
                                       col1,
                                       col2,
                                       team_col,
                                       font_sizes = c(12, 14),
                                       font_weights = c("lighter", "bold"),
                                       font_variants = c("small-caps"),
                                       color = "black"){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  stopifnot("`font_sizes` should be of length 1 or 2" = length(font_sizes) >= 1 && length(font_sizes) <= 2)
  stopifnot("`font_weights` should be of length 1 or 2" = length(font_weights) >= 1 && length(font_weights) <= 2)
  stopifnot("`font_variants` should be of length 1 or 2" = length(font_variants) >= 1 && length(font_variants) <= 2)

  if(length(font_sizes) == 1) font_sizes <- rep(font_sizes, 2)
  if(length(font_weights) == 1) font_weights <- rep(font_weights, 2)
  if(length(font_variants) == 1) font_variants <- rep(font_variants, 2)



  team <- rlang::enexpr(team_col) %>% rlang::as_string()
  team_bare <- gt_object[["_data"]][[team]]
  if(is.null(team_bare)) {
    cli::cli_abort("Must include a column of team names, `team_col` is NULL")
  }

  team_bare <- mlbplotR::clean_team_abbrs(team_bare)

  # Find the primary color for each team supplied
  team_color <- replace(team_bare,
                        1:length(team_bare),
                        c(primary_colors[team_bare[1:length(team_bare)]]))

  team_color[which(is.na(team_color))] <- "gray"


  col1_bare <- rlang::enexpr(col1) %>% rlang::as_string()
  row_name_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] ==
                                                           "stub")]
  col2_bare <- rlang::enexpr(col2) %>% rlang::as_string()
  data_in <- gt_object[["_data"]][[col2_bare]]

  gt_object %>%
    gt::text_transform(locations = if (isTRUE(row_name_var == col1_bare)) {
    gt::cells_stub(rows = gt::everything())
  }
  else {
    gt::cells_body(columns = {{ col1 }})
  }, fn = function(x) {
    paste0("<div style='line-height:10px'>
          <span style='font-variant:", font_variants[1],
          ";font-weight:", font_weights[1],
          ";color:", color,
          ";font-size:", font_sizes[1], "px'>", x,
          "</div>",

          "<div style='line-height:12px'>
          <span style ='font-variant:", font_variants[2],
          ";font-weight:", font_weights[2],
          ";color:", team_color,
          ";font-size:", font_sizes[2], "px'>", data_in,
          "</span></div>")
  }) %>%
    gt::cols_hide(columns = {{ col2 }})
}



