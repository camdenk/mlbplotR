#' @name gt_mlb
#' @title
#' Add MLB team logos into rows of a `gt` table
#' @description
#' The `gt_fmt_mlb_logo` and `gt_fmt_mlb_scoreboard_logo` functions take an existing
#' `gt_tbl` object and converts MLB team names from `valid_team_names()` into team logos.
#' This is a wrapper around
#' [`gtExtras::gt_image_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html)
#' written by Tom Mock, which is a wrapper around `gt::text_transform()` + `gt::web_image()`/
#' `gt::local_image()` with the necessary boilerplate already applied.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param columns The columns wherein changes to cell data colors should occur.
#'   Has no effect if `locations` is not `NULL`
#' @param height The absolute height (px) of the image in the table cell
#' @param locations If `NULL` (the default), the function will render
#'   logos in argument `columns`.
#'   Otherwise, the cell or set of cells to be associated with the team name/player id
#'   transformation. Only the [gt::cells_body()], [gt::cells_stub()],
#'   [gt::cells_column_labels()], and [gt::cells_row_groups()] helper functions
#'   can be used here. We can enclose several of these calls within a `list()`
#'   if we wish to make the transformation happen at different locations.
#' @return An object of class `gt_tbl`.
#' @export
#' @examples
#' library(gt)
#' library(mlbplotR)
#'
#' df <- data.frame(team = valid_team_names()[1:5],
#'                  logo = valid_team_names()[1:5],
#'                  scoreboard_logo = valid_team_names()[1:5])
#'
#' gt_logo_example <- df %>%
#'  gt::gt() %>%
#'  gt_fmt_mlb_logo(columns = "logo") %>%
#'  gt_fmt_mlb_scoreboard_logo(columns = "scoreboard_logo")

gt_fmt_mlb_logo <- function(gt_object, columns, height = 30, locations = NULL){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  gt_mlbplotR_image(
    gt_object = gt_object,
    columns = columns,
    height = height,
    locations = locations,
    type = "mlb_logo"
  )

}

#' @rdname gt_mlb
#' @export

gt_fmt_mlb_scoreboard_logo <- function(gt_object,
                                       columns,
                                       height = 30,
                                       locations = NULL){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  gt_mlbplotR_image(
    gt_object = gt_object,
    columns = columns,
    height = height,
    locations = locations,
    type = "scoreboard_logo"
  )
}


# Taken from nflplotR package and adapted for MLB purposes
gt_mlbplotR_image <- function(gt_object,
                              columns,
                              height = 30,
                              locations = NULL,
                              type = c("mlb_logo", "scoreboard_logo")){

  rlang::check_installed("gt (>= 0.8.0)", "to render images in gt tables.")

  type <- match.arg(type)

  if(is.null(locations)){
    locations <- gt::cells_body({{ columns }})
  }

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
get_image_uri <- function(team_abbr, type = c("mlb_logo", "scoreboard_logo")) {

  lookup_list <- switch (type,
                         "mlb_logo" = logo_list,
                         "scoreboard_logo" = scoreboard_logo_list
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
#' @param locations If `NULL` (the default), the function will render
#'   headshots in argument `columns`.
#'   Otherwise, the cell or set of cells to be associated with the team name/player id
#'   transformation. Only the [gt::cells_body()], [gt::cells_stub()],
#'   [gt::cells_column_labels()], and [gt::cells_row_groups()] helper functions
#'   can be used here. We can enclose several of these calls within a `list()`
#'   if we wish to make the transformation happen at different locations.
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
                                locations = NULL,
                                na_headshot_to_logo = TRUE) {

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))


  rlang::check_installed("gt (>= 0.8.0)", "to render images in gt tables.")

  if(is.null(locations)){
    locations <- gt::cells_body({{ columns }})
  }

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







#' Merge and stack text from two columns in `gt` and color one with team colors
#'
#' @description
#' The `gt_merge_stack_team_color()` function takes an existing `gt` table and merges
#' column 1 and column 2, stacking column 1's text on top of column 2's.
#' Top text is in all caps while the lower text is bigger, bolded,
#' and colored by the team name in another column.
#' This is a slightly modified version of [`gtExtras::gt_merge_stack()`](https://jthomasmock.github.io/gtExtras/reference/gt_merge_stack.html)  written by Tom Mock.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param col1 The column to stack on top. Will be converted to all caps, with black and bold text.
#' @param col2 The column to merge and place below. Will be smaller and the team color that corresponds to `team_col`.
#' @param team_col The column of team names that match `valid_team_names()` for the color of the bottom.
#' @param font_size_top the font size for the top text.
#' @param font_size_bottom the font size for the bottom text.
#' @param font_weight_top the font weight of the top text - defaults to "lighter"
#' @param font_weight_bottom the font weight of the bottom text - defaults to "bold"
#' @param font_variant_top the font variant of the top text - defaults to "small-caps"
#' @param font_variant_bottom the font variant of the bottom text - defaults to "small-caps"
#' @param color The color for the top text.
#' @return An object of class `gt_tbl`.
#' @importFrom gt %>%
#' @export
#' @import gt
#' @examples
#' library(gt)
#' library(mlbplotR)
#'
#' gt_merge_example <- mlbplotR::load_mlb_teams() %>%
#'   dplyr::slice(1:5) %>%
#'   dplyr::select(team_abbr, team_name) %>%
#'   tidyr::separate(team_name, c("Team1", "Team2","Team3"), fill = "left") %>%
#'   dplyr::mutate(Team3 = dplyr::if_else(Team3 %in% c("Sox", "Jays"), paste(Team2, Team3), Team3),
#'                 Team2 = dplyr::case_when(grepl("Sox|Jays", Team3) ~ Team1,
#'                                          Team3 == "Cardinals" ~ paste(Team1, Team2, sep = ". "),
#'                                          is.na(Team1) ~ Team2,
#'                                          TRUE ~ paste(Team1, Team2))) %>%
#'   dplyr::select(team_abbr, Team2, Team3) %>%
#'   gt::gt() %>%
#'   gt_merge_stack_team_color(col1 = "Team2", col2 = "Team3", team_col = "team_abbr")

gt_merge_stack_team_color <- function (gt_object,
                                       col1,
                                       col2,
                                       team_col,
                                       font_size_top = 12,
                                       font_size_bottom = 14,
                                       font_weight_top = "lighter",
                                       font_weight_bottom = "bold",
                                       font_variant_top = "small-caps",
                                       font_variant_bottom = "small-caps",
                                       color = "black"){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))


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
  gt_object %>% gt::text_transform(locations = if (isTRUE(row_name_var ==
                                                      col1_bare)) {
    gt::cells_stub(rows = gt::everything())
  }
  else {
    gt::cells_body(columns = {
      {
        col1
      }
    })
  }, fn = function(x) {
    paste0("<div style='line-height:10px'>
          <span style='font-variant:", font_variant_top,
          ";font-weight:", font_weight_top,
          ";color:", color,
          ";font-size:", font_size_top, "px'>", x,
          "</div>
          <div style='line-height:12px'>
          <span style ='font-variant:", font_variant_bottom,
          ";font-weight:", font_weight_bottom,
          ";color:", team_color,
          ";font-size:", font_size_bottom, "px'>", data_in,
          "</span></div>")
  }) %>% gt::cols_hide(columns = {
    {
      col2
    }
  })
}



