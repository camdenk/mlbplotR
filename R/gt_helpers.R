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
#'   Argument has no effect if `locations` is not `NULL`.
#' @param height The absolute height (px) of the image in the table cell
#' @param locations If `NULL` (the default), the function will render
#'   logos in argument `columns`.
#'   Otherwise, the cell or set of cells to be associated with the team name
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
#'                  scoreboard_logo = valid_team_names()[1:5],
#'                  dot_logo = valid_team_names()[1:5])
#'
#' gt_logo_example <- df %>%
#'  gt::gt() %>%
#'  gt_fmt_mlb_logo(columns = "logo") %>%
#'  gt_fmt_mlb_scoreboard_logo(columns = "scoreboard_logo") %>%
#'  gt_fmt_mlb_dot_logo(columns = "dot_logo")

gt_fmt_mlb_logo <- function(gt_object, columns, height = 30, locations = NULL){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  gt_mlbplotR_image(
    gt_object = gt_object,
    columns = {{ columns }},
    height = height,
    locations = locations,
    type = "mlb_logo"
  )

}

#' @rdname gt_mlb
#' @export

gt_fmt_mlb_scoreboard_logo <- function(gt_object, columns, height = 30, locations = NULL){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  gt_mlbplotR_image(
    gt_object = gt_object,
    columns = {{ columns }},
    height = height,
    locations = locations,
    type = "scoreboard_logo"
  )
}


#' @rdname gt_mlb
#' @export

gt_fmt_mlb_dot_logo <- function(gt_object, columns, height = 30, locations = NULL){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  gt_mlbplotR_image(
    gt_object = gt_object,
    columns = {{ columns }},
    height = height,
    locations = locations,
    type = "dot_logo"
  )
}


# Taken from nflplotR package and adapted for MLB purposes
gt_mlbplotR_image <- function(gt_object,
                              columns,
                              height = 30,
                              locations = NULL,
                              type = c("mlb_logo", "scoreboard_logo", "dot_logo")){

  rlang::check_installed("gt (>= 0.8.0)", "to render images in gt tables.")

  type <- match.arg(type)

  if(is.null(locations)){
    locations <- gt::cells_body({{ columns }})
  }

  if (is.numeric(height)) {
    height <- paste0(height, "px")
  }

  if (type %in% c("mlb_logo", "scoreboard_logo")) {
    gt::text_transform(
      data = gt_object,
      locations = locations,
      fn = function(x){
        team_abbr <- clean_team_abbrs(as.character(x), keep_non_matches = FALSE)
        # Create the image URI
        uri <- get_image_uri(team_abbr = team_abbr, type = type)
        # Generate the Base64-encoded image and place it within <img> tags
        out <- paste0("<img src=\"", uri, "\" style=\"height:", height, ";\">")
        out <- lapply(out, gt::html)
        # If the image uri returns NA we didn't find a match. We will return the
        # actual value then to allow the user to call gt::sub_missing()
        out[is.na(uri)] <- x[is.na(uri)]
        out
      }
    )
  } else if (type == "dot_logo") {

    gt::text_transform(
      data = gt_object,
      locations = locations,
      fn = function(x){
        image_urls <- vapply(
          x,
          FUN.VALUE = character(1),
          USE.NAMES = FALSE,
          FUN = function(team_abbr) {

            if (team_abbr %in% names(dot_logo_list)) {
              dot_logo_list[[team_abbr]]
            } else {
              # Return the MLB logo if there isn't a match
              dot_logo_list[["MLB"]]
            }

          }
        )
        gt::web_image(image_urls, height = height)
      }
    )

  }

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
      # every non match will return NULL which is when we want NA
      if (is.null(lookup_list[[team]])) return(NA_character_)
      paste0(
        "data:", "image/png",
        ";base64,", base64enc::base64encode(lookup_list[[team]])
      )
    }
  )
}


#' @name gt_milb
#' @title
#' Add MiLB team logos into rows of a `gt` table
#' @description
#' The `gt_fmt_milb_logo` and `gt_fmt_milb_dot_logo` functions take an existing
#' `gt_tbl` object and converts MiLB team names into team logos.
#' This is a wrapper around
#' [`gtExtras::gt_image_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html)
#' written by Tom Mock, which is a wrapper around `gt::text_transform()` + `gt::web_image()`/
#' `gt::local_image()` with the necessary boilerplate already applied.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param columns The columns wherein changes to cell data colors should occur.
#'   Argument has no effect if `locations` is not `NULL`.
#' @param height The absolute height (px) of the image in the table cell
#' @param locations If `NULL` (the default), the function will render
#'   logos in argument `columns`.
#'   Otherwise, the cell or set of cells to be associated with the team name
#'   transformation. Only the [gt::cells_body()], [gt::cells_stub()],
#'   [gt::cells_column_labels()], and [gt::cells_row_groups()] helper functions
#'   can be used here. We can enclose several of these calls within a `list()`
#'   if we wish to make the transformation happen at different locations.
#' @return An object of class `gt_tbl`.
#' @examples
#' \donttest{
#' library(gt)
#' library(mlbplotR)
#' gt_milb_example <- mlbplotR::load_milb_teams() %>%
#'   dplyr::filter(parent_org_name == "Texas Rangers") %>%
#'   dplyr::mutate(dot = team_name) %>%
#'   dplyr::select(team_name, dot, team_location, team_mascot) %>%
#'   gt::gt() %>%
#'   gt_fmt_milb_logo(columns = "team_name") %>%
#'   gt_fmt_milb_dot_logo(columns = "dot")
#' }
#' @export
gt_fmt_milb_logo <- function(gt_object,
                             columns,
                             height = 30,
                             locations = NULL) {

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))


  rlang::check_installed("gt (>= 0.8.0)", "to render images in gt tables.")

  if(is.null(locations)){
    locations <- gt::cells_body({{ columns }})
  }

  gt::text_transform(
    data = gt_object,
    locations = locations,
    fn = function(mlb_id){
      milb_logo_map <- load_milb_teams()
      image_urls <- vapply(
        mlb_id,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(team_name_full) {
          ret <- milb_logo_map$team_logo[milb_logo_map$team_name == team_name_full]
          if(length(ret) == 0 || is.na(ret)) ret <- NA_character_
          ret
        }
      )
      gt::web_image(image_urls, height = height)
    }
  )

}

#' @rdname gt_milb
#' @export
gt_fmt_milb_dot_logo <- function(gt_object,
                                 columns,
                                 height = 30,
                                 locations = NULL) {

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))


  rlang::check_installed("gt (>= 0.8.0)", "to render images in gt tables.")

  if(is.null(locations)){
    locations <- gt::cells_body({{ columns }})
  }

  gt::text_transform(
    data = gt_object,
    locations = locations,
    fn = function(mlb_id){
      milb_logo_map <- load_milb_teams()
      image_urls <- vapply(
        mlb_id,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(team_name_full) {
          ret <- milb_logo_map$team_dot_logo[milb_logo_map$team_name == team_name_full]
          if(length(ret) == 0 || is.na(ret)) ret <- NA_character_
          ret
        }
      )
      gt::web_image(image_urls, height = height)
    }
  )

}



#' @name gt_mlb_headshots
#' @title Render Player Headshots in 'gt' Tables
#'
#' @description
#' `gt_fmt_mlb_headshot`, `gt_fmt_mlb_dot_headshot`, and `gt_fmt_milb_dot_headshot` take an existing `gt_tbl` object and converts player ids into headshots.
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
#'   Otherwise, the cell or set of cells to be associated with the team name
#'   transformation. Only the [gt::cells_body()], [gt::cells_stub()],
#'   [gt::cells_column_labels()], and [gt::cells_row_groups()] helper functions
#'   can be used here. We can enclose several of these calls within a `list()`
#'   if we wish to make the transformation happen at different locations.
#'
#' @return An object of class `gt_tbl`.
#' @examples
#' \donttest{
#' library(gt)
#' library(mlbplotR)
#' gt_headshot_example <- mlbplotR::load_headshots() %>%
#'   head(5) %>%
#'   dplyr::select(player_name, savant_id1 = savant_id, savant_id2 = savant_id) %>%
#'   gt::gt() %>%
#'   gt_fmt_mlb_headshot(columns = "savant_id1") %>%
#'   gt_fmt_mlb_dot_headshot(columns = "savant_id2")
#' }
NULL

#' @rdname gt_mlb_headshots
#' @export
gt_fmt_mlb_headshot <- function(gt_object,
                                columns,
                                height = 30,
                                na_headshot_to_logo = TRUE,
                                locations = NULL) {

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))


  rlang::check_installed("gt (>= 0.8.0)", "to render images in gt tables.")

  if(is.null(locations)){
    locations <- gt::cells_body({{ columns }})
  }

  gt::text_transform(
    data = gt_object,
    locations = locations,
    fn = function(mlb_id){
      image_urls <- vapply(
        mlb_id,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(id) {
          url <- paste0("https://midfield.mlbstatic.com/v1/people/", id, "/mlb/436?circle=true")
          if (isFALSE(check_url(url))) {
            url <- paste0("https://midfield.mlbstatic.com/v1/people/", id, "/spots/436")
          }

          return(url)
        }
      )
      gt::web_image(image_urls, height = height)
    }
  )

}


#' @rdname gt_mlb_headshots
#' @export
gt_fmt_mlb_dot_headshot <- function(gt_object,
                                columns,
                                height = 30,
                                na_headshot_to_logo = TRUE,
                                locations = NULL) {

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))


  rlang::check_installed("gt (>= 0.8.0)", "to render images in gt tables.")

  if(is.null(locations)){
    locations <- gt::cells_body({{ columns }})
  }

  gt::text_transform(
    data = gt_object,
    locations = locations,
    fn = function(mlb_id){
      image_urls <- vapply(
        mlb_id,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(id) {
          paste0("https://midfield.mlbstatic.com/v1/people/", id, "/spots/436")
        }
      )
      gt::web_image(image_urls, height = height)
    }
  )

}




#' @rdname gt_mlb_headshots
#' @export
gt_fmt_milb_dot_headshot <- function(gt_object,
                                     columns,
                                     height = 30,
                                     na_headshot_to_logo = TRUE,
                                     locations = NULL) {

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))


  rlang::check_installed("gt (>= 0.8.0)", "to render images in gt tables.")

  if(is.null(locations)){
    locations <- gt::cells_body({{ columns }})
  }

  gt::text_transform(
    data = gt_object,
    locations = locations,
    fn = function(mlb_id){
      image_urls <- vapply(
        mlb_id,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        FUN = function(id) {
          paste0("https://midfield.mlbstatic.com/v1/people/", id, "/milb/436?circle=true")
        }
      )
      gt::web_image(image_urls, height = height)
    }
  )

}



#' Replace Team Abbreviations/Player IDs With Images In Column Labels
#'
#' @description
#' `gt_mlb_column_labels` takes in a value of a team abbreviation or player id and
#' converts the designated column to the corresponding image.
#'
#' @param value What team abbreviation/player id should be replaced with an image?
#' @param type What type of image is replacing the value?
#' @param height The absolute height (px) of the image
#' @param na_headshot_to_logo should NA/non player id matches return the MLB logo instead
#'   of a grayed out blank headshot? Ignored unless `value` is equal to `"headshot"`. Defaults to `TRUE`
#'
#' @return HTML tag for image
#' @export
#' @examples
#' \donttest{
#' library(gt)
#' library(mlbplotR)
#'
#' df <- data.frame(BAL = 1,
#'                  TEX = 1,
#'                  LAD = 1,
#'                  "Mike_Trout" = 1,
#'                  "Shohei_Ohtani" = 1
#'                  )
#'
#' gt_column_example <- df %>%
#'   gt::gt() %>%
#'   gt::cols_label(BAL = gt_mlb_column_labels("BAL", "mlb_logo"),
#'                  TEX = gt_mlb_column_labels("TEX", "scoreboard_logo"),
#'                  LAD = gt_mlb_column_labels("LAD", "dot_logo"),
#'                  "Mike_Trout" = gt_mlb_column_labels(545361, "dot_headshot"),
#'                  "Shohei_Ohtani" = gt_mlb_column_labels(660271, "headshot"))
#' }
#' @export
gt_mlb_column_labels <- function(value,
                                 type = c("mlb_logo", "scoreboard_logo",
                                          "dot_logo",  "headshot", "dot_headshot"),
                                 height = 30,
                                 na_headshot_to_logo = TRUE) {

  if (type == "headshot") {

    hs_url <- paste0("https://midfield.mlbstatic.com/v1/people/", value, "/mlb/436?circle=true")

    if (isFALSE(check_url(hs_url))) {
      hs_url <- paste0("https://midfield.mlbstatic.com/v1/people/", value, "/spots/436")
    }

    html_content <- paste0("<img src=\"", hs_url, "\" style=\"height:", height, "px;\">")

    html_content <- gt::html(html_content)
    html_content

  } else if (type == "dot_headshot") {

    hs_url <- paste0("https://midfield.mlbstatic.com/v1/people/", value, "/spots/436")


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
#' \donttest{
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
#' }
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



