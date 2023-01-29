#' @name gt_mlb
#' @title
#' Add logos into rows of a `gt` table
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
#' @inheritParams gt::web_image
#' @inheritParams gt::local_image
#' @return An object of class `gt_tbl`.
#' @importFrom gt %>%
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

gt_fmt_mlb_logo <- function(gt_object, columns, height = 30){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  # convert tidyeval column to bare string
  column_names <- gt:::resolve_cols_c(
    expr = {{ columns }},
    data = gt_object
  )

  stub_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="stub")]
  grp_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="row_group")]

  img_source <- "web"

  gt_object %>%
    gt::text_transform(
      locations = if(isTRUE(grp_var %in% column_names)){
        gt::cells_row_groups()
      } else if(isTRUE(stub_var %in% column_names)){
        gt::cells_stub(rows = gt::everything())
      } else {
        gt::cells_body({{ columns }})
      },
      fn = function(x){
        if(img_source == "web"){
          x <- mlbplotR::clean_team_abbrs(as.character(x))
          x[which(!x%in%valid_team_names())] <- "MLB"
          gt::web_image(url = logo_urls[x], height = height)
        } else {
          gt::local_image(filename = x, height = height)
        }
      }
    )
}

#' @rdname gt_mlb
#' @export

gt_fmt_mlb_scoreboard_logo <- function(gt_object, columns, height = 30){

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  # convert tidyeval column to bare string
  column_names <- gt:::resolve_cols_c(
    expr = {{ columns }},
    data = gt_object
  )

  stub_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="stub")]
  grp_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="row_group")]

  img_source <- "web"

  gt_object %>%
    gt::text_transform(
      locations = if(isTRUE(grp_var %in% column_names)){
        gt::cells_row_groups()
      } else if(isTRUE(stub_var %in% column_names)){
        gt::cells_stub(rows = gt::everything())
      } else {
        gt::cells_body({{ columns }})
      },
      fn = function(x){
        if(img_source == "web"){
          x <- mlbplotR::clean_team_abbrs(as.character(x))
          x[which(!x%in%valid_team_names())] <- "MLB"
          gt::web_image(url = scoreboard_logo_urls[x], height = height)
        } else {
          gt::local_image(filename = x, height = height)
        }
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

gt_merge_stack_team_color <- function (gt_object, col1, col2, team_col,
                                       font_size_top = 12, font_size_bottom = 14,
                                       font_weight_top = "lighter", font_weight_bottom = "bold",
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
          <span style='font-variant:small-caps;font-weight:", font_weight_top, ";color:", color, ";font-size:", font_size_top, "px'>", x,
          "</div>
          <div style='line-height:12px'>
          <span style ='font-variant:small-caps;font-weight:", font_weight_bottom, ";color:", team_color, ";font-size:", font_size_bottom, "px'>", data_in,
          "</span></div>")
  }) %>% gt::cols_hide(columns = {
    {
      col2
    }
  })
}



#' Add player headshots to a `gt_mlb` table
#'
#' @description
#' Takes an existing `gt_tbl` object and converts MLBAM IDs into player headshots.
#' This is a wrapper around
#' [`gtExtras::gt_image_rows()`](https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html)
#' written by Tom Mock, which is a wrapper around `gt::text_transform()` + `gt::web_image()`/
#' `gt::local_image()` with the necessary boilerplate already applied.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param columns The columns wherein changes to cell data colors should occur.
#' @inheritParams gt::web_image
#' @inheritParams gt::local_image
#' @return An object of class `gt_tbl`.
#' @importFrom gt %>%
#' @importFrom tibble deframe
#' @importFrom tibble add_row
#' @export
#' @examples
#' library(gt)
#' library(mlbplotR)
#'
#' gt_headshot_example <- mlbplotR::load_headshots() %>%
#'   head(5) %>%
#'   dplyr::select(player_name, savant_id) %>%
#'   gt::gt() %>%
#'   gt_fmt_mlb_headshot(columns = "savant_id")

gt_fmt_mlb_headshot <- function(gt_object, columns, height = 30) {

  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))

  headshot_map <- mlbplotR::load_headshots()
  headshot_map <- headshot_map[,c(5,10)]
  headshot_map <- headshot_map[rowSums(is.na(headshot_map)) == 0, ]
  headshot_map <- headshot_map %>%
    tibble::add_row(savant_id = 0, espn_headshot = na_headshot())

  id_options <- headshot_map$savant_id

  headshot_map <- headshot_map %>%
    tibble::deframe()

  column_names <- gt:::resolve_cols_c(
    expr = {{ columns }},
    data = gt_object
  )

  stub_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="stub")]
  grp_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]]=="row_group")]

  # need to correct for rownames
  gt_object %>%
    gt::text_transform(
      locations = if(isTRUE(grp_var %in% column_names)){
        gt::cells_row_groups()
      } else if(isTRUE(stub_var %in% column_names)){
        gt::cells_stub(rows = gt::everything())
      } else {
        gt::cells_body({{ columns }})
      },
      fn = function(x){

        x[which(!x%in%id_options)] <- 0
        gt::web_image(url = headshot_map[x], height = height)
      }
    )
}
