#' Create MLB Team Tiers
#'
#' @description This function sets up a ggplot to visualize MLB team tiers.
#'   Adapted from [`nflplotR`](https://nflplotr.nflverse.com/reference/nfl_team_tiers.html)
#'
#' @param data A data frame that has to include the variables `tier_no` (the
#'   number of the tier starting from the top tier no. 1) and `team_abbr` (the
#'   team abbreviation). `team_abbr` should be one of [`valid_team_names()`] and
#'   the function tries to clean team names internally by calling
#'   [`clean_team_abbrs()`]. If data includes the variable `tier_rank`,
#'   these ranks will be used within each tier. Otherwise, if `presort = FALSE`,
#'   the function will assume that data is already sorted and if `presort = TRUE`,
#'   teams will be sorted alphabetically within tiers.
#' @param title The title of the plot. If `NULL`, it will be omitted.
#' @param subtitle The subtitle of the plot. If `NULL`, it will be omitted.
#' @param caption The caption of the plot. If `NULL`, it will be omitted.
#' @param tier_desc A named vector consisting of the tier descriptions. The names
#'   must equal the tier numbers from `tier_no`
#' @param presort If `FALSE` (the default) the function assumes that the teams
#'   are already sorted within the tiers. Will otherwise sort alphabetically.
#' @param alpha The alpha channel of the logos, i.e. transparency level, as a
#'   numerical value between 0 and 1. Defaults to 1
#' @param width The desired width of the logo in `npc` (Normalised Parent Coordinates). A typical size is 0.075.
#' @param no_line_below_tier Vector of tier numbers. The function won't draw tier
#'   separation lines below these tiers. This is intended to be used for tiers
#'   that shall be combined (see examples).
#' @param devel Determines if logos shall be rendered. If `FALSE` (the default),
#'   logos will be rendered on each run. If `TRUE` the team abbreviations will be
#'   plotted instead of the logos. This is much faster and helps with the plot
#'   development.
#' @param background_color Background color for the plot. Defaults to "#1e1e1e"
#' @param line_color Line color for the plot. Defaults to "#e0e0e0"
#' @param title_color Text color for the title. Defaults to "white"
#' @param subtitle_color Text color the the subtitle. Defaults to "#8e8e93"
#' @param caption_color Text color the the caption. Defaults to be equal to the subtitle
#' @param tier_label_color Text color for the tier labels. Defaults to be equal to the title
#' @param logo_type What logo should be used for each team ("main", "scoreboard", or "dot")? Defaults to "main"
#' @return A plot object created with [ggplot2::ggplot()].
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(dplyr, warn.conflicts = FALSE)
#' teams <- valid_team_names()
#' # remove conference logos from this example
#' teams <- teams[!teams %in% c("AL", "NL", "MLB")]
#'
#' # Build the team tiers data frame
#' # This is completely random!
#' df <- data.frame(
#'   tier_no = sample(1:5, length(teams), replace = TRUE),
#'   team_abbr = teams
#' ) %>%
#'   dplyr::group_by(tier_no) %>%
#'   dplyr::mutate(tier_rank = sample(1:n(), n()))
#'
#' # Plot team tiers
#' mlb_team_tiers(df)
#'
#' # Create a combined tier which is useful for tiers with lots of teams that
#' # should be split up in two or more rows. This is done by setting an empty
#' # string for the tier 5 description and removing the tier separation line
#' # below tier number 4.
#' # This example also shows how to turn off the subtitle and add a caption
#' mlb_team_tiers(df,
#'                subtitle = NULL,
#'                caption = "This is the caption",
#'                tier_desc = c("1" = "World Series",
#'                              "2" = "Very Good",
#'                              "3" = "Medium",
#'                              "4" = "A Combined Tier",
#'                              "5" = ""),
#'                no_line_below_tier = 4)
#'
#' # For the development of the tiers, it can be useful to turn off logo image
#' # rendering as this can take quite a long time. By setting `devel = TRUE`, the
#' # logo images are replaced by team abbreviations which is much faster
#' mlb_team_tiers(df,
#'                tier_desc = c("1" = "World Series",
#'                              "2" = "Very Good",
#'                              "3" = "",
#'                              "4" = "A Combined Tier",
#'                              "5" = ""),
#'                no_line_below_tier = c(2, 4),
#'                devel = TRUE)
#' }
#' @export
mlb_team_tiers <- function(data,
                           title = "MLB Team Tiers",
                           subtitle = "Created with the #mlbplotR Tiermaker",
                           caption = NULL,
                           tier_desc = c("1" = "World Series",
                                         "2" = "Very Good",
                                         "3" = "Medium",
                                         "4" = "Bad",
                                         "5" = "Tankathon",
                                         "6" = "",
                                         "7" = ""),
                           presort = FALSE,
                           alpha = 1,
                           width = 0.075,
                           no_line_below_tier = NULL,
                           devel = FALSE,
                           background_color = "#1e1e1e",
                           line_color = "#e0e0e0",
                           title_color = "white",
                           subtitle_color = "#8e8e93",
                           caption_color = subtitle_color,
                           tier_label_color = title_color,
                           logo_type = "main"){

  rlang::check_installed("sjmisc", "to build the mlbplotR team tiers.")

  required_vars <- c("tier_no", "team_abbr")

  if (!all(required_vars %in% names(data))){
    cli::cli_abort("The data frame {.var data} has to include the variables {.var {required_vars}}!")
  }

  if (!logo_type %in% c("main", "scoreboard", "dot")){
    cli::cli_abort("The parameter {.var logo_type} has to be either \"main\", \"scoreboard\", or \"dot\"")
  }

  bg <- background_color
  lines <- line_color

  tiers <- sort(unique(data$tier_no))
  tierlines <- tiers[!tiers %in% no_line_below_tier] + 0.5
  tierlines <- c(min(tiers) - 0.5, tierlines)

  if (isTRUE(presort)){
    data <- data %>%
      dplyr::group_by(.data$tier_no) %>%
      dplyr::arrange(.data$team_abbr) %>%
      dplyr::mutate(tier_rank = 1:dplyr::n()) %>%
      dplyr::ungroup()
  }

  if (!"tier_rank" %in% names(data)){
    data <- data %>%
      dplyr::group_by(.data$tier_no) %>%
      dplyr::mutate(tier_rank = 1:dplyr::n()) %>%
      dplyr::ungroup()
  }

  data$team_abbr <- mlbplotR::clean_team_abbrs(as.character(data$team_abbr), keep_non_matches = FALSE)

  p <- ggplot2::ggplot(data, ggplot2::aes(y = .data$tier_no, x = .data$tier_rank)) +
    ggplot2::geom_hline(yintercept = tierlines, color = lines)

  if (isFALSE(devel) & logo_type == "main") {
    p <- p + mlbplotR::geom_mlb_logos(ggplot2::aes(team_abbr = .data$team_abbr), width = width, alpha = alpha)
  } else if (isFALSE(devel) & logo_type == "scoreboard") {
    p <- p + mlbplotR::geom_mlb_scoreboard_logos(ggplot2::aes(team_abbr = .data$team_abbr), width = width, alpha = alpha)
  } else if (isFALSE(devel) & logo_type == "dot") {
    p <- p + mlbplotR::geom_mlb_dot_logos(ggplot2::aes(team_abbr = .data$team_abbr), width = width, alpha = alpha)
  } else if (isTRUE(devel)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$team_abbr), color = tier_label_color)
  }

  p <- p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(add = 0.1),
      limits = rev(c(min(tiers) - 0.5, max(tiers) + 0.5)),
      breaks = rev(tiers),
      labels = function(x) sjmisc::word_wrap(tier_desc[x], 15),
      trans = "reverse"
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_minimal(base_size = 11.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = title_color, face = "bold"),
      plot.subtitle = ggplot2::element_text(color = subtitle_color),
      plot.caption = ggplot2::element_text(color = caption_color, hjust = 1),
      plot.title.position = "plot",
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(color = tier_label_color, face = "bold", size = ggplot2::rel(1.1)),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = bg, color = bg),
      panel.background = ggplot2::element_rect(fill = bg, color = bg)
    ) +
    NULL

  p
}










#' Create MLB Player Tiers
#'
#' @description This function sets up a ggplot to visualize MLB player tiers.
#'   Adapted from [`nflplotR`](https://nflplotr.nflverse.com/reference/nfl_team_tiers.html)
#'
#' @param data A data frame that has to include the variables `tier_no` (the
#'   number of the tier starting from the top tier no. 1) and `player_id` (the
#'   player's MLBAM/Savant ID). If data includes the variable `tier_rank`,
#'   these ranks will be used within each tier. Otherwise, if `presort = FALSE`,
#'   the function will assume that data is already sorted and if `presort = TRUE`,
#'   teams will be sorted alphabetically within tiers.
#' @param title The title of the plot. If `NULL`, it will be omitted.
#' @param subtitle The subtitle of the plot. If `NULL`, it will be omitted.
#' @param caption The caption of the plot. If `NULL`, it will be omitted.
#' @param tier_desc A named vector consisting of the tier descriptions. The names
#'   must equal the tier numbers from `tier_no`
#' @param presort If `FALSE` (the default) the function assumes that the teams
#'   are already sorted within the tiers. Will otherwise sort alphabetically.
#' @param alpha The alpha channel of the logos, i.e. transparency level, as a
#'   numerical value between 0 and 1. Defaults to 1
#' @param width The desired width of the logo in `npc` (Normalised Parent Coordinates). A typical size is 0.1.
#' @param no_line_below_tier Vector of tier numbers. The function won't draw tier
#'   separation lines below these tiers. This is intended to be used for tiers
#'   that shall be combined (see examples).
#' @param devel Determines if headshots shall be rendered. If `FALSE` (the default),
#'   headshots will be rendered on each run. If `TRUE` the player ids will be
#'   plotted instead of the logos. This is much faster and helps with the plot
#'   development.
#' @param background_color Background color for the plot. Defaults to "#1e1e1e"
#' @param line_color Line color for the plot. Defaults to "#e0e0e0"
#' @param title_color Text color for the title. Defaults to "white"
#' @param subtitle_color Text color the the subtitle. Defaults to "#8e8e93"
#' @param caption_color Text color the the caption. Defaults to be equal to the subtitle
#' @param tier_label_color Text color for the tier labels. Defaults to be equal to the title
#' @param na_headshot_to_logo Should NA/non-matches return the MLB logo instead of a grayed out blank headshot? Defaults to `TRUE`
#' @return A plot object created with [ggplot2::ggplot()].
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(dplyr, warn.conflicts = FALSE)
#' player_ids <- load_headshots() |>
#'   head(35) |>
#'   pull(savant_id)
#'
#' # Build the player tiers data frame
#' # This is completely random!
#' df <- data.frame(
#'   tier_no = sample(1:5, length(player_ids), replace = TRUE),
#'   player_id = player_ids
#' ) %>%
#'   dplyr::group_by(tier_no) %>%
#'   dplyr::mutate(tier_rank = sample(1:n(), n()))
#'
#' # Plot player tiers
#' mlb_player_tiers(df)
#'
#' # Create a combined tier which is useful for tiers with lots of players that
#' # should be split up in two or more rows. This is done by setting an empty
#' # string for the tier 5 description and removing the tier separation line
#' # below tier number 4.
#' # This example also shows how to turn off the subtitle and add a caption
#' mlb_player_tiers(df,
#'                  subtitle = NULL,
#'                  caption = "This is the caption",
#'                  tier_desc = c("1" = "MVP Candidates",
#'                                "2" = "Very Good",
#'                                "3" = "Medium",
#'                                "4" = "A Combined Tier",
#'                                "5" = ""),
#'                  no_line_below_tier = 4)
#'
#' # For the development of the tiers, it can be useful to turn off image
#' # rendering as this can take quite a long time. By setting `devel = TRUE`, the
#' # headshots are replaced by player ids which is much faster
#' mlb_player_tiers(df,
#'                  tier_desc = c("1" = "MVP Candidates",
#'                                "2" = "Very Good",
#'                                "3" = "",
#'                                "4" = "A Combined Tier",
#'                                "5" = ""),
#'                  no_line_below_tier = c(2, 4),
#'                  devel = TRUE)
#' }
#' @export
mlb_player_tiers <- function(data,
                             title = "MLB Player Tiers",
                             subtitle = "Created with the #mlbplotR Tiermaker",
                             caption = NULL,
                             tier_desc = c("1" = "MVP Candidates",
                                           "2" = "Very Good",
                                           "3" = "Medium",
                                           "4" = "Bad",
                                           "5" = "Negative WAR",
                                           "6" = "",
                                           "7" = ""),
                             presort = FALSE,
                             alpha = 1,
                             width = 0.1,
                             no_line_below_tier = NULL,
                             devel = FALSE,
                             background_color = "#1e1e1e",
                             line_color = "#e0e0e0",
                             title_color = "white",
                             subtitle_color = "#8e8e93",
                             caption_color = subtitle_color,
                             tier_label_color = title_color,
                             na_headshot_to_logo = TRUE){

  rlang::check_installed("sjmisc", "to build the mlbplotR player tiers.")

  required_vars <- c("tier_no", "player_id")

  if (!all(required_vars %in% names(data))){
    cli::cli_abort("The data frame {.var data} has to include the variables {.var {required_vars}}!")
  }

  bg <- background_color
  lines <- line_color

  tiers <- sort(unique(data$tier_no))
  tierlines <- tiers[!tiers %in% no_line_below_tier] + 0.5
  tierlines <- c(min(tiers) - 0.5, tierlines)

  if (isTRUE(presort)){
    data <- data %>%
      dplyr::group_by(.data$tier_no) %>%
      dplyr::arrange(.data$player_id) %>%
      dplyr::mutate(tier_rank = 1:dplyr::n()) %>%
      dplyr::ungroup()
  }

  if (!"tier_rank" %in% names(data)){
    data <- data %>%
      dplyr::group_by(.data$tier_no) %>%
      dplyr::mutate(tier_rank = 1:dplyr::n()) %>%
      dplyr::ungroup()
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(y = .data$tier_no, x = .data$tier_rank)) +
    ggplot2::geom_hline(yintercept = tierlines, color = lines)

  if(isFALSE(devel)) p <- p + mlbplotR::geom_mlb_headshots(ggplot2::aes(player_id = .data$player_id), width = width, alpha = alpha)
  if(isTRUE(devel)) p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$player_id), color = tier_label_color)

  p <- p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(add = 0.1),
      limits = rev(c(min(tiers) - 0.5, max(tiers) + 0.5)),
      breaks = rev(tiers),
      labels = function(x) sjmisc::word_wrap(tier_desc[x], 15),
      trans = "reverse"
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_minimal(base_size = 11.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = title_color, face = "bold"),
      plot.subtitle = ggplot2::element_text(color = subtitle_color),
      plot.caption = ggplot2::element_text(color = caption_color, hjust = 1),
      plot.title.position = "plot",
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(color = tier_label_color, face = "bold", size = ggplot2::rel(1.1)),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = bg, color = bg),
      panel.background = ggplot2::element_rect(fill = bg, color = bg)
    ) +
    NULL

  p
}
