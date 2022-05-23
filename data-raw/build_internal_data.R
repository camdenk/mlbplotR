# Save raw logos in internal data for more speed
teams_colors_logos <- readr::read_csv("./data-raw/MLB_Colors_Logos.csv")

logo_list <- lapply(teams_colors_logos$team_primary_abbr, function(x){
  url <- teams_colors_logos$team_logo_espn[teams_colors_logos$team_primary_abbr == x]
  curl::curl_fetch_memory(url)$content
})

logo_list <- rlang::set_names(logo_list, teams_colors_logos$team_primary_abbr)

primary_colors <- teams_colors_logos$team_color
names(primary_colors) <- teams_colors_logos$team_primary_abbr

secondary_colors <- teams_colors_logos$team_color2
names(secondary_colors) <- teams_colors_logos$team_primary_abbr

logo_urls <- teams_colors_logos$team_logo_espn
names(logo_urls) <- teams_colors_logos$team_primary_abbr



scoreboard_logo_list <- lapply(teams_colors_logos$team_primary_abbr, function(x){
  url <- teams_colors_logos$team_scoreboard_logo_espn[teams_colors_logos$team_primary_abbr == x]
  curl::curl_fetch_memory(url)$content
})

scoreboard_logo_list <- rlang::set_names(scoreboard_logo_list, teams_colors_logos$team_primary_abbr)



light_cap_logo_list <- lapply(teams_colors_logos$team_primary_abbr, function(x){
  url <- teams_colors_logos$team_cap_logo_on_light[teams_colors_logos$team_primary_abbr == x]
  #curl::curl_fetch_memory(url)$content
})



light_logo_list <- rlang::set_names(light_cap_logo_list, teams_colors_logos$team_primary_abbr)



dark_cap_logo_list <- lapply(teams_colors_logos$team_primary_abbr, function(x){
  url <- teams_colors_logos$team_cap_logo_on_dark[teams_colors_logos$team_primary_abbr == x]
  #curl::curl_fetch_memory(url)$content
})

dark_logo_list <- rlang::set_names(dark_cap_logo_list, teams_colors_logos$team_primary_abbr)


# Build team name df that'll be used to clean abbreviations before plotting

team_data <- tibble::tribble(~team, ~alternate,
                           "ARI", "ARI",
                           "ATL", "ATL",
                           "BAL", "BAL",
                           "BOS", "BOS",
                           "CHC", "CHC",
                           "CWS", "CWS",
                           "CWS", "CHW",
                           "CIN", "CIN",
                           "CLE", "CLE",
                           "COL", "COL",
                           "DET", "DET",
                           "HOU", "HOU",
                           "KC" , "KC",
                           "KC" , "KCR",
                           "LAA", "LAA",
                           "LAD", "LAD",
                           "MIA", "MIA",
                           "MIL", "MIL",
                           "MIN", "MIN",
                           "NYM", "NYM",
                           "NYY", "NYY",
                           "OAK", "OAK",
                           "PHI", "PHI",
                           "PIT", "PIT",
                           "SD" , "SD",
                           "SD" , "SDP",
                           "SF" , "SF",
                           "SF" , "SFG",
                           "SEA", "SEA",
                           "STL", "STL",
                           "TB" , "TB",
                           "TB" , "TBR",
                           "TEX", "TEX",
                           "TOR", "TOR",
                           "WSH", "WSH",
                           "WSH", "WAS") |>
  dplyr::select(alternate, team) |>
  tibble::deframe()





usethis::use_data(teams_colors_logos, logo_list,
                  primary_colors, secondary_colors,
                  logo_urls, scoreboard_logo_list,
                  light_logo_list, dark_logo_list,
                  team_data,
                  internal = TRUE, overwrite = TRUE)
