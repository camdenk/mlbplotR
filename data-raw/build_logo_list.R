# Save raw logos in internal data for more speed
teams_colors_logos <- load_mlb_teams()

logo_list <- lapply(teams_colors_logos$team_espn_abbr, function(x){
  url <- teams_colors_logos$team_logo_espn[teams_colors_logos$team_espn_abbr == x]
  curl::curl_fetch_memory(url)$content
})

logo_list <- rlang::set_names(logo_list, teams_colors_logos$team_espn_abbr)

primary_colors <- teams_colors_logos$team_color
names(primary_colors) <- teams_colors_logos$team_espn_abbr

secondary_colors <- teams_colors_logos$team_color2
names(secondary_colors) <- teams_colors_logos$team_espn_abbr

logo_urls <- teams_colors_logos$team_logo_espn
names(logo_urls) <- teams_colors_logos$team_espn_abbr


teams_colors_logos <- readRDS("./data-raw/MLB_Colors_Logos.rds")



usethis::use_data(teams_logos_colors, logo_list, primary_colors,
                  secondary_colors,
                  logo_urls, internal = TRUE, overwrite = TRUE)
