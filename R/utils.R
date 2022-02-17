logo_html <- function(team_abbr, type = c("height", "width"), size = 15){
  type <- rlang::arg_match(type)
  url <- logo_urls[team_abbr]
  sprintf("<img src='%s' %s = '%s'>", url, type, size)
}


headshot_html <- function(player_id, type = c("height", "width"), size = 25){
  type <- rlang::arg_match(type)
  headshot_map <- load_headshots()
  player_id <- ifelse(player_id %in% headshot_map$savent_id, player_id, "NA_ID")
  headshot_map <- rbind(
    headshot_map,
    list(savant_id = "NA_ID", espn_headshot = na_headshot())
  )
  joined <- merge(
    data.frame(savant_id = player_id),
    headshot_map,
    by = "savant_id",
    all.x = TRUE,
    sort = FALSE
  )
  url <- joined$espn_headshot
  url <- ifelse(grepl(".png", url), url, paste0(url, ".png"))
  sprintf("<img src='%s' %s = '%s'>", url, type, size)
}


rds_from_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)

  if (inherits(load, "try-error")) {
    warning(paste0("Failed to readRDS from <", url, ">"), call. = FALSE)
    return(data.table::data.table())
  }

  data.table::setDT(load)
  return(load)
}


is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

load_headshots <- function() rds_from_url("https://github.com/camdenk/Data/raw/main/Player%20IDs.rds")

na_headshot <- function() "https://a.espncdn.com/combiner/i?img=/i/teamlogos/leagues/500/mlb.png"