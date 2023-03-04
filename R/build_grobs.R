# INTERNAL HELPER THAT BUILDS THE GROBS FOR
# GEOM LOGOS AND HEADSHOTS
build_grobs <- function(i, alpha, colour, data, type = c("teams", "light_cap", "dark_cap", "scoreboard", "logo_headshots", "gray_headshots", "path")) {
  make_null <- FALSE
  type <- rlang::arg_match(type)
  if(type == "teams") {
    team_abbr <- data$team_abbr[i]
    image_to_read <- logo_list[[team_abbr]]
    if (is.na(team_abbr)) make_null <- TRUE
  } else if(type == "dark_cap"){
    team_abbr <- data$team_abbr[i]
    image_to_read <- dark_logo_list[[team_abbr]]
    if (is.na(team_abbr)) make_null <- TRUE
  } else if(type == "light_cap"){
    team_abbr <- data$team_abbr[i]
    image_to_read <- light_logo_list[[team_abbr]]
    if (is.na(team_abbr)) make_null <- TRUE
  } else if(type == "scoreboard"){
    team_abbr <- data$team_abbr[i]
    image_to_read <- scoreboard_logo_list[[team_abbr]]
    if (is.na(team_abbr)) make_null <- TRUE
  } else if (type == "path"){
    image_to_read <- data$path[i]
  } else if (type == "logo_headshots") {
    id <- data$player_id[i]
    headshot_map <- load_headshots()
    image_to_read <- headshot_map$espn_headshot[headshot_map$savant_id == id]
    if(length(image_to_read) == 0 || is.na(image_to_read)){
      image_to_read <- na_headshot(TRUE)
    }
  } else if (type == "gray_headshots") {
    id <- data$player_id[i]
    headshot_map <- load_headshots()
    image_to_read <- headshot_map$espn_headshot[headshot_map$savant_id == id]
    if(length(image_to_read) == 0 || is.na(image_to_read)){
      image_to_read <- na_headshot(FALSE)
    }
  }

  if (is.na(make_null)){
    grid <- grid::nullGrob()
  } else if (is.null(alpha)) {
    img <- reader_function(image_to_read)
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w"){
      new <- magick::image_quantize(img, colorspace = 'gray')
    } else{
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(img, opa, col)
    }
    grid <- grid::rasterGrob(new)
  } else if (length(alpha) == 1L) {
    if (as.numeric(alpha) <= 0 || as.numeric(alpha) >= 1) {
      cli::cli_abort("aesthetic {.var alpha} requires a value between {.val 0} and {.val 1}")
    }
    img <- reader_function(image_to_read)
    new <- magick::image_fx(img, expression = paste0(alpha, "*a"), channel = "alpha")
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w"){
      new <- magick::image_quantize(new, colorspace = 'gray')
    } else{
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(new, opa, col)
    }
    grid <- grid::rasterGrob(new)
  } else {
    if (any(as.numeric(alpha) < 0) || any(as.numeric(alpha) > 1)) {
      cli::cli_abort("aesthetics {.var alpha} require values between {.val 0} and {.val 1}")
    }
    img <- reader_function(image_to_read)
    new <- magick::image_fx(img, expression = paste0(alpha[i], "*a"), channel = "alpha")
    col <- colour[i]
    if (!is.null(col) && col %in% "b/w"){
      new <- magick::image_quantize(new, colorspace = 'gray')
    } else{
      opa <- ifelse(is.na(col) || is.null(col), 0, 100)
      col <- ifelse(is.na(col) || is.null(col), "none", col)
      new <- magick::image_colorize(new, opa, col)
    }
    grid <- grid::rasterGrob(new)
  }

  grid$vp <- grid::viewport(
    x = grid::unit(data$x[i], "native"),
    y = grid::unit(data$y[i], "native"),
    width = grid::unit(data$width[i], "npc"),
    height = grid::unit(data$height[i], "npc"),
    just = c(data$hjust[i], data$vjust[i]),
    angle = data$angle[i],
    name = paste("geom_mlb.panel", data$PANEL[i],
                 "row", i,
                 sep = "."
    )
  )

  grid$name <- paste("mlb.grob", i, sep = ".")

  grid
}

reader_function <- function(img){
  if(is.factor(img)) img <- as.character(img)
  if(is.raw(img) || tools::file_ext(img) != "svg"){
    magick::image_read(img)
  } else if(tools::file_ext(img) == "svg"){
    magick::image_read_svg(img)
  }
}
