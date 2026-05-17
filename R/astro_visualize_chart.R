#' Draw chart template using ggplot
#'
#' @param style chart style (whole sign, chris brennan, quadrant)
#' @param include_limits logical. If TRUE, add default x/y limits to the template
#'
#' @importFrom showtext showtext_auto
#' @importFrom stringr str_extract
#' @import ggplot2
#'
#' @return ggplot object (three possible empty chart templates for further plotting)
#'

draw_chart_template <- function(style=c("whole_sign", "chris_brennan", "quadrant"),
                                include_limits = TRUE){

  style <- match.arg(style)

  ## 1. define x,y for for circles

  outer_circle <- get_circle_coords(length.out=1080)
  outer_circle2 <- get_circle_coords(r=0.9, length.out=1080)
  inner_circle <- get_circle_coords(r=0.5, length.out=1080)
  inner_circle2 <- get_circle_coords(r=0.4, length.out=1080)

  mytheme <- list(
    theme_void() +
      theme(
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background  = element_rect(fill = "white", colour = "white")
      )
  )

  ## 2. define sign division & whole sign house cusps

  equal_division <- seq(from=1, by=1080/12, length.out=12)

  sign_x <- outer_circle$x [equal_division]
  sign_x_end <- outer_circle2$x [equal_division]
  sign_y <- outer_circle$y [equal_division]
  sign_y_end <- outer_circle2$y [equal_division]

  cusps_x <- inner_circle2$x [equal_division]
  cusps_y <- inner_circle2$y [equal_division]

  ## 4. draw chris brennan style chart template

  if(style=="chris_brennan") {

    p_chris_prennan <-
      ggplot()+
      geom_path(aes(x=outer_circle$x,y=outer_circle$y), linewidth=0.3)+
      geom_path(aes(x=outer_circle2$x, y=outer_circle2$y), linewidth=0.3)+
      mytheme+
      coord_equal()+
      geom_segment(aes(x=sign_x, y=sign_y, xend=0, yend=0), color="black", linewidth=0.3)

    if (isTRUE(include_limits)) {
      p_chris_prennan <-
        p_chris_prennan +
        xlim(-1.05, 1.05)+
        ylim(-1.05, 1.05)
    }

    return(p_chris_prennan)

  }

  ## 5. draw common parts of the template

  p_common <-
    ggplot()+
    # draw four circles
    geom_path(aes(x=outer_circle$x,y=outer_circle$y), linewidth=0.3)+
    geom_path(aes(x=outer_circle2$x, y=outer_circle2$y), linewidth=0.3)+
    geom_path(aes(x=inner_circle$x, y=inner_circle$y), linewidth=0.3)+
    geom_path(aes(x=inner_circle2$x, y=inner_circle2$y), linewidth=0.3)+
    # add custom theme (white background etc.)
    mytheme+
    # make coordinates x & y equally long
    coord_equal()

  if (isTRUE(include_limits)) {
    p_common <-
      p_common +
      xlim(-1.10, 1.10)+
      ylim(-1.05, 1.05)
  }

  ## 6. draw whole sign chart template

  if(style=="whole_sign") {

    ## define x, y of house number for whole sign chart
    house_position <- get_circle_coords(r=0.45, length.out=360)
    house_x <- house_position$x[seq(from=15, by=30, length.out=12)]
    house_y <- house_position$y[seq(from=15, by=30, length.out=12)]
    house_number <- as.character(1:12)


    p_whole_sign <-
      p_common +
      geom_segment(aes(x=sign_x, y=sign_y, xend=sign_x_end, yend=sign_y_end), color="black", linewidth=0.3) +
      # house division
      geom_segment(aes(x=cusps_x, y=cusps_y, xend=sign_x_end, yend=sign_y_end), color="grey50", linewidth=0.2) +
      # house number
      geom_text(aes(x=house_x, y=house_y, label=c(7:12, 1:6)), size=3.5)

    return(p_whole_sign)

  }

  ## 7. draw template for whole sign house chart

  if(style=="quadrant"){

    p_quadrant <-
      p_common+
      geom_segment(aes(x=cusps_x[c(1,7)], y=cusps_y[c(1,7)], xend=c(0.9, -0.9), yend=sign_y[c(1,7)]),
                   color="black", linewidth=0.4) +
      geom_segment(aes(x=c(1,-1), y=0, xend=c(1.05, -1.05), yend=0),
                   color="black", linewidth=0.4)

    return(p_quadrant)

  }

}

#' Convert planet degree to theta
#'
#' @param deg a vector of planetary degrees
#' @param starting_deg a number of degree for theta which corresponds to the x=-0.9, y=0 of the chart
#'

convert_degree_to_theta <- function(deg, starting_deg){

  new_deg <- (deg - starting_deg) %% 360

  theta <- as.integer(new_deg /360 *36000) + 1

  return(theta)

}

#' Get chart information labels
#'
#' @param date a datetime (POSIXct) object. Time of the chart
#' @param city a character string. name of the city
#' @param country a character string. name of the country
#' @param timezone a string. Timezone of the chart
#'
#' @return list with formatted chart date, time, city and country labels
#'
get_chart_info_labels <- function(date, city, country, timezone) {
  list(
    formatted_date = format(date, "%Y/%m/%d, %A"),
    time = paste(format(date, "%T"), timezone),
    city = city,
    country = country
  )
}

#' Add chart information layers
#'
#' @param p a ggplot object
#' @param chart_name a character string. name of the chart
#' @param date a datetime (POSIXct) object. Time of the chart
#' @param city a character string. name of the city
#' @param country a character string. name of the country
#' @param timezone a string. Timezone of the chart
#'
#' @return ggplot object with chart information layers
#'
add_chart_info_layers <- function(p, chart_name, date, city, country, timezone) {
  chart_info <- get_chart_info_labels(date, city, country, timezone)

  p +
    ggplot2::geom_text(
      ggplot2::aes(x = -0.99),
      y = 1.31,
      label = chart_name,
      vjust = "inward",
      hjust = "inward",
      size = 4,
      fontface = "bold"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = c(-0.99, -0.99, -0.99, -0.99),
        y = c(1.23, 1.16, 1.09, 1.02),
        label = c(
          chart_info$formatted_date,
          chart_info$time,
          chart_info$city,
          chart_info$country
        )
      ),
      vjust = "inward",
      hjust = "inward",
      size = 3.5
    )
}

#' Prepare planetary drawing coordinates
#'
#' @param planet_position a data frame (obtained by calculate_planet_position)
#' @param starting_deg a number of degree for theta which corresponds to the x=-0.9, y=0 of the chart
#'
#' @return list with planetary drawing coordinates
#'
prepare_planet_layers_data <- function(planet_position, starting_deg) {
  selected_elements <- row.names(planet_position)
  coords_planet_points <- get_circle_coords(r = 0.9, length.out = 36000)
  coords_planet_glyphs <- get_circle_coords(r = 0.82, length.out = 36000)
  coords_lines <- get_circle_coords(r = 0.87, length.out = 36000)

  planet_position$planet_theta <- convert_degree_to_theta(planet_position$deg, starting_deg)
  planet_position$planet <- row.names(planet_position)

  new_theta <- optmize_planet_position(planet_position$planet_theta, planets = selected_elements)
  planet_position <- planet_position |>
    dplyr::left_join(data.frame(planet_glyphs = names(new_theta), new_theta), by = "planet_glyphs")

  replaced <- planet_position$planet_theta != planet_position$new_theta
  planet_sign_coord <- get_circle_coords(r = 0.66, length.out = 36000)
  deg_coord <- get_circle_coords(r = 0.73, length.out = 36000)
  min_coord <- get_circle_coords(r = 0.6, length.out = 36000)
  retrograde_coord <- get_circle_coords(r = 0.56, length.out = 36000)

  list(
    planet_position = planet_position,
    planet_x_on_circle = coords_planet_points$x[planet_position$planet_theta],
    planet_y_on_circle = coords_planet_points$y[planet_position$planet_theta],
    planet_x_glyphs = coords_planet_glyphs$x[planet_position$new_theta],
    planet_y_glyphs = coords_planet_glyphs$y[planet_position$new_theta],
    lines_x = coords_planet_points$x[planet_position$planet_theta][replaced],
    lines_y = coords_planet_points$y[planet_position$planet_theta][replaced],
    lines_end_x = coords_lines$x[planet_position$new_theta][replaced],
    lines_end_y = coords_lines$y[planet_position$new_theta][replaced],
    planet_sign_x = planet_sign_coord$x[planet_position$new_theta],
    planet_sign_y = planet_sign_coord$y[planet_position$new_theta],
    deg = paste(planet_position$deg_in_sign, "\u00b0", sep = ""),
    deg_x = deg_coord$x[planet_position$new_theta],
    deg_y = deg_coord$y[planet_position$new_theta],
    minute = paste(planet_position$min_in_sign, "'", sep = ""),
    min_x = min_coord$x[planet_position$new_theta],
    min_y = min_coord$y[planet_position$new_theta],
    degree_color = dplyr::case_when(planet_position$speed < 0 ~ "darkred", TRUE ~ "black"),
    retrograde_x = retrograde_coord$x[planet_position$new_theta][planet_position$speed < 0],
    retrograde_y = retrograde_coord$y[planet_position$new_theta][planet_position$speed < 0]
  )
}

#' Prepare aspect line coordinates
#'
#' @param aspect_table a data frame of aspects
#' @param starting_deg a number of degree for theta which corresponds to the x=-0.9, y=0 of the chart
#'
#' @return data frame with aspect drawing coordinates
#'
prepare_aspect_layers_data <- function(aspect_table, starting_deg) {
  aspect_table <-
    aspect_table |>
      dplyr::filter(draw_line)

  aspect_table$theta_p1 <- convert_degree_to_theta(aspect_table$deg_p1, starting_deg)
  aspect_table$theta_p2 <- convert_degree_to_theta(aspect_table$deg_p2, starting_deg)

  coords_aspect_lines <- get_circle_coords(0.4, length.out = 36000)
  aspect_table$x <- coords_aspect_lines$x[aspect_table$theta_p1]
  aspect_table$y <- coords_aspect_lines$y[aspect_table$theta_p1]
  aspect_table$x_end <- coords_aspect_lines$x[aspect_table$theta_p2]
  aspect_table$y_end <- coords_aspect_lines$y[aspect_table$theta_p2]
  aspect_table$color <- dplyr::recode(
    aspect_table$aspect,
    "sextile" = "deepskyblue2",
    "square" = "brown1",
    "trine" = "deepskyblue4",
    "opposition" = "darkred"
  )

  aspect_table
}

#' Add planetary layers
#'
#' @param p a ggplot object
#' @param planet_position a data frame (obtained by calculate_planet_position)
#' @param starting_deg a number of degree for theta which corresponds to the x=-0.9, y=0 of the chart
#' @param x_limits numeric vector with x limits
#' @param y_limits numeric vector with y limits
#'
#' @return ggplot object with planet layers
#'
add_planet_layers <- function(p, planet_position, starting_deg, x_limits, y_limits) {
  planet_data <- prepare_planet_layers_data(planet_position, starting_deg)
  planet_position <- planet_data$planet_position

  p +
    ggplot2::geom_point(
      ggplot2::aes(x = planet_data$planet_x_on_circle, y = planet_data$planet_y_on_circle),
      color = planet_position$planet_color
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = planet_data$planet_x_glyphs,
        y = planet_data$planet_y_glyphs,
        label = planet_position$planet_glyphs
      ),
      family = planet_position$font_glyphs,
      size = planet_position$font_size
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = planet_data$lines_x,
        xend = planet_data$lines_end_x,
        y = planet_data$lines_y,
        yend = planet_data$lines_end_y
      ),
      color = "grey65"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = planet_data$planet_sign_x,
        y = planet_data$planet_sign_y,
        label = zodiac_sign[planet_position$sign]
      ),
      family = "HamburgSymbols",
      color = planet_position$planet_color
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = planet_data$deg_x, y = planet_data$deg_y, label = planet_data$deg),
      size = 3.1,
      color = planet_data$degree_color
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = planet_data$min_x, y = planet_data$min_y, label = planet_data$minute),
      size = 2.9,
      color = planet_data$degree_color
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = planet_data$retrograde_x, y = planet_data$retrograde_y, label = "R"),
      size = 2.4,
      color = "darkred"
    ) +
    ggplot2::xlim(x_limits) +
    ggplot2::ylim(y_limits)
}

#' Add aspect layers
#'
#' @param p a ggplot object
#' @param aspect_table a data frame of aspects
#' @param starting_deg a number of degree for theta which corresponds to the x=-0.9, y=0 of the chart
#'
#' @return ggplot object with aspect layers
#'
add_aspect_layers <- function(p, aspect_table, starting_deg) {
  aspect_table <- prepare_aspect_layers_data(aspect_table, starting_deg)

  p +
    ggplot2::geom_segment(
      data = aspect_table,
      ggplot2::aes(x = x, xend = x_end, y = y, yend = y_end),
      color = aspect_table$color
    )
}

#' Add whole sign zodiac layers
#'
#' @param p a ggplot object
#' @param asc_sign integer. Ascendant sign number
#'
#' @return ggplot object with zodiac sign glyph layers
#'
add_whole_sign_zodiac_layers <- function(p, asc_sign) {
  sign_order <- define_sign_order(asc_sign)
  circle <- get_circle_coords(r = 0.95, length.out = 156)

  sign_x <- circle$x[seq(from = 7, by = 13, length.out = 12)]
  sign_y <- circle$y[seq(from = 7, by = 13, length.out = 12)]
  sign_x <- sign_x[define_sign_order(7)]
  sign_y <- sign_y[define_sign_order(7)]

  p +
    ggplot2::geom_text(
      ggplot2::aes(x = sign_x, y = sign_y, label = zodiac_sign[sign_order]),
      family = "HamburgSymbols",
      size = 6,
      color = zodiac_sign_color[sign_order]
    )
}

#' Prepare quadrant house drawing data
#'
#' @param house_cusps a data frame of house cusps
#' @param house_system a character string. Quadrant house system
#'
#' @return list with quadrant chart coordinates and axis metadata
#'
prepare_quadrant_house_layers_data <- function(house_cusps, house_system) {
  quadrant_start <- house_cusps$placidus[1]
  whole_sign_start <- house_cusps$whole_sign[1]
  ic <- house_cusps$placidus[4]
  degree_difference <- quadrant_start - whole_sign_start
  first_line <- as.integer((30 - degree_difference) * 3)

  outer_circle <- get_circle_coords(length.out = 1080)
  outer_circle2 <- get_circle_coords(r = 0.9, length.out = 1080)
  outer_circle3 <- get_circle_coords(r = 1.05, length.out = 1080)
  inner_circle2 <- get_circle_coords(r = 0.4, length.out = 1080)

  sign_border_position <- seq(from = first_line, by = 90, length.out = 12)

  ac_ic_difference <- (ic - quadrant_start) %% 360

  ic_point <- 541 + (ac_ic_difference) * 3
  mc_point <- (ac_ic_difference) * 3

  first_symbol_position <- first_line - 45
  symbol_position <- seq(first_symbol_position, by = 90, length.out = 12)
  symbol_position <- dplyr::if_else(symbol_position > 1080, symbol_position - 1080, symbol_position)
  symbol_position <- dplyr::if_else(symbol_position < 0, symbol_position + 1080, symbol_position)

  circle <- get_circle_coords(r = 0.95, length.out = 1080)
  sign_x <- circle$x[symbol_position]
  sign_y <- circle$y[symbol_position]
  sign_x <- sign_x[define_sign_order(7)]
  sign_y <- sign_y[define_sign_order(7)]

  quadrant_house_cusps <- house_cusps[house_system] |> unlist()
  quadrant_house_cusps <- (quadrant_house_cusps - quadrant_start) %% 360

  quadrant_house_cusps_position <- as.integer((quadrant_house_cusps[c(2, 3, 5, 6, 8, 9, 11, 12)]) * 3)

  position <-
    tibble::as_tibble(quadrant_house_cusps) |>
    dplyr::mutate(next_cusp = dplyr::coalesce(dplyr::lead(value), 360)) |>
    dplyr::mutate(diff = next_cusp - value) |>
    dplyr::mutate(position = as.integer((value + (diff / 2)) * 3)) |>
    dplyr::select(position) |>
    dplyr::pull()

  house_number_circle <- get_circle_coords(r = 0.45, length.out = 1080)

  list(
    quadrant_start = quadrant_start,
    mc_point = mc_point,
    sign_x_start = outer_circle2$x[sign_border_position],
    sign_x_end = outer_circle$x[sign_border_position],
    sign_y_start = outer_circle2$y[sign_border_position],
    sign_y_end = outer_circle$y[sign_border_position],
    axix_x_start = inner_circle2$x[c(ic_point, mc_point)],
    axix_x_end = outer_circle2$x[c(ic_point, mc_point)],
    axix_y_start = inner_circle2$y[c(ic_point, mc_point)],
    axix_y_end = outer_circle2$y[c(ic_point, mc_point)],
    marker_x_start = outer_circle$x[c(ic_point, mc_point)],
    marker_x_end = outer_circle3$x[c(ic_point, mc_point)],
    marker_y_start = outer_circle$y[c(ic_point, mc_point)],
    marker_y_end = outer_circle3$y[c(ic_point, mc_point)],
    sign_x = sign_x,
    sign_y = sign_y,
    cusps_x_start = inner_circle2$x[quadrant_house_cusps_position],
    cusps_x_end = outer_circle2$x[quadrant_house_cusps_position],
    cusps_y_start = inner_circle2$y[quadrant_house_cusps_position],
    cusps_y_end = outer_circle2$y[quadrant_house_cusps_position],
    house_number_x = house_number_circle$x[position],
    house_number_y = house_number_circle$y[position]
  )
}

#' Add quadrant house layers
#'
#' @param p a ggplot object
#' @param asc_sign integer. Ascendant sign number
#' @param house_cusps a data frame of house cusps
#' @param house_system a character string. Quadrant house system
#'
#' @return list with ggplot object and quadrant metadata
#'
add_quadrant_house_layers <- function(p, asc_sign, house_cusps, house_system) {
  sign_order <- define_sign_order(asc_sign)
  house_data <- prepare_quadrant_house_layers_data(house_cusps, house_system)

  p <- p +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = house_data$sign_x_start,
        y = house_data$sign_y_start,
        xend = house_data$sign_x_end,
        yend = house_data$sign_y_end
      ),
      color = "black",
      linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = house_data$axix_x_start,
        y = house_data$axix_y_start,
        xend = house_data$axix_x_end,
        yend = house_data$axix_y_end
      ),
      color = "black",
      linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = house_data$marker_x_start,
        y = house_data$marker_y_start,
        xend = house_data$marker_x_end,
        yend = house_data$marker_y_end
      ),
      color = "black",
      linewidth = 0.4
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = house_data$sign_x, y = house_data$sign_y, label = zodiac_sign[sign_order]),
      family = "HamburgSymbols",
      size = 6,
      color = zodiac_sign_color[sign_order]
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = house_data$cusps_x_start,
        y = house_data$cusps_y_start,
        xend = house_data$cusps_x_end,
        yend = house_data$cusps_y_end
      ),
      color = "grey",
      linewidth = 0.4
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = house_data$house_number_x,
        y = house_data$house_number_y,
        label = c(7:12, 1:6)
      ),
      size = 3.5
    )

  list(plot = p, house_data = house_data)
}

#' Add quadrant axis label layers
#'
#' @param p a ggplot object
#' @param axis_position a data frame with ASC and MC positions
#' @param mc_point numeric. MC position index on quadrant circle
#'
#' @return ggplot object with ASC and MC label layers
#'
add_quadrant_axis_label_layers <- function(p, axis_position, mc_point) {
  asc_mc_circle <- get_circle_coords(length.out = 1080, r = 1.1)
  mc_x <- asc_mc_circle$x[mc_point]
  mc_y <- asc_mc_circle$y[mc_point]

  asc_mc_deg_x <- asc_mc_circle$x[c(555, mc_point - 25)]
  asc_mc_deg_y <- asc_mc_circle$y[c(555, mc_point - 25)]
  asc_mc_deg <- paste0(axis_position$deg_in_sign, "\u00b0", " ", axis_position$min_in_sign, "'", sep = "")

  p +
    ggplot2::geom_text(
      ggplot2::aes(x = c(-1.10, mc_x), y = c(0, mc_y), label = c("P", "Q")),
      family = "AstroDotBasic",
      size = 6.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = asc_mc_deg_x, y = asc_mc_deg_y, label = asc_mc_deg),
      size = 3.1,
      color = "black"
    )
}

#' Draw natal chart
#'
#' @param planet_position a data frame (obtained by calculate_planet_position)
#' @param chart_name a character string. name of the chart
#' @param date a datetime (POSIXct) object. Time of the chart
#' @param city a character string. name of the city
#' @param country a character string. name of the country
#' @param timezone a string. Timezone of the chart
#' @param aspect_table a data frame of aspects
#' @param house_cusps a data frame of house cusps
#' @param house_system a character string. House system to draw
#'
#' @return ggplot object
#'
draw_natal_chart <- function(planet_position,
                             chart_name,
                             date,
                             city,
                             country,
                             timezone,
                             aspect_table,
                             house_cusps = NULL,
                             house_system = c("whole_sign", "placidus", "koch", "regiomontanus")) {
  house_system <- match.arg(house_system)
  asc_sign <- find_sign(planet_position$deg[row.names(planet_position) %in% "asc"])

  if (house_system == "whole_sign") {
    p <- draw_chart_template(style = "whole_sign", include_limits = FALSE)
    p <- add_whole_sign_zodiac_layers(p, asc_sign)

    starting_sign <- find_opposite_sign(asc_sign)
    starting_deg <- (starting_sign - 1) * 30

    p <- add_planet_layers(
      p,
      planet_position = planet_position,
      starting_deg = starting_deg,
      x_limits = c(-1.00, 1.00),
      y_limits = c(-1.06, 1.32)
    )
    p <- add_chart_info_layers(p, chart_name, date, city, country, timezone)
    p <- add_aspect_layers(p, aspect_table, starting_deg)

    return(suppressMessages(p))
  }

  if (is.null(house_cusps)) {
    stop("house_cusps is required for quadrant charts", call. = FALSE)
  }

  p <- draw_chart_template(style = "quadrant", include_limits = FALSE)
  quadrant_layers <- add_quadrant_house_layers(p, asc_sign, house_cusps, house_system)
  p <- quadrant_layers$plot
  house_data <- quadrant_layers$house_data

  starting_deg <- (house_data$quadrant_start - 180) %% 360

  axis_position <- planet_position[row.names(planet_position) %in% c("asc", "mc"), ]
  planet_position <- planet_position[!row.names(planet_position) %in% c("asc", "mc"), ]

  p <- add_planet_layers(
    p,
    planet_position = planet_position,
    starting_deg = starting_deg,
    x_limits = c(-1.12, 1.05),
    y_limits = c(-1.06, 1.32)
  )
  p <- add_chart_info_layers(p, chart_name, date, city, country, timezone)
  p <- add_aspect_layers(p, aspect_table, starting_deg)

  suppressMessages(add_quadrant_axis_label_layers(p, axis_position, house_data$mc_point))
}

#' Visualize chart in whole sign style
#'
#' @param planet_position a data frame (obtained by calculate_planet_position)
#' @param chart_name a character string. name of the chart
#' @param date a datetime (POXIXct) object. Time of the chart
#' @param city a character string. name of the city
#' @param country a character string. name of the country
#' @param timezone a string. Timezone of the chart
#' @param aspect_table a data frame of aspects
#'

draw_whole_sign_chart <- function(planet_position, chart_name, date, city, country, timezone, aspect_table){

  suppressMessages(
    draw_natal_chart(
      planet_position = planet_position,
      chart_name = chart_name,
      date = date,
      city = city,
      country = country,
      timezone = timezone,
      aspect_table = aspect_table,
      house_system = "whole_sign"
    )
  )
}

#' Draw quadrant chart
#' @param planet_position a data frame (obtained by calculate_planet_position)
#' @param house_cusps a data frame (obtained by calculate_planet_position)
#' @param house_system can be either "placidus", "koch", or "regiomontanus"
#' @param chart_name a character string. name of the chart
#' @param date a datetime (POXIXct) object. Time of the chart
#' @param city a character string. name of the city
#' @param country a character string. name of the country
#' @param timezone a string. Timezone of the chart
#' @param aspect_table a data frame of aspects
#'

draw_quadrant_chart <- function(planet_position,
                                  house_cusps,
                                  house_system=c("placidus", "koch", "regiomontanus"),
                                  chart_name,
                                  date,
                                  city,
                                  country,
                                  timezone,
                                  aspect_table){

  house_system <- match.arg(house_system)

  suppressMessages(
    draw_natal_chart(
      planet_position = planet_position,
      chart_name = chart_name,
      date = date,
      city = city,
      country = country,
      timezone = timezone,
      aspect_table = aspect_table,
      house_cusps = house_cusps,
      house_system = house_system
    )
  )

}

#' Render a natal chart ggplot to a temporary JPEG file
#'
#' Convenience function for async workers: writes the chart to a temporary JPEG
#' and returns only the absolute file path. No ggplot object is returned,
#' preventing large objects from crossing \code{future} process boundaries.
#'
#' @param planet_position Data frame of planetary positions (rows = bodies,
#'   as returned by \code{calculate_planet_position()$planetary_position}).
#' @param chart_name Character. Name shown on the chart.
#' @param date POSIXct. Datetime of the chart.
#' @param city Character. City name.
#' @param country Character. Country name.
#' @param timezone Character. IANA timezone string.
#' @param aspect_table Data frame of aspects from \code{calculate_aspect()}.
#' @param width Integer. JPEG width in pixels. Default 600.
#' @param height Integer. JPEG height in pixels. Default 600.
#' @param pointsize Integer. Base font size. Default 24.
#' @param res Integer. Resolution in ppi. Default 96.
#' @param house_cusps Optional data frame of house cusps. Required for quadrant
#'   house systems.
#' @param house_system Character. One of \code{"whole_sign"}, \code{"placidus"},
#'   \code{"koch"}, or \code{"regiomontanus"}. Default \code{"whole_sign"}.
#'
#' @return Absolute path (character) to the generated JPEG tempfile.
#'
#' @importFrom grDevices jpeg dev.off
render_natal_chart_to_file <- function(
    planet_position, chart_name, date, city, country, timezone, aspect_table,
    width = 600, height = 600, pointsize = 24, res = 96,
    house_cusps = NULL,
    house_system = c("whole_sign", "placidus", "koch", "regiomontanus")) {

  house_system <- match.arg(house_system)
  outfile <- tempfile(fileext = ".jpg")
  grDevices::jpeg(outfile, width = width, height = height,
                  pointsize = pointsize, res = res, bg = "white")
  on.exit(grDevices::dev.off())
  print(
    draw_natal_chart(
      planet_position = planet_position,
      chart_name = chart_name,
      date = date,
      city = city,
      country = country,
      timezone = timezone,
      aspect_table = aspect_table,
      house_cusps = house_cusps,
      house_system = house_system
    )
  )
  outfile
}
