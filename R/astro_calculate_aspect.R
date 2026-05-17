#' Check if two planets have aspects (within 3 degrees / 13 degrees for moon)
#'
#' @param distance a matrix of distance between planets
#'

has_degree_based_aspect <- function(distance){

  # orb for moon = 13 degree; orb for asc/mc/vertex still 3 degree, also with moon
  res <- distance < 13
  res <- distance < 3

  for(i in 1:ncol(distance)) res [i,i:ncol(distance)] <- FALSE # create an asymmetric matrix

  res <- data.frame(res)
  res$planet <- names(res)

  return(res)
}

#' Calculate distance between planets
#'
#' @param data planetary_position
#'

calculate_degree_distance <- function(data){

  distance <- matrix(nrow = length(data$deg), ncol = length(data$deg))

  for (i in 1:length(data$deg)) distance[i, ] <- (data$deg[i]- data$deg) %%360

  rownames(distance) <- row.names(data)
  colnames(distance) <- row.names(data)

  return(distance)
}

#' Calculate edge list of the aspect
#'
#' @param x data frame (adjacency matrix of connection between planets and name of planets)
#' @param asp_name input string for name of the aspect
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#'

get_aspect_edge_list <- function(x, asp_name){

  x |>
    tidyr::gather("planet2","aspect", -planet) |>
    dplyr::filter(aspect) |>
    dplyr::mutate(aspect=asp_name) |>
    dplyr::mutate(
      p1 = pmin(planet, planet2),
      p2 = pmax(planet, planet2)
    ) |>
    dplyr::distinct(p1, p2, aspect) |>
    dplyr::select(planet = p1, planet2 = p2, aspect)

}

#' Calculate conjuction / sextile / sqauare / trine / opposition
#'
#' @param data data frame of planetary position
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#'

calculate_aspect <- function(data){

  inclusion <- c("sun", "moon",
                 "mercury", "venus", "mars",
                 "jupiter", "saturn",
                 "uranus", "neptune", "pluto", "chiron", "mc", "mean_node")

  data <- data[row.names(data) %in% inclusion, ]

  df_sign_based_aspect <- get_sign_based_aspects(data)

  make_edge_list <- function(x) {

    data.frame(df_sign_based_aspect == x) |>
      dplyr::mutate(planet = row.names(df_sign_based_aspect)) |>
      get_aspect_edge_list(asp_name = x) |>
      dplyr::filter(planet != planet2)

  }

  aspects <- c("conjunction", "sextile", "square", "trine", "opposition")

  edge_list_sign_based_aspects <- purrr::map_dfr(aspects, make_edge_list)

  df_degree <-
    data |>
    dplyr::mutate(planet = row.names(data)) |>
    dplyr::select(planet, deg)

  df_speed <-
    data |>
    dplyr::mutate(planet = row.names(data)) |>
    dplyr::select(planet, speed)

  report_result_from_edge_list <- function(x) {

    x |>
      ## get planet degree
      dplyr::left_join(df_degree, by = "planet") |>
      dplyr::rename(deg_p1 = deg) |>
      dplyr::left_join(df_degree, by = c("planet2"="planet")) |>
      dplyr::rename(deg_p2 = deg) |>
      ## calculate orb
      dplyr::mutate(orb1 = deg_p1 - deg_p2) |>
      dplyr::mutate(distance_abs = abs(orb1)) |>
      dplyr::mutate(orb1 = dplyr::case_when(
        aspect == "sextile" ~ distance_abs - 60,
        aspect == "square" ~ distance_abs - 90,
        aspect == "trine" ~ distance_abs - 120,
        aspect == "opp1osition" ~ distance_abs - 180,
        TRUE ~ orb1
      )) |>
      dplyr::mutate(in_three_degree = abs(orb1) < 3,
                    in_thriteen_degree = abs(orb1) < 13) |>
      ## evaluate if should draw aspect line
      dplyr::mutate(draw_line = dplyr::case_when(
        planet == "moon" | planet2 == "moon" ~ in_thriteen_degree,
        TRUE ~ in_three_degree
      )) |>
      dplyr::mutate(draw_line = dplyr::if_else(aspect=="conjunction", FALSE, draw_line)) |>
      ## calculate orb of the next day
      dplyr::left_join(df_speed, by = "planet") |>
      dplyr::rename(speed_p1 = speed) |>
      dplyr::left_join(df_speed, by = c("planet2"="planet")) |>
      dplyr::rename(speed_p2=speed) |>
      dplyr::mutate(deg2_p1 = deg_p1 + speed_p1,
                    deg2_p2 = deg_p2 + speed_p2) |>
      dplyr::mutate(orb2 = deg2_p1 - deg2_p2) |>
      dplyr::mutate(distance2_abs = abs(orb2)) |>
      dplyr::mutate(orb2 = dplyr::case_when(
        aspect == "sextile" ~ distance2_abs - 60,
        aspect == "square" ~ distance2_abs - 90,
        aspect == "trine" ~ distance2_abs- 120,
        aspect == "opposition" ~ distance2_abs - 180,
        TRUE ~ orb2
      )) |>
      ## report applying or separation
      dplyr::mutate(separation = dplyr::case_when(
        abs(orb2) > abs(orb1) ~ "separating",
        abs(orb2) < abs(orb1) ~ "applying")) |>
      dplyr::mutate(separation = dplyr::if_else((orb1 < 0 & orb2 > 0) | (orb2 < 0 & orb1 > 0),
                                                "applying", separation)) |>
      dplyr::select(planet, planet2, aspect, deg_p1, deg_p2, orb1, orb2, separation, draw_line)


  }

  result_sign_based_aspects <- report_result_from_edge_list(edge_list_sign_based_aspects)

  distance <- calculate_degree_distance(data)

  aspects <-
    list(conjunction = 0,
       sextile = 60,
       square = 90,
       trine = 120,
       opposition = 180)

  aspects <-
    lapply(aspects, function(x) {
    data.frame(abs(distance - x) < 3) |>
      dplyr::mutate(planet = row.names(data))
  })

  seven_stars <- c("sun", "moon", "mercury", "venus", "mars", "jupiter", "saturn")

  make_degree_based_edge_list <- function(x) {
    aspects[[x]] |>
      get_aspect_edge_list(x) |>
      dplyr::filter(planet != planet2) |>
      dplyr::filter((!planet %in% seven_stars) | (!planet2 %in% seven_stars))
  }

  edge_list_degree_based_aspects <- purrr::map_dfr(names(aspects), make_degree_based_edge_list)

  result_degree_based_aspects <-
    report_result_from_edge_list(edge_list_degree_based_aspects) |>
    dplyr::mutate(draw_line = dplyr::if_else(aspect == "conjunction", FALSE, TRUE))

  result <- rbind(result_sign_based_aspects, result_degree_based_aspects)

  return(result)
}

#' Find sign based aspects
#' @param data planetary_position

get_sign_based_aspects <- function(data) {

  df_seven_stars <- data [row.names(data) %in% c("sun", "moon", "mercury", "venus", "mars", "jupiter", "saturn"), ]
  planet <- row.names(df_seven_stars)
  df_sign_based_distance <-
    sapply(planet, function(x) calculate_sign_distance(x, data = df_seven_stars))

  df_sign_based_distance[df_sign_based_distance == "0"] <- "conjunction"
  df_sign_based_distance[df_sign_based_distance == "2" |
                         df_sign_based_distance == "10"] <- "sextile"
  df_sign_based_distance[df_sign_based_distance == "3" |
                           df_sign_based_distance == "9"] <- "square"
  df_sign_based_distance[df_sign_based_distance == "4" |
                           df_sign_based_distance == "8"] <- "trine"
  df_sign_based_distance[df_sign_based_distance == "6"] <- "opposition"

  df_sign_based_distance[df_sign_based_distance == "1" |
                         df_sign_based_distance == "11" |
                         df_sign_based_distance == "5" |
                           df_sign_based_distance == "7"] <- ""

  return(df_sign_based_distance)

}

#' Calculate sign distance
#' @param planet name of planet
#' @param data planetary_position. Must have row.names (name of planets)
#'

calculate_sign_distance <- function(planet, data) {

  sign_distance <- (data$sign[row.names(data) %in% planet] - data$sign) %% 12
  names(sign_distance) <- row.names(data)
  return(sign_distance)

}

# r6 <- DataManager$new()
# data <- r6$planet_position$planetary_position

# TODO.2 Orb systems
# Hellenistic orb: moon = 13, other planets = 3
# Renaissance orb
# Modern orb
# Custom orb

# I will implement Hellenistic orb first and then a modern orb system

# TODO.5 enabling modern aspects

# NOTE. this will be an adjacency matrix

#' Filter aspect results to the four major aspects only
#'
#' Reduces the data frame returned by [calculate_aspect()] to rows whose
#' `aspect` column matches one of the four major aspects: conjunction, square,
#' trine, and opposition.  Sextile and any other non-major aspects are excluded.
#'
#' This is the recommended entry point when semantic summaries are required for
#' the major-4 subset, as it pairs with [DescriptionEngine]`$get_aspect_summary()`
#' without needing to filter again at the consumer.
#'
#' @param aspect_result A data frame produced by [calculate_aspect()].  Must
#'   contain at minimum an `aspect` column of character strings.
#' @return A data frame with only rows where `aspect` is one of
#'   `"conjunction"`, `"square"`, `"trine"`, or `"opposition"`.  Column
#'   structure is identical to the input.
#' @export
filter_major_aspects <- function(aspect_result) {
  if (!is.data.frame(aspect_result)) {
    stop("aspect_result must be a data frame.", call. = FALSE)
  }
  if (!"aspect" %in% names(aspect_result)) {
    stop("aspect_result must contain an 'aspect' column.", call. = FALSE)
  }

  major_labels <- c("conjunction", "square", "trine", "opposition")
  aspect_result[aspect_result$aspect %in% major_labels, , drop = FALSE]
}
