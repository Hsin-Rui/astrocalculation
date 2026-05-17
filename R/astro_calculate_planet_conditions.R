#' Convert the degree so that the degree of AC becomes 0
#'
#' @param planet_position a list of two data.frames (planetary_position and house_cusps)
#' @return a list with one named vector (normalized_planet_degree) and one data.frame (normalized_house_cusps)
#'

normalize_degree <- function(planet_position) {

  planet_degree <- planet_position$planetary_position$deg
  names(planet_degree) <- row.names(planet_position$planetary_position)

  asc_degree <- planet_degree[match("asc",names(planet_degree))]

  normalized_degree <- (planet_degree - asc_degree) %% 360

  house_cusps <- planet_position$house_cusps
  normalized_cusps <- (house_cusps - asc_degree) %% 360

  return(list(normalized_planet_degree = normalized_degree,
              normalized_house_cusps = normalized_cusps))

}

#' Determine chart sect
#'
#' @param normalized_planet_degree named vector. The degree of AC is set qs 0.
#' @return a boolean. If diurnal then TRUE, otherwise FALSE
#'

is_diurnal_chart <- function(normalized_planet_degree){

  sun_degree <- normalized_planet_degree [match("sun",names(normalized_planet_degree))]
  is_diurnal <- FALSE
  if (sun_degree > 177 | sun_degree < 3) {
    is_diurnal <- TRUE
  }
  return(is_diurnal)
}

#' Determine chart sect
#'
#' @param sun_degree degree of sun
#' @param mercury_degree degree of mercury
#' @return a boolean. If diurnal then TRUE, otherwise FALSE
#'

get_mercury_sect <- function(sun_degree, mercury_degree) {

  diff <- (mercury_degree - sun_degree) %% 360
  is_diurnal <- diff > 180

  return(is_diurnal)
}

#' Determine if the planets are in the comfortable sect
#'
#' @param is_diurnal boolean. If TRUE, it's a diurnal chart.
#' @param planetary_position a data.frame of planetary position
#'
#' @return a named boolean vector. Names are the planets, boolean indicates if the planet is in sect
#'

is_in_sect <- function(planetary_position, is_diurnal){

  degree <- planetary_position$deg
  sun_degree <- degree[match("sun", row.names(planetary_position))]
  mercury_degree <-  degree[match("mercury", row.names(planetary_position))]

  is_diurnal_mercury <- get_mercury_sect(sun_degree, mercury_degree)

  planets <- c("sun", "moon", "mercury", "venus", "mars", "jupiter", "saturn")
  is_in_sect <- rep(FALSE, 7)
  names(is_in_sect) <- planets

  if (isTRUE(is_diurnal)) {
    is_in_sect[match(c("sun", "jupiter", "saturn"), names(is_in_sect))] <- TRUE
    if (isTRUE(is_diurnal_mercury)) {
      is_in_sect[match(c("mercury"), names(is_in_sect))] <- TRUE
    }
  } else {
      is_in_sect[match(c("moon", "venus", "mars"), names(is_in_sect))] <- TRUE
      if (isFALSE(is_diurnal_mercury)) {
        is_in_sect[match(c("mercury"), names(is_in_sect))] <- TRUE
    }
  }
  return(is_in_sect)
}

#' Determine house placement
#'
#' @param normalized_planet_degree named vector. The degree of AC is set qs 0.
#' @param normalized_house_cusps named vector. The degree of AC is set qs 0.
#' @param house_system whole_sign, placidus, regiomontanus, koch
#'

find_house_placement <- function(normalized_planet_degree, normalized_house_cusps, house_system) {

  # 1. Extract the 12 cusps for the requested house system
  cusps <- normalized_house_cusps[[house_system]]

  # 2. Define the "end" boundary for each house
  # The 12th house ends at the 1st house cusp, so we shift the vector by 1 and wrap the first element to the end.
  next_cusps <- c(cusps[2:12], cusps[1])

  # 3. Define a helper logic to locate a single degree
  get_house <- function(deg) {
    # Condition A: The house does NOT cross the 0-degree line (e.g., starts at 30°, ends at 60°)
    # The planet must be >= the start AND < the end.
    normal_match <- (cusps < next_cusps) & (deg >= cusps & deg < next_cusps)

    # Condition B: The house crosses the 0-degree line (e.g., starts at 330°, ends at 30°)
    # The planet must be >= the start OR < the end.
    wrap_match <- (cusps > next_cusps) & (deg >= cusps | deg < next_cusps)

    # Return the index (1 to 12) where either condition is TRUE
    return(which(normal_match | wrap_match))
  }

  # 4. Apply the helper to all planets
  # sapply will iterate over your named vector and automatically return a named integer vector.
  house_placements <- sapply(normalized_planet_degree, get_house)

  return(house_placements)

}

#'
#' Determine essential dignities
#'
#' @param planetary_position a data.frame of planetary position
#' @return a data.frame of booleans and element
#'
has_essential_dignities <- function(planetary_position) {

  get_element <- function(sign) {
    c("Fire", "Earth", "Air", "Water")[((sign - 1) %% 4) + 1]
  }

  traditional_planets <- c("sun", "moon", "mercury", "venus", "mars", "jupiter", "saturn")

  res <- planetary_position |>
    tibble::rownames_to_column("planet") |>
    dplyr::mutate(
      planet_lower = tolower(planet),
      element = get_element(sign),
      d = deg %% 30,
      is_trad = planet_lower %in% traditional_planets
    ) |>
    dplyr::mutate(
      is_domicile_lord = dplyr::case_when(
        !is_trad ~ FALSE,
        planet_lower == "sun" & sign == 5 ~ TRUE,
        planet_lower == "moon" & sign == 4 ~ TRUE,
        planet_lower == "mercury" & sign %in% c(3, 6) ~ TRUE,
        planet_lower == "venus" & sign %in% c(2, 7) ~ TRUE,
        planet_lower == "mars" & sign %in% c(1, 8) ~ TRUE,
        planet_lower == "jupiter" & sign %in% c(9, 12) ~ TRUE,
        planet_lower == "saturn" & sign %in% c(10, 11) ~ TRUE,
        TRUE ~ FALSE
      ),
      is_exaltation_lord = dplyr::case_when(
        !is_trad ~ FALSE,
        planet_lower == "sun" & sign == 1 ~ TRUE,
        planet_lower == "moon" & sign == 2 ~ TRUE,
        planet_lower == "mercury" & sign == 6 ~ TRUE,
        planet_lower == "venus" & sign == 12 ~ TRUE,
        planet_lower == "mars" & sign == 10 ~ TRUE,
        planet_lower == "jupiter" & sign == 4 ~ TRUE,
        planet_lower == "saturn" & sign == 7 ~ TRUE,
        TRUE ~ FALSE
      ),
      is_triplicity_lord = dplyr::case_when(
        !is_trad ~ FALSE,
        planet_lower == "sun" & element == "Fire" ~ TRUE,
        planet_lower == "moon" & element %in% c("Earth", "Water") ~ TRUE,
        planet_lower == "mercury" & element == "Air" ~ TRUE,
        planet_lower == "venus" & element %in% c("Earth", "Water") ~ TRUE,
        planet_lower == "mars" & element %in% c("Earth", "Water") ~ TRUE,
        planet_lower == "jupiter" & element %in% c("Fire", "Air") ~ TRUE,
        planet_lower == "saturn" & element %in% c("Fire", "Air") ~ TRUE,
        TRUE ~ FALSE
      ),
      is_term_lord = dplyr::case_when(
        !is_trad ~ FALSE,
        planet_lower == "mercury" & (
          (sign == 1 & d >= 12 & d < 20) | (sign == 2 & d >= 8 & d < 14) |
            (sign == 3 & d >= 0 & d < 6) | (sign == 4 & d >= 13 & d < 19) |
            (sign == 5 & d >= 18 & d < 24) | (sign == 6 & d >= 0 & d < 7) |
            (sign == 7 & d >= 6 & d < 14) | (sign == 8 & d >= 11 & d < 19) |
            (sign == 9 & d >= 17 & d < 21) | (sign == 10 & d >= 0 & d < 7) |
            (sign == 11 & d >= 0 & d < 7) | (sign == 12 & d >= 16 & d < 19)
        ) ~ TRUE,
        planet_lower == "venus" & (
          (sign == 1 & d >= 6 & d < 12) | (sign == 2 & d >= 0 & d < 8) |
            (sign == 3 & d >= 12 & d < 17) | (sign == 4 & d >= 7 & d < 13) |
            (sign == 5 & d >= 6 & d < 11) | (sign == 6 & d >= 7 & d < 17) |
            (sign == 7 & d >= 21 & d < 28) | (sign == 8 & d >= 7 & d < 11) |
            (sign == 9 & d >= 12 & d < 17) | (sign == 10 & d >= 14 & d < 22) |
            (sign == 11 & d >= 7 & d < 13) | (sign == 12 & d >= 0 & d < 12)
        ) ~ TRUE,
        planet_lower == "mars" & (
          (sign == 1 & d >= 20 & d < 25) | (sign == 2 & d >= 27 & d < 30) |
            (sign == 3 & d >= 17 & d < 24) | (sign == 4 & d >= 0 & d < 7) |
            (sign == 5 & d >= 24 & d < 30) | (sign == 6 & d >= 21 & d < 28) |
            (sign == 7 & d >= 28 & d < 30) | (sign == 8 & d >= 0 & d < 7) |
            (sign == 9 & d >= 26 & d < 30) | (sign == 10 & d >= 26 & d < 30) |
            (sign == 11 & d >= 20 & d < 25) | (sign == 12 & d >= 19 & d < 28)
        ) ~ TRUE,
        planet_lower == "jupiter" & (
          (sign == 1 & d >= 0 & d < 6) | (sign == 2 & d >= 14 & d < 22) |
            (sign == 3 & d >= 6 & d < 12) | (sign == 4 & d >= 19 & d < 26) |
            (sign == 5 & d >= 0 & d < 6) | (sign == 6 & d >= 17 & d < 21) |
            (sign == 7 & d >= 14 & d < 21) | (sign == 8 & d >= 19 & d < 24) |
            (sign == 9 & d >= 0 & d < 12) | (sign == 10 & d >= 7 & d < 14) |
            (sign == 11 & d >= 13 & d < 20) | (sign == 12 & d >= 12 & d < 16)
        ) ~ TRUE,
        planet_lower == "saturn" & (
          (sign == 1 & d >= 25 & d < 30) | (sign == 2 & d >= 22 & d < 27) |
            (sign == 3 & d >= 24 & d < 30) | (sign == 4 & d >= 26 & d < 30) |
            (sign == 5 & d >= 11 & d < 18) | (sign == 6 & d >= 28 & d < 30) |
            (sign == 7 & d >= 0 & d < 6) | (sign == 8 & d >= 24 & d < 30) |
            (sign == 9 & d >= 21 & d < 26) | (sign == 10 & d >= 22 & d < 26) |
            (sign == 11 & d >= 25 & d < 30) | (sign == 12 & d >= 28 & d < 30)
        ) ~ TRUE,
        TRUE ~ FALSE
      ),
      is_face_lord = dplyr::case_when(
        !is_trad ~ FALSE,
        planet_lower == "sun" & (
          (sign == 1 & d >= 10 & d < 20) | (sign == 3 & d >= 20 & d < 30) |
            (sign == 6 & d >= 0 & d < 10) | (sign == 8 & d >= 10 & d < 20) |
            (sign == 10 & d >= 20 & d < 30)
        ) ~ TRUE,
        planet_lower == "moon" & (
          (sign == 2 & d >= 10 & d < 20) | (sign == 4 & d >= 20 & d < 30) |
            (sign == 7 & d >= 0 & d < 10) | (sign == 9 & d >= 10 & d < 20) |
            (sign == 11 & d >= 20 & d < 30)
        ) ~ TRUE,
        planet_lower == "mercury" & (
          (sign == 2 & d >= 0 & d < 10) | (sign == 4 & d >= 10 & d < 20) |
            (sign == 6 & d >= 20 & d < 30) | (sign == 9 & d >= 0 & d < 10) |
            (sign == 11 & d >= 10 & d < 20)
        ) ~ TRUE,
        planet_lower == "venus" & (
          (sign == 1 & d >= 20 & d < 30) | (sign == 4 & d >= 0 & d < 10) |
            (sign == 6 & d >= 10 & d < 20) | (sign == 8 & d >= 20 & d < 30) |
            (sign == 11 & d >= 0 & d < 10)
        ) ~ TRUE,
        planet_lower == "mars" & (
          (sign == 1 & d >= 0 & d < 10) | (sign == 3 & d >= 10 & d < 20) |
            (sign == 5 & d >= 20 & d < 30) | (sign == 8 & d >= 0 & d < 10) |
            (sign == 10 & d >= 10 & d < 20) | (sign == 12 & d >= 20 & d < 30)
        ) ~ TRUE,
        planet_lower == "jupiter" & (
          (sign == 3 & d >= 0 & d < 10) | (sign == 5 & d >= 10 & d < 20) |
            (sign == 7 & d >= 20 & d < 30) | (sign == 10 & d >= 0 & d < 10) |
            (sign == 12 & d >= 10 & d < 20)
        ) ~ TRUE,
        planet_lower == "saturn" & (
          (sign == 2 & d >= 20 & d < 30) | (sign == 5 & d >= 0 & d < 10) |
            (sign == 7 & d >= 10 & d < 20) | (sign == 9 & d >= 20 & d < 30) |
            (sign == 12 & d >= 0 & d < 10)
        ) ~ TRUE,
        TRUE ~ FALSE
      ),
      is_in_detriment = dplyr::case_when(
        !is_trad ~ FALSE,
        planet_lower == "sun" & sign == 11 ~ TRUE,
        planet_lower == "moon" & sign == 10 ~ TRUE,
        planet_lower == "mercury" & sign %in% c(9, 12) ~ TRUE,
        planet_lower == "venus" & sign %in% c(1, 8) ~ TRUE,
        planet_lower == "mars" & sign %in% c(2, 7) ~ TRUE,
        planet_lower == "jupiter" & sign %in% c(3, 6) ~ TRUE,
        planet_lower == "saturn" & sign %in% c(4, 5) ~ TRUE,
        TRUE ~ FALSE
      ),
      is_in_fall = dplyr::case_when(
        !is_trad ~ FALSE,
        planet_lower == "sun" & sign == 7 ~ TRUE,
        planet_lower == "moon" & sign == 8 ~ TRUE,
        planet_lower == "mercury" & sign == 12 ~ TRUE,
        planet_lower == "venus" & sign == 6 ~ TRUE,
        planet_lower == "mars" & sign == 4 ~ TRUE,
        planet_lower == "jupiter" & sign == 10 ~ TRUE,
        planet_lower == "saturn" & sign == 1 ~ TRUE,
        TRUE ~ FALSE
      )
    ) |>
    dplyr::mutate(
      is_peregrine = is_trad & !(is_domicile_lord | is_exaltation_lord | is_triplicity_lord | is_term_lord | is_face_lord)
    ) |>
    dplyr::select(
      planet, element, is_domicile_lord, is_exaltation_lord, is_triplicity_lord,
      is_term_lord, is_face_lord, is_in_detriment, is_in_fall, is_peregrine
    ) |>
    tibble::column_to_rownames("planet")

  return(res)
}

#' Get comprehensive planetary conditions
#'
#' @param planet_position a list containing two data.frames: `planetary_position` and `house_cusps`
#' @param house_system character string specifying the house system (e.g., "placidus", "whole_sign")
#' @return a data.frame containing house placement, sect status, and essential dignities
#'
get_planetary_conditions <- function(planet_position, house_system) {

  norm_data <- normalize_degree(planet_position)
  norm_planets <- norm_data$normalized_planet_degree
  norm_cusps <- norm_data$normalized_house_cusps

  is_diurnal <- is_diurnal_chart(norm_planets)

  sect_status <- is_in_sect(planet_position$planetary_position, is_diurnal)

  house_placements <- find_house_placement(norm_planets, norm_cusps, house_system)

  dignities_df <- has_essential_dignities(planet_position$planetary_position)

  final_df <- dignities_df |>
    tibble::rownames_to_column("planet") |>
    dplyr::mutate(
      house = house_placements[planet],
      is_in_sect = dplyr::coalesce(sect_status[planet], FALSE)
    ) |>
    dplyr::select(
      planet,
      house,
      is_in_sect,
      element,
      is_domicile_lord,
      is_exaltation_lord,
      is_triplicity_lord,
      is_term_lord,
      is_face_lord,
      is_in_detriment,
      is_in_fall,
      is_peregrine
    ) |>
    tibble::column_to_rownames("planet")

  return(final_df)
}
