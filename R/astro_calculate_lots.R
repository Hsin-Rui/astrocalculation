#' Calculate greek lots
#'
#' @param planetary_position data.frame of planetary position
#' @param planetary_conditions data.frame of planetary conditions (output of get_planetary_conditions)
#'
#' @return a data.frame with seven greek lots (lot of spirit, fortune, necessity, eros, courage, victory, nemesis)
#'
calculate_greek_lots <- function(planetary_position, planetary_conditions) {

  is_diurnal <- planetary_conditions$is_in_sect [match("sun", row.names(planetary_conditions))]

  asc_degree <- planetary_position$deg [match("asc", row.names(planetary_position))]
  sun_degree <- planetary_position$deg [match("sun", row.names(planetary_position))]
  moon_degree <- planetary_position$deg [match("moon", row.names(planetary_position))]
  mercury_degree <- planetary_position$deg [match("mercury", row.names(planetary_position))]
  venus_degree <- planetary_position$deg [match("venus", row.names(planetary_position))]
  mars_degree <- planetary_position$deg [match("mars", row.names(planetary_position))]
  jupiter_degree <- planetary_position$deg [match("jupiter", row.names(planetary_position))]
  saturn_degree <- planetary_position$deg [match("saturn", row.names(planetary_position))]

  lot_of_spirit <- calculate_lot_of_spirit(asc_degree, sun_degree, moon_degree, is_diurnal)
  lot_of_fortune <- calculate_lot_of_fortune(asc_degree, sun_degree, moon_degree, is_diurnal)

  lot_of_necessity <- calculate_lot_of_necessity(asc_degree, lot_of_fortune, mercury_degree, is_diurnal)
  lot_of_eros <- calculate_lot_of_eros(asc_degree, lot_of_spirit, venus_degree, is_diurnal)
  lot_of_courage <- calculate_lot_of_courage(asc_degree, lot_of_fortune, mars_degree, is_diurnal)
  lot_of_victory <- calculate_lot_of_victory(asc_degree, lot_of_spirit, jupiter_degree, is_diurnal)
  lot_of_nemesis <- calculate_lot_of_nemesis(asc_degree, lot_of_fortune, saturn_degree, is_diurnal)

  deg <- c(lot_of_spirit, lot_of_fortune, lot_of_necessity, lot_of_eros, lot_of_courage, lot_of_victory, lot_of_nemesis)
  names(deg) <- c("spirit", "fortune", "necessity", "eros", "courage", "victory", "nemesis")

  sign <- sapply(deg, find_sign)
  deg_in_sign <- as.integer(deg - (sign-1)*30)
  min_in_sign <- as.integer(((deg - (sign-1)*30) - deg_in_sign)*60)
  sec_in_sign <- as.integer((((deg - (sign-1)*30) - deg_in_sign)*60 - min_in_sign)*60)
  planet_glyphs <- c("a","b","c","d","e","f","g")
  planet_color <- zodiac_sign_color[sign]
  font_glyphs <- "AstroParts"
  font_size <- 4.8

  greek_lots <-
    data.frame(
    deg=deg,
    speed=0,
    sign=sign,
    deg_in_sign=deg_in_sign,
    min_in_sign=min_in_sign,
    sec_in_sign=sec_in_sign,
    planet_glyphs=planet_glyphs,
    planet_color=planet_color,
    font_glyphs=font_glyphs,
    font_size =font_size
  )

  row.names(greek_lots) <- names(deg)

  return(greek_lots)

}

#' Calculate lot of fortune
#'
#' @param asc_degree degree of ASC (0-360)
#' @param sun_degree degree of sun (0-360)
#' @param moon_degree degree of moon (0-360)
#' @param is_diurnal If TRUE, sun above horizon
#'
#' @return degree of lot of fortune (0-360)
#'

calculate_lot_of_fortune <- function(asc_degree, sun_degree, moon_degree, is_diurnal) {

  if (isTRUE(is_diurnal)) {
    distance <- (moon_degree - sun_degree) %% 360
  } else {
    distance <- (sun_degree - moon_degree) %% 360
  }
  (asc_degree + distance) %% 360
}

#' Calculate lot of spirit
#'
#' @param asc_degree degree of ASC (0-360)
#' @param sun_degree degree of sun (0-360)
#' @param moon_degree degree of moon (0-360)
#' @param is_diurnal If TRUE, sun above horizon
#'
#' @return degree of lot of spirit (0-360)
#'

calculate_lot_of_spirit <- function(asc_degree, sun_degree, moon_degree, is_diurnal) {

  if (isTRUE(is_diurnal)) {
    distance <- (sun_degree - moon_degree) %% 360
  } else {
    distance <- (moon_degree - sun_degree) %% 360
  }
  (asc_degree + distance) %% 360
}

#' Calculate lot of necessity (Greek lot of mercury)
#'
#' @param asc_degree degree of ASC (0-360)
#' @param lot_of_fortune degree of lot of fortune (0-360)
#' @param mercury_degree degree of mercury (0-360)
#' @param is_diurnal If TRUE, sun above horizon
#'
#' @return degree of lot of necessity (0-360)
#'
calculate_lot_of_necessity <- function(asc_degree, lot_of_fortune, mercury_degree, is_diurnal) {

  if (isTRUE(is_diurnal)) {
    distance <- (lot_of_fortune - mercury_degree) %% 360
  } else {
    distance <- (mercury_degree - lot_of_fortune) %% 360
  }
  (asc_degree + distance) %% 360

}

#' Calculate lot of eros (Greek lot of venus)
#'
#' @param asc_degree degree of ASC (0-360)
#' @param lot_of_spirit degree of lot of spirit (0-360)
#' @param venus_degree degree of venus (0-360)
#' @param is_diurnal If TRUE, sun above horizon
#'
#' @return degree of lot of eros (0-360)
#'
calculate_lot_of_eros <- function(asc_degree, lot_of_spirit, venus_degree, is_diurnal) {

  if (isTRUE(is_diurnal)) {
    distance <- (lot_of_spirit - venus_degree) %% 360
  } else {
    distance <- (venus_degree - lot_of_spirit) %% 360
  }
  (asc_degree + distance) %% 360

}

#' Calculate lot of courage (Greek lot of mars)
#'
#' @param asc_degree degree of ASC (0-360)
#' @param lot_of_fortune degree of lot of fortune (0-360)
#' @param mars_degree degree of mars (0-360)
#' @param is_diurnal If TRUE, sun above horizon
#'
#' @return degree of lot of courage (0-360)
#'
calculate_lot_of_courage <- function(asc_degree, lot_of_fortune, mars_degree, is_diurnal) {

  if (isTRUE(is_diurnal)) {
    distance <- (lot_of_fortune - mars_degree) %% 360
  } else {
    distance <- (mars_degree - lot_of_fortune) %% 360
  }
  (asc_degree + distance) %% 360

}

#' Calculate lot of Victory (Greek lot of Jupiter)
#'
#' @param asc_degree degree of ASC (0-360)
#' @param lot_of_spirit degree of lot of spirit (0-360)
#' @param jupiter_degree degree of jupiter (0-360)
#' @param is_diurnal If TRUE, sun above horizon
#'
#' @return degree of lot of victory (0-360)
#'
calculate_lot_of_victory <- function(asc_degree, lot_of_spirit, jupiter_degree, is_diurnal) {

  if (isTRUE(is_diurnal)) {
    distance <- (lot_of_spirit - jupiter_degree) %% 360
  } else {
    distance <- (jupiter_degree - lot_of_spirit) %% 360
  }
  (asc_degree + distance) %% 360

}

#' Calculate lot of nemesis (Greek lot of satrun)
#'
#' @param asc_degree degree of ASC (0-360)
#' @param lot_of_fortune degree of lot of fortune (0-360)
#' @param saturn_degree degree of saturn (0-360)
#' @param is_diurnal If TRUE, sun above horizon
#'
#' @return degree of lot of saturn (0-360)
#'
calculate_lot_of_nemesis <- function(asc_degree, lot_of_fortune, saturn_degree, is_diurnal) {

  if (isTRUE(is_diurnal)) {
    distance <- (lot_of_fortune - saturn_degree) %% 360
  } else {
    distance <- (saturn_degree - lot_of_fortune) %% 360
  }
  (asc_degree + distance) %% 360

}
