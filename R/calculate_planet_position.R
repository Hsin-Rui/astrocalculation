#' This function calculate planetary positions and other chart elements by calling functions in swephR
#' planetary positions are calculated by calling swe_calc_ut()
#' ASC, MC, Vertex and house cusps are calculated by calling swe_houses_ex()
#' swe_calc_ut returns 6 double parameters: 1. longitude, 2. latitude, 3. distance, 4. speed in longitude, 5. speed in latitude, 6. speed in distance
#'
#' @param date A POSIXct class date time string
#' @param timezone A string of time zone. It has to be time zone that lubridate recognizes. Default is "Asia/Taipei"
#' @param city A character string of city along with longitude and latitude (acquired in cities dataset)
#'
#' @importFrom swephR swe_set_ephe_path swe_calc_ut swe_houses_ex
#' @importFrom here here
#' @importFrom stringr str_extract
#' @importFrom dplyr case_when
#'

calculate_planet_position <- function(date, timezone, city){

  se_path <- (here::here("inst/se_data"))
  swe_set_ephe_path(se_path)
  # call date_to_jd convert date to julian day
  jd <- date_to_jd(date = date, timezone = timezone)

  # calculate planatory position & speed
  elements <- list(SE$SUN, SE$MOON, SE$MERCURY, SE$VENUS, SE$MARS, SE$JUPITER, SE$SATURN, SE$URANUS, SE$NEPTUNE, SE$PLUTO, SE$CHIRON, SE$MEAN_NODE, SE$TRUE_NODE)

  calculate_degree_and_speed <- function(x){swe_calc_ut(jd, x, SE$FLG_SPEED)$xx[c(1,4)]}
  position <- data.frame(sapply(elements, calculate_degree_and_speed))
  names(position) <- c("sun", "moon", "mercury", "venus", "mars", "jupiter", "saturn", "uranus", "neptune", "pluto", "chiron", "mean_node", "true_node")

  # calculate ASC & MC
  longitude <- as.numeric(stringr::str_extract(city, "^.{2,}lng: (.{2,}),", group=1))
  latitude <- as.numeric(stringr::str_extract(city, "lat: (.{1,})$", group=1))

  ascension <- swe_houses_ex(jd, 0, geolat = latitude, geolon = longitude, hsys = "W")$ascmc
  asc <- ascension[1] # first element is ASC
  mc <- ascension[2] # second element is MC
  vertex <- ascension[4] # 4th element is Vertex

  position$asc <- c(asc, 0)
  position$mc <- c(mc, 0)
  position$vertex <- c(vertex,0)

  # calculate house cusps  ## whole sign = W, equal = "E", Placidus = "P", Koch = "K"
  systems <- list("W", "E", "P", "K")

  calculate_house_cusps <- function(x){swe_houses_ex(jd, 0, geolat = latitude, geolon = longitude, hsys = x)$cusps[2:13]}

  house_cusps <- data.frame(sapply(systems, calculate_house_cusps))
  names(house_cusps) <- c("whole_sign", "equal", "placidus", "koch")

  # find sign and put on information for chart visualization
  position <- t(position)
  position <- data.frame(deg=position[,1], speed=position[,2])

  position$sign <- sapply(as.list(position$deg), find_sign) # find sign for each element
  position$deg_in_sign <- as.integer(position$deg - (position$sign-1)*30)
  position$min_in_sign <- as.integer(((position$deg - (position$sign-1)*30) - position$deg_in_sign)*60)
  position$sec_in_sign <- as.integer((((position$deg - (position$sign-1)*30) - position$deg_in_sign)*60 - position$min_in_sign)*60)

  position$planet_glyphs <- convert_planet_symbol(row.names(position))
  position$planet_color <- zodiac_sign_color[position$sign]

  position$font_gpyphs <-  dplyr::case_when(position$planet_glyphs == "Vx" ~ "sans", TRUE ~ "AstroDotBasic")
  position$font_size <- dplyr::case_when(position$planet_glyphs == "Vx" ~ 4.5, TRUE ~ 6.5)

  return(list(planetary_position=position ,house_cusps=house_cusps))
}

