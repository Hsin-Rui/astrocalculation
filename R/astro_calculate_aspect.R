#' Check if two planets have aspects (within 3 degrees / 13 degrees for moon)
#' 
#' @param distance a matrix of distance between planets
#'

has_aspect <- function(distance){
  
  distance <- abs(distance)
  
  asc <- which(row.names(distance) %in% "asc")
  mc <- which(row.names(distance) %in% "mc")
  vertex <- which(row.names(distance) %in% "vertex")
  
  # orb for all planets but moon = 3 degree
  res <- distance < 3 
  
  # orb for moon = 13 degree; orb for asc/mc/vertex still 3 degree, also with moon
  res [c(-asc, -mc, -vertex),2] <- distance[c(-asc, -mc, -vertex),2] < 13 
  res [2,c(-asc, -mc, -vertex)] <- distance[2,c(-asc, -mc, -vertex)] < 3 
  
  for(i in 1:ncol(distance)) res [i,i:ncol(distance)] <- FALSE # create an asymmetric matrix
  
  res <- data.frame(res)
  res$planet <- names(res)

  return(res)
}

#' Calculate distance between planets
#' 
#' @param deg a vector of degrees (planet position)
#' @param planets a character vector of planet names
#' 

calculate_distance <- function(deg, planets){
 
  distance <- matrix(nrow = length(deg), ncol = length(deg))
   
  for (i in 1:length(deg)) distance[i, ] <- deg[i]- deg
  
  distance <- abs(distance)
  distance[distance > 195] <- abs(distance[distance > 195] - 360)
  
  rownames(distance) <- planets
  colnames(distance) <- planets
  
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
  
  x %>%
    tidyr::gather("planet2","aspect", -planet) %>%
    dplyr::filter(aspect) %>%
    dplyr::mutate(aspect=asp_name)
  
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
  
  distance <- calculate_distance(data$deg, row.names(data))
  
  is_conjunction <- has_aspect(distance)
  is_sextile <- has_aspect(abs(distance-60))
  is_square <- has_aspect(abs(distance-90))
  is_trine <- has_aspect(abs(distance-120))
  is_opposition <- has_aspect(abs(distance-180))
  
  is_conjuction <- get_aspect_edge_list(is_conjunction, "conjunction")
  is_sextile <- get_aspect_edge_list(is_sextile, "sextile")
  is_square <- get_aspect_edge_list(is_square, "square")
  is_trine <- get_aspect_edge_list(is_trine, "trine")
  is_opposition <- get_aspect_edge_list(is_opposition, "opposition")
  
  result <- rbind(is_conjuction, is_sextile, is_square, is_trine, is_opposition)
  
  data$planet <- row.names(data)
  planet_degree <- 
    data %>% 
    dplyr::select(planet, deg, speed) %>% 
    dplyr::mutate(new_deg=deg+speed) %>% 
    dplyr::select(-speed)
  
  result <- 
    result %>%
    dplyr::left_join(planet_degree, by="planet") %>%
    dplyr::left_join(planet_degree, by=c("planet2"="planet"), suffix=c("_p1","_p2")) %>%
    dplyr::mutate(distance1 = abs(deg_p1-deg_p2),
                  distance2 = abs(new_deg_p1-new_deg_p2),
                  aspect_num = case_when(aspect == "conjunction" ~ 0,
                                         aspect == "sextile" ~ 60,
                                         aspect == "square" ~ 90,
                                         aspect == "trine" ~ 120,
                                         aspect == "opposition" ~ 180)) %>%
    dplyr::mutate(distance1 = case_when(distance1 > 195 ~ abs(distance1-360), TRUE ~ distance1),
                  distance2 = case_when(distance2 > 195 ~ abs(distance2-360), TRUE ~ distance2)) %>%
    dplyr::mutate(orb1 = distance1 - aspect_num,
                  orb2 = distance2 - aspect_num,
                  separation="applying")
  
  result$separation [abs(result$orb2) > abs(result$orb1)] <- "separating"
  result$separation [result$orb1 > 0 & result$orb2 < 0] <- "applying"
  result$separation [result$orb1 < 0 & result$orb2 > 0] <- "applying"
  result$separation [result$planet %in% c("asc", "mc", "vertex")] <- ""
  result$separation [result$planet2 %in% c("asc", "mc", "vertex")] <- ""

  result <- 
    result %>%
    dplyr::select(planet, planet2, aspect, orb1, separation, deg_p1, deg_p2)
  
  return(result)
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