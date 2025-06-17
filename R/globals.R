#' Global variable: a vector of characters representing 12 zodiac signs
#' 
#' @importFrom utils globalVariables
#' 

zodiac_sign <- c("a", #aries
                 "s", #taurus
                 "d", #gemini
                 "f", #cancer
                 "g", #leo
                 "h", #virgo
                 "j", #libra
                 "k", #scorpio
                 "l", #sagittarius
                 "z", #capricorn
                 "x", #aquarius
                 "c") #pisces

globals <- utils::globalVariables("zodiac_sign")

#' Global variable: a list of Swiss Ephemeris objects
#' 
#' @importFrom utils globalVariables
#' @import swephR
#' 

SE <- swephR::SE
globals <- utils::globalVariables("SE")

#' Define global variable: dataset cities
#' 
#' @importFrom utils globalVariables
#' 
globals <- utils::globalVariables("cities")

#' Global variable: colors of sign
#' 
#' @importFrom utils globalVariables
#' 

zodiac_sign_color <- rep(c("red","darkgreen", "darkgoldenrod2", "blue"), 3)

globals <- utils::globalVariables("zodiac_sign_color")
