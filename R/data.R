#' Cities data
#'
#' Cities having population > 1000 along with country name. Latitude and longitude are included in names for astrological calculation.
#' 
#'
#' @format ## `cities`
#' A data frame with 145,536 rows and 2 columns:
#' \describe{
#'   \item{country}{Country name}
#'   \item{city}{name of city, lat: latitude, lng: longitude.}
#'   \item{tz}{timezone}
#'   \item{population}{population of the city}
#'   \item{big_city}{boolean, population > 300000}
#'   
#' }
#' @source <https://www.geonames.org/>
#' @source <https://api.opencube.tw/twzipcode>
"cities"