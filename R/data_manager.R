#' @title R6 Class to store & process data.
#' @description
#' An R6 Class to communicate between shiny modules
#'
#' @import R6
#' @export
#'

DataManager <- R6::R6Class(
  "DataManager",
  public = list(
    #' @field horoscope_datetime (`POSIXct()`)\cr
    #' Date & time of the horoscope. Default to current datetime.
    horoscope_datetime = NULL,
    #' @field horoscope_timezone (`character()`)\cr
    #' Time zone. Default to Asia/Taipei
    horoscope_timezone = NULL,
    #' @field horoscope_city (`character()`)\cr
    #' City. Default to Taipei City
    horoscope_city = NULL,
    #' @field horoscope_longitude (`numeric()`)\cr
    #' Longitude of the City. Default to 121.52639
    horoscope_longitude = NULL,
    #' @field horoscope_latitude (`numeric()`)\cr
    #' Longitude of the City. Default to 25.05306
    horoscope_latitude = NULL,
    #' @field horoscope_country (`character()`)\cr
    #' Country
    horoscope_country = NULL,
    #' @field planet_position (`list()`)\cr
    #' A list of dataf rame containing planetary positions and house cusps
    planet_position = NULL,
    #' @field chart (`list()`)\cr
    #' #' ggplot object
    chart = NULL,
    #' @field chart_name (`character()`)\cr
    #' name of the chart
    chart_name = NULL,
    #' @field selected_planets (`character()`)\cr
    #' name of the chart
    selected_planets = c("sun","moon","mercury","venus","mars","jupiter","saturn","uranus","neptune","pluto",
                         "chiron","mean_node","asc","mc","vertex"),
    #' @field aspect_table
    #' a data frame of aspects
    aspect_table = NULL,

    #' @description
    #' Initalize initialize horoscope city, calculate planatery positions & draw charts
    #'
    #'
    initialize = function(){

      self$horoscope_datetime <- Sys.time()
      self$horoscope_timezone <- "Asia/Taipei"
      self$horoscope_city <- "Taipei"
      self$horoscope_longitude <- 121.52639
      self$horoscope_latitude <- 25.05306
      self$horoscope_country <- "Taiwan"
      self$chart_name <- "Transits"

      self$update_chart()

    },

    #' @description
    #' update horoscope. Calculate planetary positions, draw chart
    #'
    update_chart = function(){

      self$planet_position <- calculate_planet_position(self$horoscope_datetime, self$horoscope_timezone, self$horoscope_longitude, self$horoscope_latitude)
      data <- self$planet_position$planetary_position
      data <- data[(row.names(data) %in% self$selected_planets),]
      self$aspect_table <- calculate_aspect(data)
      self$planet_position$planetary_position <- data
      self$chart <- draw_whole_sign_chart(data, self$chart_name, self$horoscope_datetime, self$horoscope_city, self$horoscope_country, self$horoscope_timezone, self$aspect_table)

    }
  )
)
