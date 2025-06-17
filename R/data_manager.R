#' @title R6 Class to store & process data.
#' @description
#' An R6 Class to communicate between shiny modules
#'
#' @import R6
#'

DataManager <- R6::R6Class(
  "DataManager",
  public = list(
    #' @field horoscope_datetime (`POSIXct()`)\cr
    #' Date & time of the horoscope. Default to current datetime.
    horoscope_datetime = Sys.time(),
    #' @field horoscope_timezone (`character()`)\cr
    #' Time zone. Default to Asia/Taipei
    horoscope_timezone = "Asia/Taipei",
    #' @field horoscope_city (`character()`)\cr
    #' City with longitude & latitude. Default to Taipei City
    horoscope_city = NULL,
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
    chart_name = "Transits",
    #' @field selected_planets (`character()`)\cr
    #' name of the chart
    selected_planets = c("sun","moon","mercury","venus","mars","jupiter","saturn","uranus","neptune","pluto",
                         "chiron","mean_node","asc","mc","vertex"),
    #' @field aspect_table
    #' a data frame of aspects
    aspect_table = NULL,

    #' @description
    #' Initalize the R6 class using the original shiny.i18n Translator class initialize function
    #' furthermore, initialize horoscope city, calculate planatery positions & draw charts
    #'
    #' @param translation_csvs_path the path to the folder where the csv files for translation are stored
    #' @param separator_csv csv separator
    #'
    initialize = function(){

      self$horoscope_city <- cities$city [1]
      self$horoscope_country <- cities$country [cities$city %in% self$horoscope_city]
      self$update_chart()

    },

    #' @description
    #' update horoscope. Calculate planetary positions, draw chart
    #'
    update_chart = function(){

      self$planet_position <- calculate_planet_position(self$horoscope_datetime, self$horoscope_timezone, self$horoscope_city)
      data <- self$planet_position$planetary_position
      data <- data[(row.names(data) %in% self$selected_planets),]
      self$aspect_table <- calculate_aspect(data)
      self$planet_position$planetary_position <- data
      self$chart <- draw_whole_sign_chart(data, self$chart_name, self$horoscope_datetime, self$horoscope_city, self$horoscope_country, self$aspect_table)

    }
  )
)
