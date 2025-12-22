#' @title R6 Class to store & process data.
#' @description
#' An R6 Class to communicate between shiny modules
#'
#' @import R6
#' @export
#'

DataManager <- R6::R6Class(
  "DataManager",
  # 1. Public fields ------------------------------------------
  public = list(
    ## 1-1. horoscope input ####
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

    ## 1-2. calculation results ####
    planet_position = NULL,
    #' @field chart (`list()`)\cr
    #' #' ggplot object
    chart = NULL,
    #' @field chart_name (`character()`)\cr
    #' name of the chart
    chart_name = NULL,

    #' @field aspect_table
    #' a data frame of aspects
    aspect_table = NULL,

    ## 1-3. Calculation config ####
    #' @field selected_planets (`character()`)\cr
    #' name of the chart
    selected_planets = c("sun","moon","mercury","venus","mars","jupiter","saturn","uranus","neptune","pluto",
                         "chiron","mean_node","asc","mc","vertex"),

    ## 1-4. User auth fileds ####

    #' @field pool
    #' postgres SQL database connection
    pool = NULL,
    #' @field user_id
    #' ID to identify the user. If NULL, user is Guest
    user_id = NULL,
    #' @field user_profile
    #' The 1-row data frame of self

    ## 1-5. User data fields ####
    user_profile = NULL,
    #' @field user_library
    #' The data frame of the user's saved charts
    user_library = NULL,

    #' @description
    #' Initialize manager. If user_id is provided, connect to DB and load profile.
    #' If not, default to Guest/Transit mode.
    #' @param user_id Optional Azure Object ID
    #'
    initialize = function(user_id = NULL){

      self$horoscope_datetime <- Sys.time()
      self$horoscope_timezone <- "Asia/Taipei"
      self$horoscope_city <- "Taipei"
      self$horoscope_longitude <- 121.52639
      self$horoscope_latitude <- 25.05306
      self$horoscope_country <- "Taiwan"
      self$chart_name <- "Transits"

      # Check Auth
      if (!is.null(user_id)) {
        self$user_id <- user_id
        self$pool <- connect_postgres_db()

        # Load Data using Service Function
        self$refresh_user_data()
      }

      self$update_chart()

    },

    # 2. Methods: State Management ---------------------
    #' @description Refresh profile and library from DB
    refresh_user_data = function() {
      if (is.null(self$user_id)) return()

      # Call external service functions
      self$user_profile <- db_get_profile(self$pool, self$user_id)
      self$user_library <- db_get_library(self$pool, self$user_id)
    },

    #' @description Save Profile Wrapper
    save_user_profile = function(data_list) {
      if (is.null(self$user_id)) stop("Guest cannot save profile")

      # Call external service
      db_save_profile(self$pool, self$user_id, data_list)

      # Refresh local state
      self$refresh_user_data()
    },

    #' @description Save Library Entry Wrapper
    save_chart_to_library = function(data_list, entity_id = NULL) {
      if (is.null(self$user_id)) stop("Guest cannot save to library")

      db_save_library_entry(self$pool, self$user_id, data_list, entity_id)
      self$refresh_user_data()
    },

    #' @description Load a chart from Profile or Library into the Calculation Engine
    #' @param source "profile" or "library"
    #' @param library_id Optional ID if source is library
    load_chart_to_view = function(source = "profile", library_id = NULL) {

      target_row <- NULL

      if (source == "profile") {
        target_row <- self$user_profile
        self$chart_name <- target_row$display_name
      } else if (source == "library" && !is.null(library_id)) {
        target_row <- self$user_library[self$user_library$entity_id == library_id, ]
        self$chart_name <- target_row$name
      }

      if (!is.null(target_row) && nrow(target_row) > 0) {

        # TIMEZONE HANDLING
        # 1. Get the UTC timestamp from DB
        db_ts <- target_row$birth_timestamp
        # 2. Get the target timezone string
        target_tz <- target_row$timezone

        # 3. Convert UTC -> Local Time for Display/Calculation
        # We use lubridate::with_tz to force the display time to match the user's city
        self$horoscope_datetime <- lubridate::with_tz(db_ts, tzone = target_tz)

        self$horoscope_datetime <- target_row$birth_timestamp
        self$horoscope_timezone <- target_row$timezone
        self$horoscope_city     <- target_row$city_name
        self$horoscope_country  <- target_row$country
        self$horoscope_latitude <- target_row$lat
        self$horoscope_longitude<- target_row$lng

        self$update_chart()
      }
    },

    # 3. Methods: Calculation ------------------------------------

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

    },

    #' @description Cleanup
    finalize = function() {
      if (!is.null(self$pool)) close_postgres_db(self$pool)
    }
  )
)
