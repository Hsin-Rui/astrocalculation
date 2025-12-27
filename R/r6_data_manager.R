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

    ## 1-5. User data fields ####
    #' @field user_profile
    #' The 1-row data frame of self
    user_profile = NULL,
    #' @field user_library
    #' The data frame of the user's saved charts
    user_library = NULL,

    #' @field logger
    #' R6 logger object
    logger = NULL, #

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

      # 2. Connect to DB (MUST RUN EVERY TIME)
      self$pool <- connect_postgres_db()

      self$logger <- Logger$new(self$pool)

      # Check Auth
      if (!is.null(user_id)) {
        self$user_id <- user_id

        # Load Data using Service Function
        self$refresh_user_data()
        self$logger$log_info("LOGIN", "User restored from session", user_id)

      } else {

        self$logger$log_info("INIT", "Guest session started")

      }

      self$update_chart()

    },

    # 2. Methods: Auth Integration ---------------------

    #' @description Register a new user
    #' @param user_id user ID (user chosen)
    #' @param email user ID
    #' @param password user password
    #' @param display_name User name to be displayed in chart
    #' @return The new user_id if successful, throws error otherwise
    register = function(user_id, email, password, display_name) {
      # Delegates to the logic function
      new_id <- auth_register_user(self$pool, user_id, email, password, display_name)
      return(new_id)
    },

    #' @description Validate Session Token
    #' @param token The session cookie string
    #' @return user_id if valid, NULL otherwise
    validate_session = function(token) {
      if (is.null(token) || token == "") return(NULL)

      # Use the internal pool
      valid_user_id <- auth_validate_session(self$pool, token)

      if (!is.null(valid_user_id)) {
        self$user_id <- valid_user_id
        self$refresh_user_data()
      }

      return(valid_user_id)
    },

    #' @description Login User (Flexible)
    #' @param login_id User email OR User ID
    #' @param password User password
    #' @return Session Token (String) if success, NULL if failed
    login = function(login_id, password) {
      verified_id <- auth_verify_user(self$pool, login_id, password)

      if (!is.null(verified_id)) {
        self$user_id <- verified_id
        self$refresh_user_data()

        # Generate Session Token
        token <- auth_create_session(self$pool, verified_id)
        return(token)
      } else {
        return(NULL)
      }
    },

    #' @description Logout
    logout = function() {
      self$user_id <- NULL
      self$user_profile <- NULL
      self$user_library <- NULL
      # Reset chart to default transit? Optional.
    },

    # 3. Methods: State Management ---------------------

    #' @description Refresh profile and library from DB
    refresh_user_data = function() {
      if (is.null(self$user_id)) return()

      # Call external service functions
      self$user_profile <- db_get_profile(self$pool, self$user_id)
      self$user_library <- db_get_library(self$pool, self$user_id)
    },

    #' @description Save Profile Wrapper
    #' @param data_list A list of variables. It should contain display_name (chart name), country, city_name and birth_timestamp
    save_user_profile = function(data_list) {
      if (is.null(self$user_id)) stop("Guest cannot save profile")

      # Call external service
      db_save_profile(self$pool, self$user_id, data_list)

      # Refresh local state
      self$refresh_user_data()
    },

    #' @description Save Library Entry Wrapper
    #' @param data_list A list of variables. It should contain display_name (chart name), country, city_name and birth_timestamp, note
    #' @param entity_id UUID of a specific data entry in the user personal database
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

    #' @description Delete a chart from library
    #' @param entity_id uuid of the chart
    delete_chart_from_library = function(entity_id) {
      if (is.null(self$user_id)) stop("Guest cannot delete from library")
      affected_rows <- db_delete_library_entry(self$pool, entity_id)
      if (affected_rows == 0) {
        warning(paste("No active chart found to delete with entity_id:", entity_id))
      }
      self$refresh_user_data()
      invisible(affected_rows)
    },

    # 4. Methods: Calculation ------------------------------------

    #' @description
    #' update horoscope. Calculate planetary positions, draw chart
    #'
    update_chart = function(){
      tryCatch({
        # 1. Perform Calculation
        self$planet_position <- calculate_planet_position(self$horoscope_datetime, self$horoscope_timezone, self$horoscope_longitude, self$horoscope_latitude)

        data <- self$planet_position$planetary_position
        data <- data[(row.names(data) %in% self$selected_planets),]
        self$aspect_table <- calculate_aspect(data)
        self$planet_position$planetary_position <- data
        self$chart <- draw_whole_sign_chart(data, self$chart_name, self$horoscope_datetime, self$horoscope_city, self$horoscope_country, self$horoscope_timezone, self$aspect_table)

      }, error = function(e) {
        # 2. LOG THE CRASH
        self$logger$log_error(
          event = "CALC_FAILURE",
          message = e$message,
          user_id = self$user_id,
          context = list(
            time = as.character(self$horoscope_datetime),
            city = self$horoscope_city,
            tz = self$horoscope_timezone
          )
        )
        # Re-throw so UI knows something broke
        stop(e)
      })
    },
  #' @description
  #' Add or minus datetime according for a certain value and unit, then plot the chart
  #' @param operation add or minus (interact with an action button)
  #' @param value range from 1 to 30
  #' @param unit Minutes, Hours, Days, Months and Years
  #'
  adjust_time = function(operation = c("add", "minus"), value, unit) {
    operation <- match.arg(operation)

    if (operation == "add") {
      self$horoscope_datetime <- add_datetime(self$horoscope_datetime, unit, value)
    } else {
      self$horoscope_datetime <- minus_datetime(self$horoscope_datetime, unit, value)
    }

    # Refresh the chart data with the new datetime
    self$update_chart()

    self$logger$log_info(
      event = "TIME_ADJUST",
      message = paste("Time adjusted:", operation, value, unit,
                      "| New time:", self$horoscope_datetime)
    )

    return(invisible(self))
  }),

  # 5. Private Methods -----------------------------------------
  private = list(
    finalize = function() {
      if (!is.null(self$pool)) close_postgres_db(self$pool)
    }
  )
)
