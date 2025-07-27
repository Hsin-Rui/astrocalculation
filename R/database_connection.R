#' Get a connection to the cities SQLite database
#'
#' This function locates the 'cities.sqlite' database included with the package
#' and returns a DBI connection to it.
#'
#' @return A DBI database connection object.
#' @export
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect
#' @examples
#' \dontrun{
#' con <- get_cities_connection()
#' DBI::dbListTables(con)
#' DBI::dbDisconnect(con)
#' }
connect_cities_db <- function() {
  # Locate the database file within the installed package
  path <- system.file("extdata", "cities.sqlite", package = "astrocalculation", mustWork = TRUE)

  # Establish and return the connection
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

  return(con)
}
