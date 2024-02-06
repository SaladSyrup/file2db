#' Set database connection
#'
#' Sets the database connection to be used when loading data.
#'
#' The user is responsible for both opening and closing the database connection.
#'
#' @param conn A DBIConnection object returned by a call to dbConnect.
#'
#' @returns `conn`, invisibly.
#'
#' @examples
#' conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' set_dbconn(conn)
#'
#' # Do things...
#'
#' DBI::dbDisconnect(get_dbconn())
#'
#' @export
set_dbconn <- function(conn) {

  stopifnot(methods::is(conn, "DBIConnection"))

  the$db_conn <- conn
  invisible(conn)
}

#' Get database connection
#'
#' Gets the database connection previously set by `set_dbconn`.
#'
#' @returns A DBIConnection object or NULL if no database connection has been set.
#'
#' @inherit set_dbconn examples
#'
#' @export
get_dbconn <- function() {
  return(the$db_conn)
}
