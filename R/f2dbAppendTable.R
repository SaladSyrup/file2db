#' f2dbAppendTable
#'
#' Creates an `f2dbTask` for writing data to a database using
#' `DBI::dbAppendTable`.
#'
#' @param dbConn A `DBI::DBIConnection` object, as returned by `DBI::dbConnect()`.
#' @param dbTable A table name.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Parameters passed to other
#' methods.
#'
#' @returns An `f2dbTask` object.
#'
#' @family f2dbTask
#' @export
f2dbAppendTable <- function(dbConn, dbTable, ...) {
  stopifnot(DBI::dbIsValid(dbConn))
  stopifnot(!DBI::dbIsReadOnly(dbConn))
  stopifnot(DBI::dbExistsTable(dbConn, dbTable))

  appendTable <- f2dbAppendTableFunction(dbConn, dbTable)

  f2dbTask("f2dbAppendTable", appendTable, ..., inputName = "value")
}

#-------------------------------------------------------------------------------
#' f2dbAppendTableFunction
#'
#' Function factory. This is an internal function.
#'
#' @noRd
f2dbAppendTableFunction <- function(dbConn, dbTable) {
  function(value, ..., row.names = NULL) {
    DBI::dbAppendTable(dbConn, dbTable, value, row.names)
  }
}
