#' setf2dbLogger
#'
#' Sets the file2db logger.
#'
#' `file2db` uses the package `log4r`. The default log is
#' `log4r::logger("INFO", appenders = log4r::file_appender("f2db.log"))`.
#'
#' @param logger An object of `log4r::logger` class.
#'
#' @family logging
#' @export
setf2dbLogger <- function(logger) {
  assign("f2dbLogger", logger, pos = asNamespace("file2db"))

#  ns$f2dbLogger <- logger
}

#-------------------------------------------------------------------------------
# Internal methods
#-------------------------------------------------------------------------------
#' logDebug
#'
#' This is an internal function.
#'
#' @param ... One or more items to log.
#'
#' @noRd
debug <- function(...) log4r::debug(f2dbLogger, ...)

#' logInfo
#'
#' @inherit logDebug
#' @noRd
info <- function(...) log4r::info(f2dbLogger, ...)

#' logWarn
#'
#' @inherit logDebug
#' @noRd
warn <- function(...) log4r::warn(f2dbLogger, ...)

#' logError
#'
#' @inherit logDebug
#' @noRd
error <- function(...) log4r::error(f2dbLogger, ...)

#' logFatal
#'
#' @inherit logDebug
#' @noRd
fatal <- function(...) log4r::fatal(f2dbLogger, ...)
