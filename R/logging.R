#' setf2dbLogger
#'
#' Sets the file2db logger.
#'
#' @details
#' `file2db` uses the package `log4r` to maintain two logs:
#'
#'  * `f2db.log` documents the process of running an `f2dbObject`; e.g., name,
#'  number of tasks, task function, etc. The default `f2db.log` is
#'  `log4r::logger("INFO", appenders = log4r::file_appender("f2db.log"))`.
#'  * `task.log` logs the results of executing a task function. The default
#'  `task.log` is
#'  `log4r::logger("INFO", appenders = log4r::file_appender("task.log"))`
#'
#'  Use `setf2dbLogger` and `setTaskLogger` to modify the default loggers.
#'
#'
#' @param logger An object of `log4r::logger` class.
#'
#' @family logging
#' @export
setf2dbLogger <- function(logger) f2dbLogger <<- logger

#' setTaskLogger
#'
#' Sets the task logger.
#'
#' @inherit setf2dbLogger details
#' @family logging
#' @export
setTaskLogger <- function(logger) taskLogger <<- logger


#-------------------------------------------------------------------------------
# Internal methods
#-------------------------------------------------------------------------------
#' logDebug
#'
#' This is an internal function.
#'
#' @param logger An object of class 'logger'.
#' @param ... One or more items to log.
#'
#' @noRd
logDebug <- function(logger, ...) log4r::debug(logger, ...)

#' logInfo
#'
#' @inherit logDebug
#' @noRd
logInfo <- function(logger, ...) log4r::info(logger, ...)

#' logWarn
#'
#' @inherit logDebug
#' @noRd
logWarn <- function(logger, ...) log4r::warn(logger, ...)

#' logError
#'
#' @inherit logDebug
#' @noRd
logError <- function(logger, ...) log4r::error(logger, ...)

#' logFatal
#'
#' @inherit logDebug
#' @noRd
logFatal <- function(logger, ...) log4r::fatal(logger, ...)
