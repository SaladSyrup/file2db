#' f2dbAppendTable
#'
#' A `f2dbTask` for appending data to a database table.
#'
#' @name f2dbAppendTable-class
#' @docType class
#' @family f2dbTask
#' @family f2db classes
#' @export
methods::setClass("f2dbAppendTable", contains = c("f2dbTask"))

#-------------------------------------------------------------------------------
#' f2dbAppendTable
#'
#' `f2dbAppendTable` constructor.
#'
#' @returns An `f2dbAppendTable` object.
#'
#' @family f2dbTask
#' @export
f2dbAppendTable <- function() {}
