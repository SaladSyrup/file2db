#' f2dbObject class
#'
#' Base class from which all other f2db classes descend. This is a virtual class.
#'
#' @name f2dbObject-class
#' @docType class
#' @family f2db classes
#' @export
methods::setClass("f2dbObject", contains = c("VIRTUAL"))

#-------------------------------------------------------------------------------
#' f2dbRun
#'
#' Generic method for running an `f2dbObject`.
#'
#' @param object An `f2dbObject` to run
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Parameters passed to
#'   class-specific `f2dbRun` implementations.
#'
#' @returns
#' A list:
#' \item{success}{Logical value indicating success (`TRUE`) or failure (`FALSE`).
#'   Success only indicates that it is safe to proceed; it does not mean there
#'   are no warnings or errors.}
#' \item{output}{Run results, if any.}
#'
#' @name f2dbRun-method
#' @aliases f2dbRun
#' @docType methods
#' @family f2dbRun methods
#' @export
methods::setGeneric("f2dbRun",
  function(object, ...) standardGeneric("f2dbRun"),
  signature = "object"
)
