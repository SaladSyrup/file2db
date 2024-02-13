#' f2dbObject class
#'
#' Base class from which all other f2db classes descend. This is a
#' virtual class.
#'
#' @name f2dbObject-class
#' @docType class
#' @family file2db classes
#' @export
methods::setClass("f2dbObject", contains = c("VIRTUAL"))

#' f2dbRun
#'
#' Generic method for running an f2dbObject
#'
#' @returns Logical value indicating success (`TRUE`) or failure (`FALSE`). Success
#' only indicates that it is safe to proceed; it does not mean there are no
#' warnings or errors.
#'
#' @name f2dbRun-generic
#' @docType methods
#' @family f2dbRun methods
#' @export
methods::setGeneric("f2dbRun",
  function(.Object, ...) standardGeneric("f2dbRun"),
  signature = ".Object"
)
