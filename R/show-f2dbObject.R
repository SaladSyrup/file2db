#' showObject
#'
#' Pretty-prints for show()
#'
#' @param object An `f2dbObject`.
#'
#' @returns A character vector
#'
#' @noRd
showObject <- function(object) paste0(class(object)[1], ": ", name(object))

#-------------------------------------------------------------------------------
#' show
#'
#' @param object An `f2dbObject` to show.
#'
#' @name show,f2dbObject-method
#' @docType methods
#' @noRd
methods::setMethod("show", "f2dbObject", function(object) cat(showObject(object)))
