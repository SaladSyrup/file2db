#' showTaskFunction
#'
#' Pretty-prints for show()
#'
#' @param object An `f2dbTaskFunction`.
#'
#' @returns A character vector
#'
#' @noRd
showTaskFunction <- function(object) paste0(class(object)[1], ": ", name(object))

#-------------------------------------------------------------------------------
#' show
#'
#' @param object An `f2dbTaskFunction` to show.
#'
#' @name show,f2dbTaskFunction-method
#' @docType methods
#' @noRd
methods::setMethod("show", "f2dbTaskFunction", function(object) cat(showTaskFunction(object)))
