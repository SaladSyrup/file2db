#' f2dbShow
#'
#' Pretty prints for show. This is an internal function.
#'
#' @param object
#'
#' @returns A named character vector
#'
#' @name f2dbShow-method
#' @docType methods
#' @noRd
methods::setGeneric("f2dbShow", function(object) standardGeneric("f2dbShow"), signature = "object")

#-------------------------------------------------------------------------------
#' @name f2dbShow,f2dbObject-method
#' @docType methods
#' @noRd
methods::setMethod("f2dbShow", "f2dbObject", function(object) c(name = paste0("<", class(object)[1], ">: ", name(object))))

#-------------------------------------------------------------------------------
#' show
#'
#' @param object An `f2dbObject` to show.
#'
#' @name show,f2dbObject-method
#' @docType methods
#' @noRd
methods::setMethod("show", "f2dbObject", function(object) cat(f2dbShow(object), sep = "\n  "))
