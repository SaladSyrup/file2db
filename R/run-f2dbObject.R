#' f2dbRun
#'
#' This is the default `f2dbRun` method for `f2dbObject`s.
#'
#' @param object An `f2dbObject`.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#'
#' @returns
#' A list:
#' \item{success}{`TRUE`.}
#' \item{object}{`f2dbObject`.}
#' \item{name}{The name of `object`.}
#'
#' @name f2dbRun,f2dbObject-method
#' @docType methods
#' @family f2dbObject
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbObject",
  function(object, ...) {
    debug("Running ", f2dbShow(object)[["name"]])
    debug(name(object), ": f2dbObject returning TRUE")
    return(TRUE)
  }
)
