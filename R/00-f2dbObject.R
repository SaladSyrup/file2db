#' f2dbObject class
#'
#' Base class from which all other f2db classes descend.
#'
#' @slot name Object name.
#'
#' @name f2dbObject-class
#' @docType class
#' @family f2dbObject
#' @family f2db classes
#' @export
methods::setClass("f2dbObject",
  slots = c(name = "character"),
  prototype = list(name = "f2dbObject")
)

#-------------------------------------------------------------------------------
#' f2dbObject
#'
#' `f2dbObject` constructor.
#'
#' @param name Object name.
#'
#' @returns An `f2dbObject`.
#'
#' @family f2dbObject
#' @export
f2dbObject <- function(name) {
  if (!methods::hasArg(name)) name <- "<f2dbObject>"
  methods::new("f2dbObject", name = name)
}

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
#' @family f2dbObject
#' @family f2dbRun methods
#' @export
methods::setGeneric("f2dbRun",
  function(object, ...) standardGeneric("f2dbRun"),
  signature = "object"
)

#-------------------------------------------------------------------------------
#' f2dbRun
#' @name f2dbRun,f2dbObject-method
#' @rdname f2dbRun-method
#' @export
methods::setMethod(
  "f2dbRun", "f2dbObject",
  function(object, ...) {
    list(success = TRUE, output = name(object))
  }
)

#-------------------------------------------------------------------------------
#' name
#'
#' Returns the name of an `f2dbObject`.
#'
#' @param object An `f2dbObject`.
#'
#' @returns A name.
#'
#' @name name-method
#' @aliases name
#' @docType methods
#' @family f2dbObject
#' @export
methods::setGeneric("name",
  function(object) standardGeneric("name"),
  signature = "object"
)

#-------------------------------------------------------------------------------
#' @name name,f2dbObject-method
#' @rdname name-method
#' @export
methods::setMethod("name", "f2dbObject", function(object) object@name)
