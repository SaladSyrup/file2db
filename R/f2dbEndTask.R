#' f2dbEndTask class
#'
#' A do-nothing task. This is the default `nextTask` for `f2dbTask` instances.
#'
#' @slot name "ENDTASK"
#'
#' @name f2dbEndTask-class
#' @docType class
#' @family f2db classes
#' @export
methods::setClass("f2dbEndTask",
  contains = c("f2dbTask"),
  prototype = list(
    name = "ENDTASK"
  )
)

#-------------------------------------------------------------------------------
#' initialize
#'
#' Initializer function for `f2dbEndTask` objects. This initializer explicitly
#' avoids calling the parent `f2dbTask` initializer.
#'
#' @param .Object `f2dbEndTask` object.
#'
#' @returns Initialized `f2dbEndTask` object.
#'
#' @name initialize,f2dbEndTask-method
#' @docType methods
#' @noRd
methods::setMethod("initialize", "f2dbEndTask", function(.Object) .Object)

#' f2dbRun
#' @name f2dbRun,f2dbEndTask-method
#' @rdname f2dbRun-f2dbTask-method
#' @export
methods::setMethod(
  "f2dbRun", "f2dbEndTask",
  function(object, input = NA, item = NA) {
    list(success = TRUE, output = object@name)
  }
)
