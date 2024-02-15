#' f2dbEndTask class
#'
#' A do-nothing task. This is the default nextTask for f2dbTask instances.
#'
#' @slot name "ENDTASK"
#'
#' @name f2dbEndTask-class
#' @docType class
#' @family file2db classes
#' @export
methods::setClass("f2dbEndTask",
  contains = c("f2dbTask"),
  prototype = list(
    name = "ENDTASK"
  )
)

#' Initialize function for f2dbEndTask objects
#'
#' This initializer explicitly avoids calling the parent f2dbTask initializer.
methods::setMethod("initialize", "f2dbEndTask", function(.Object) .Object)
