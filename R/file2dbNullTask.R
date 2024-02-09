#' f2dbNullTask class
#'
#' A do-nothing task. This is the default nextTask for f2dbTask instances.
#'
#' @slot name "NULLTASK"
#'
#' @export
methods::setClass("f2dbNullTask", contains = c("f2dbTask"),

  prototype = list(
    name = "NULLTASK"
  )
)

#' Initialize function for f2dbNullTask objects
#'
#' This initializer explicitly avoids calling the parent f2dbTask initializer.
methods::setMethod("initialize", "f2dbNullTask", function(.Object) .Object)

#setMethod("runTask", "f2dbNullTask", function() return(TRUE))
