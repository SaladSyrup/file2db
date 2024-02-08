#' file2dbNullTask class
#'
#' A do-nothing task. This is the default nextTask for file2dbTask instances.
#'
#' @slot name "NULLTASK"
#'
#' @export
methods::setClass("file2dbNullTask", contains = c("file2dbTask"),

  prototype = list(
    name = "NULLTASK"
  )
)

#' Initialize function for file2dbNullTask objects
#'
#' This initializer explicitly avoids calling the parent file2dbTask initializer.
methods::setMethod("initialize", "file2dbNullTask", function(.Object) .Object)

#setMethod("runTask", "file2dbNullTask", function() return(TRUE))
