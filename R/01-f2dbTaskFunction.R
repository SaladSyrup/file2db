#' f2dbTaskFunction class
#'
#' @description
#' The `f2dbTaskFunction` class encapsulates the function that accomplishes the
#' actual work of the task. Along with [f2dbRun()] method, this class is
#' responsible for:
#'
#' 1. Calling the underlying task function.
#' 1. Capturing any warnings or errors.
#' 1. Returning task function output.
#' 1. Returning whether or not the task executed successfully.
#'
#' @details
#' Classes inheriting from `f2dbTaskFunction` should fulfill the
#' responsibilities listed above and implement the [f2dbRun()] generic method.
#'
#' @slot name Set to the name of the underlying task function by the constructor.
#' @slot taskCall A call to the underlying task function.
#'
#' @name f2dbTaskFunction-class
#' @docType class
#' @family f2dbTaskFunction
#' @family f2db classes
#' @export
methods::setClass("f2dbTaskFunction",
  contains = "f2dbObject",
  slots = c(
    taskCall = "call"
  ),
  prototype = list(
    taskCall = NULL
  )
)

#-------------------------------------------------------------------------------
#' f2dbTaskFunction
#'
#' `f2dbTaskFunction` constructor.
#'
#' @param taskFunction Function to call when running the task.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Parameters passed to the task
#'   function.
#' @param inputName By default, task input is passed to the task function as an
#'   unnamed first parameter. If `inputName` is provided, task input will be
#'   passed using `inputName`.
#' @param itemName By default, the current job item is not passed to the task
#'   function. If `itemName` is provided, the job item will be passed using
#'   `itemName`.
#'
#' @returns An `f2dbTaskFunction` object.
#'
#' @family f2dbTaskFunction
#' @export
f2dbTaskFunction <- function(taskFunction,
                             ...,
                             inputName,
                             itemName) {
  if (methods::hasArg("taskFunction")) {
    taskFunction <- rlang::enexpr(taskFunction)
  } else {
    taskFunction <- rlang::expr(function(x) x)
  }
  stopifnot(rlang::is_callable(taskFunction))

  params <- list(as.symbol("taskInput"))

  if (methods::hasArg(inputName)) {
    stopifnot(identical(inputName, make.names(inputName)))
    names(params)[1] <- inputName
  }

  if (methods::hasArg(itemName)) {
    stopifnot(identical(itemName, make.names(itemName)))
    params[[itemName]] <- as.symbol("jobItem")
  }

  taskName <- rlang::as_label(taskFunction)

  methods::new("f2dbTaskFunction", name = taskName, taskCall = rlang::call2(taskFunction, !!!params, ...))
}

#-------------------------------------------------------------------------------
# Accessors
#-------------------------------------------------------------------------------
#' taskCall
#'
#' Returns the task call of an `f2dbTaskFunction`.
#'
#' @param object An `f2dbTaskFunction` object.
#'
#' @returns The `taskCall` evaluated when the given object is run.
#'
#' @name taskCall-method
#' @aliases taskCall
#' @docType methods
#' @family f2dbTaskFunction
#' @export
methods::setGeneric("taskCall",
  function(object) standardGeneric("taskCall"),
  signature = "object"
)

#-------------------------------------------------------------------------------
#' @name taskCall,f2dbTaskFunction-method
#' @rdname taskCall-method
#' @export
methods::setMethod("taskCall", "f2dbTaskFunction", function(object) object@taskCall)
