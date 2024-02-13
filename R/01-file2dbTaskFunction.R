#' f2dbTaskFunction class
#'
#' @description
#' The f2dbTaskFunction class encapsulates the function that accomplishes the
#' actual work of the task. Along with the `f2dbRun` method, this class is
#' responsible for:
#'
#' 1. Calling the underlying task function
#' 1. Capturing and logging any warnings or errors
#' 1. Capturing task function output for use by the calling `f2dbTask`
#' 1. Returning whether or not the task executed successfully
#'
#' @details
#' Classes inheriting from `f2dbTaskFunction` should fulfill the responsibilities
#' listed above and implement their own `f2dbRun()`.
#'
#' @slot taskFunction The task function
#' @slot auxParameters Additional parameters to be passed to the task function
#'
#' @name f2dbTaskFunction-class
#' @docType class
#' @family file2db classes
#' @export
methods::setClass("f2dbTaskFunction",
  contains = "f2dbObject",
  slots = c(
    taskFunction = "function",
    auxParameters = "character"
  ),
  prototype = list(
    taskFunction = NULL,
    auxParameters = ""
  )
)
