#' f2dbTaskFunction class
#'
#' @description
#' The f2dbTaskFunction class encapsulates the function that accomplishes the
#' actual work of the task. This class is responsible for:
#'
#' 1. Calling the underlying task function
#' 1. Capturing and logging any warnings or errors
#' 1. Indicating whether or not the task executed successfully
#' 1. Returning task function output
#'
#' @details
#' **must** implement a method for the generic `run()`
#'
#' @slot taskFunction The task function
#' @slot auxParameters Additional parameters to be passed to the task function
#'
#' @docType class
#' @name f2dbTaskFunction-class
#' @export
methods::setClass("f2dbTaskFunction", contains = "f2dbObject",

  slots = c(
    taskFunction = "function",
    auxParameters = "character"

  ),

  prototype = list(
    taskFunction = NULL,
    auxParameters = ""
  )
)

methods::setGeneric("f2dbRun", function(taskFunction, ...) standardGeneric("f2dbRun"),
  signature = "taskFunction")
