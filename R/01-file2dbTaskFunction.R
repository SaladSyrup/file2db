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
#' listed above and implement the `f2dbRun` generic method.
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

#' f2dbTaskFunction
#'
#' Creates a new f2dbTaskFunction object.
#'
#' @param taskFunction Name of the function to call when running the task
#' @param ... Parameters to pass to the task function
#' @param inputName By default, task input is passed to the task function as an
#' unnamed first parameter. If `inputName` is provided, task input will be passed
#' using `inputName`.
#' @param checklistItemName By default, the current checklist item is not passed
#' to the task function. If `checklistItemName` is provided, the checklist item
#' will be passed using `checklistItemName`.
#'
#' @returns An `f2dbTaskFunction` object
#'
#' @family file2db classes
#' @export
f2dbTaskFunction <- function(taskFunction, ..., inputName = NA, checklistItem = NA) {
  methods::new("f2dbTaskFunction")
}

#' f2dbRun
#'
#' Runs the given task function.
#'
#' @param .Object An `f2dbTaskFunction` object
#' @inheritParams f2dbRun-generic
#'
#' @inherit f2dbRun-generic return
#'
#' @name f2dbRun.f2dbTaskFunction
#' @docType methods
#' @family f2dbRun methods
#' @export
methods::setMethod("f2dbRun", "f2dbTaskFunction",

  function(.Object, input = NA, checklistItem = NA) {

  }
)
