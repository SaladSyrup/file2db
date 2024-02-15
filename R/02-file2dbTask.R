#' f2dbTask class
#'
#' A class for encapsulating an individual task.
#'
#' @slot name Task name. This name normally be should be unique within the job.
#' @slot taskFunction An `f2dbTaskFunction` to perform the actual work of the task.
#' @slot nextTask An `f2dbTaskObject` object that performs the next task in the
#' job sequence. `nextTask` takes the output of the current task as its input.
#' This value is set when adding tasks to a job and should not be modified.
#'
#' @name f2dbTask.class
#' @docType class
#' @family f2dbTask
#' @family file2db classes
#' @export
methods::setClass("f2dbTask",
  contains = c("f2dbObject"),
  slots = c(
    name = "character",
    taskFunction = "f2dbTaskFunction",
    nextTask = "f2dbObject"
  ),
  prototype = list(
    name = "<UNNAMED>",
    taskFunction = NULL,
    nextTask = NULL
  )
)

#' Default initialize function for f2dbTask objects
methods::setMethod(
  "initialize", "f2dbTask",
  function(.Object) {
    .Object <- methods::callNextMethod()
    .Object@nextTask <- methods::new("f2dbEndTask")
    .Object
  }
)

#' f2dbTask
#'
#' Creates a new `f2dbTask` object.
#'
#' @param name Task name. This name normally be should be unique within the job.
#' @inheritParams f2dbTaskFunction
#'
#' @family f2dbTask
#' @family file2db classes
#' @export
f2dbTask <- function(name = NA, taskFunction, ..., inputName = NA, itemName = NA) {

}
