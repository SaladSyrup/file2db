#' f2dbTask class
#'
#' A class for encapsulating an individual task.
#'
#' @slot name Task name. This name normally be should be unique within the job.
#' @slot taskFunction Function that performs the actual work of the task.
#' @slot nextTask A f2dbTaskObject object that performs the next task in the
#' job sequence. nextTask takes the output of the current task as its input.
#'
#' @name f2dbTask-class
#' @docType class
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
