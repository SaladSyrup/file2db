#' file2dbTask class
#'
#' A virtual class for encapsulating an individual task.
#'
#' @slot name Task name. This name should be unique within the job.
#' @slot taskFunction Function that performs the actual work of the task.
#' @slot nextTask A file2dbTaskObject object that performs the next task in the
#' job sequence. nextTask takes the output of the current task as its input.
#'
#' @export
methods::setClass("file2dbTask", contains = c("file2dbTaskObject", "VIRTUAL"),

  slots = c(
    name = "character",
    taskFunction = "function",
    nextTask = "file2dbTaskObject"
  ),

  prototype = list(
    name = "<UNNAMED>",
    taskFunction = NULL,
    nextTask = NULL
  )
)

#' Default initialize function for file2dbTask objects
methods::setMethod("initialize", "file2dbTask",

 function(.Object) {
   .Object <- methods::callNextMethod()
   .Object@nextTask <- methods::new("file2dbNullTask")
   .Object
 }
)
