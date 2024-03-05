#' f2dbJob class
#'
#' A container for job tasks.
#'
#' @slot name Job name. This should be unique within the job batch.
#' @slot jobInput Input supplied to the first task.
#' @slot tasks List of job tasks.
#'
#' @name f2dbJob-class
#' @docType class
#' @family f2dbJob
#' @family f2db classes
#' @export
methods::setClass("f2dbJob",
  contains = c("f2dbObject"),
  slots = c(
    jobInput = "ANY",
    taskList = "list"
  ),
  prototype = list(
    jobInput = NULL,
    taskList = list()
  )
)

#-------------------------------------------------------------------------------
#' f2dbJob
#'
#' `f2dbJob` constructor.
#'
#' @param name Job name. This should be unique within the job batch.
#' @param input Input supplied to the first task.
#'
#' @returns An `f2dbJob` object.
#'
#' @family f2dbJob
#' @export
f2dbJob <- function(name, input) {
  if (!methods::hasArg(name)) {
    name <- "Unnamed"
  } else {
    name <- as.character(name)
  }

  if (!methods::hasArg(input)) input <- ""

  methods::new("f2dbJob", name = name, jobInput = input)
}

#-------------------------------------------------------------------------------
#' listTasks
#'
#' Shows job tasks.
#'
#' @param object An `f2dbJob` object.
#'
#' @returns A list of named character vectors describing each task.
#'
#' @name listTasks-method
#' @aliases listTasks
#' @docType methods
#' @family f2dbJob
#' @export
methods::setGeneric("listTasks",
  function(object) standardGeneric("listTasks"),
  signature = "object"
)

#-------------------------------------------------------------------------------
# Accessors
#-------------------------------------------------------------------------------
#' jobInput
#'
#' Returns job input to the first task
#'
#' @param object An `f2dbJob` object.
#'
#' @returns `jobInput
#'
#' @name jobInput-method
#' @aliases jobInput
#' @docType methods
#' @family f2dbJob`
#' @export
methods::setGeneric("jobInput",
  function(object) standardGeneric("jobInput"),
  signature = "object"
)

#-------------------------------------------------------------------------------
#' @name jobInput,f2dbJob-method
#' @rdname jobInput-method
#' @export
methods::setMethod("jobInput", "f2dbJob", function(object) object@jobInput)

#-------------------------------------------------------------------------------
#' jobInput<-
#'
#' Sets job input to the first task
#'
#' @param object An `f2dbJob`
#' @param value `jobInput`
#'
#' @name jobInput-set-method
#' @aliases jobInput<-
#' @docType methods
#' @family f2dbJob
#' @export
methods::setGeneric("jobInput<-",
  function(object, value) standardGeneric("jobInput<-"),
  signature = c("object", "value")
)

#-------------------------------------------------------------------------------
#' @name jobInput<-,f2dbJob-method
#' @rdname jobInput-set-method
#' @export
methods::setMethod(
  "jobInput<-",
  signature(object = "f2dbJob", value = "ANY"),
  function(object, value) {
    object@jobInput <- value
    object
  }
)

#-------------------------------------------------------------------------------
# Internal methods
#-------------------------------------------------------------------------------
#' linkTaskList
#'
#' This is an internal function.
#'
#' @param job A job containing tasks to be linked. Tasks are linked in the same
#' order as the taskList.
#'
#' @returns A job containing linked tasks.
#'
#' @name linkTaskList-method
#' @docType methods
#' @noRd
linkTaskList <- function(job) {
  numTasks <- length(job@taskList)

  if (numTasks > 1) {
    for (i in (numTasks - 1):1) {
      nextTask(job@taskList[[i]]) <- job@taskList[[i + 1]]
    }
  }

  return(job)
}
