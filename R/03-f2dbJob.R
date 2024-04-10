#' f2dbJob class
#'
#' A container for job tasks.
#'
#' @slot name Job name. This should be unique within the job batch.
#' @slot jobInput Input supplied to the first task.
#' @slot taskList List of job tasks.
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
#' @param jobName Job name. This should be unique within the job batch.
#' @param input Input supplied to the first task.
#'
#' @returns An `f2dbJob` object.
#'
#' @family f2dbJob
#' @export
f2dbJob <- function(jobName, input) {
  if (!methods::hasArg(jobName)) {
    jobName <- "Unnamed"
  } else {
    jobName <- as.character(jobName)
  }

  if (!methods::hasArg(input)) input <- NA

  methods::new("f2dbJob", name = jobName, jobInput = input)
}

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
#' taskList
#'
#' Returns taskList.
#'
#' @param object An `f2dbJob` object.
#'
#' @returns A list of `f2dbTask`s.
#'
#' @name taskList-method
#' @aliases taskList
#' @docType methods
#' @family f2dbJob
#' @export
methods::setGeneric("taskList",
  function(object) standardGeneric("taskList"),
  signature = "object"
)

#-------------------------------------------------------------------------------
#' @rdname taskList-method
#' @export
methods::setMethod("taskList", "f2dbJob", function(object) object@taskList)

#-------------------------------------------------------------------------------
#' taskList<-
#'
#' Add `f2dbTask` objects to the end of the task list and update the `nextTask`
#' slot of the next-to-last task to point to the newly added task.
#'
#' @param object An `f2dbJob`.
#' @param value An `f2dbTask`.
#'
#' @name taskList-set-method
#' @aliases taskList<-
#' @docType methods
#' @family f2dbJob
#' @export
methods::setGeneric("taskList<-",
  function(object, value) standardGeneric("taskList<-"),
  signature = c("object", "value")
)

#-------------------------------------------------------------------------------
#' @rdname taskList-set-method
#' @export
methods::setMethod(
  "taskList<-",
  signature(object = "f2dbJob", value = "f2dbTask"),
  function(object, value) {
    stopifnot(methods::validObject(object))
    stopifnot(methods::validObject(value))

    taskNames <- names(object@taskList)
    numTasks <- length(object@taskList)

    if (numTasks != 0) {
      if (isa(object@taskList[[numTasks]], "f2dbEndTask")) {
        stop("Cannot add new tasks after an end task")
      }
      object@taskList <- append(object@taskList, value)
    } else {
      object@taskList <- list(value)
    }

    taskNames <- c(taskNames, name(value))
    taskNames <- make.names(taskNames, unique = TRUE)
    names(object@taskList) <- taskNames

    linkTaskList(object)
  }
)

#-------------------------------------------------------------------------------
#' @rdname taskList-set-method
#' @export
methods::setMethod(
  "taskList<-",
  signature(object = "f2dbJob", value = "list"),
  function(object, value) {
    stopifnot(methods::validObject(object))

    for (task in value) {
      taskList(object) <- task
    }

    return(object)
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
