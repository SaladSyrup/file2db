#' appendTask
#'
#' Adds an existing `f2dbTask` object to the end of the task list and updates
#' the `nextTask` slot of the next-to-last task to point to the newly added task.
#'
#' @param object An `f2dbJob`.
#' @param task The `f2dbTask` to add.
#'
#' @returns `NULL`, invisibly.
#'
#' @name appendTask-method
#' @aliases appendTask
#' @docType methods
#' @family f2dbJob
#' @export
methods::setGeneric("appendTask", function(object, task, ...) standardGeneric("appendTask"),
  signature = c("object", "task")
)

#-------------------------------------------------------------------------------
#' @name appendTask,f2dbJob,f2dbTask-method
#' @rdname appendTask-method
#' @export
methods::setMethod(
  "appendTask", signature(object = "f2dbJob", task = "f2dbTask"),
  function(object, task) {
    stopifnot(methods::validObject(object))
    stopifnot(methods::validObject(task))

    numTasks <- length(object@taskList)

    if (numTasks != 0) {
      if (isa(object@taskList[[numTasks]], "f2dbEndTask")) {
        stop("Cannot add new tasks after an end task")
      }

      nextTask(object@taskList[[numTasks]]) <- task
      object@taskList <- c(object@taskList, task)
    } else {
      object@taskList <- list(task)
    }

    object
  }
)
