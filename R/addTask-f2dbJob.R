#' addTask, addNewTask
#'
#' Add a `f2dbTask` object to the end of the task list and update the `nextTask`
#' slot of the next-to-last task to point to the newly added task.
#'
#' @param job An `f2dbJob` to add a task to.
#' @param task An existing `f2dbTask` to add.
#' @param followNext Recursively adds `nextTask`
#' @inheritParams f2dbTask
#'
#' @returns An `f2dbJob` with the added task.
#'
#' @name addTask-method
#' @aliases addTask
#' @docType methods
#' @family f2dbJob
#' @export
methods::setGeneric("addTask", function(job, task, followNext = FALSE, ...) standardGeneric("addTask"),
  signature = c("job", "task")
)

#-------------------------------------------------------------------------------
#' @rdname addTask-method
#' @export
methods::setMethod(
  "addTask", signature(job = "f2dbJob", task = "f2dbTask"),
  function(job, task, followNext = FALSE) {
    stopifnot(methods::validObject(job))
    stopifnot(methods::validObject(task))
    stopifnot(is.logical(followNext))

    taskNames <- names(job@taskList)
    numTasks <- length(job@taskList)

    if (numTasks != 0) {
      if (isa(job@taskList[[numTasks]], "f2dbEndTask")) {
        stop("Cannot add new tasks after an end task")
      }

      job@taskList <- append(job@taskList, task)
      nextTask(job@taskList[[numTasks]]) <- job@taskList[[numTasks + 1]]
    } else {
      job@taskList <- list(task)
    }

    taskNames <- c(taskNames, name(task))
    taskNames <- make.names(taskNames, unique = TRUE)
    names(job@taskList) <- taskNames

    if ((followNext == TRUE) && (isa(nextTask(task), "f2dbTask"))) {
      job <- addTask(job, nextTask(task), followNext)
    }

    return(job)
  }
)

#-------------------------------------------------------------------------------
#' @rdname addTask-method
#' @export
methods::setMethod(
  "addTask", signature(job = "f2dbJob", task = "list"),
  function(job, task, followNext = FALSE) {
    stopifnot(methods::validObject(job))

    for (t in task) {
      job <- addTask(job, t, followNext)
    }

    return(job)
  }
)
