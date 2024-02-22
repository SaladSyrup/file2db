#' appendTask
#'
#' Adds an existing `f2dbTask` object to the end of the task list and updates
#' the `nextTask` slot of the next-to-last task to point to the newly added
#' task. Saves the modified object to given jobSymbol and environment.
#'
#' This is an internal function.
#'
#' @param job The `f2dbJob` to add a job to.
#' @param task The `f2dbTask` to add.
#' @param jobSymbol Symbol of the `f2dbJob` to modify.
#' @param env Environment where `jobSymbol` is located.
#'
#' @returns `NULL`, invisibly.
#'
#' @name appendTask-method
#' @aliases appendTask
#' @docType methods
#' @noRd
methods::setGeneric("appendTask", function(job, task, jobSymbol, env) standardGeneric("appendTask"),
  signature = c("job", "task")
)

#-------------------------------------------------------------------------------
#' @name appendTask,f2dbJob,f2dbTask-method
#' @noRd
methods::setMethod(
  "appendTask", signature(job = "f2dbJob", task = "f2dbTask"),
  function(job, task, jobSymbol, env) {
    taskNames <- names(job@taskList)
    numTasks <- length(job@taskList)

    if (numTasks != 0) {
      if (isa(job@taskList[[numTasks]], "f2dbEndTask")) {
        stop("Cannot add new tasks after an end task")
      }

      nextTask(job@taskList[[numTasks]]) <- task
      job@taskList <- c(job@taskList, task)
    } else {
      job@taskList <- list(task)
    }

    taskNames <- c(taskNames, name(task))
    taskNames <- make.names(taskNames, unique = TRUE)
    names(job@taskList) <- taskNames

    rlang::env_poke(env, rlang::as_string(jobSymbol), job)

    invisible()
  }
)
