#' f2dbRun
#'
#' Run an `f2dbJob`.
#'
#' When called on an `f2dbJob` object, `f2dbRun` will run the tasks in the
#' `taskList`. The first task is called using `jobInput` as both task `input`
#' and job `item`.
#'
#' @param object An `f2dbJob` to run.
#'
#' @returns
#' Logical value indicating success (`TRUE`) or failure (`FALSE`). There may be
#' warning or informational messages even if success is indicated.
#'
#' @name f2dbRun,f2dbJob-method
#' @docType methods
#' @family f2dbTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbJob",
  function(object) {
    info("Running ", f2dbShow(object)[["name"]])
    info("  jobInput: ", object@jobInput)

    numTasks <- length(object@taskList)
    if (numTasks == 0) {
      info(name(object), ": No tasks to run")
      return(TRUE)
    }

    for (n in 1:numTasks) {
      info("Task ", n, "/", numTasks)
      info("  ", name(object@taskList[[n]]))
      info("  ", rlang::quo_name(taskFunction(object@taskList[[n]])@taskCall))
    }

    debug(name(object), ": Calling first task")
    success <- f2dbRun(object@taskList[[1]], object@jobInput, object@jobInput)

    if (success == TRUE) {
      info(name(object), ": Job completed successfully")
    } else {
      error(name(object), ": Job unsuccessful")
    }

    success
  }
)
