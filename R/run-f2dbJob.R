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
#' A list:
#' \item{success}{Logical value indicating success (`TRUE`) or failure
#' (`FALSE`). There may be warning or informational messages even if success is
#' indicated.}
#' \item{object}{The type of object being run.}
#' \item{name}{The name of the object being run.}
#' \item{messages}{Captured error, warning, or informational messages.}
#'
#' @name f2dbRun,f2dbJob-method
#' @docType methods
#' @family f2dbTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbJob",
  function(object) {
    if (length(object@taskList) == 0) {
      return(list(
        success = TRUE, object = class(object)[1],
        name = name(object), messages = list("No tasks to run.")
      ))
    }

    taskListOutput <- f2dbRun(object@taskList[[1]], object@jobInput, object@jobInput)

    result <- list(
      success = taskListOutput[[1]]$success, object = class(object)[1],
      name = name(object), messages = taskListOutput[[1]]$messages
    )

    return(result)
  }
)
