#' f2dbRun
#'
#' Run an `f2dbTask`.
#'
#' When called on an `f2dbTask` object, `f2dbRun` will run `taskFunction`. If
#' `taskFunction` is successful, then `f2dbRun` is called with `nextTask`.
#'
#' @param object An `f2dbTask` to run.
#' @param input Input to pass to the underlying task function.
#' @param item The job item being processed.
#'
#' @returns
#' A list of lists. The first list contains:
#' \item{success}{Logical value indicating success (`TRUE`) or failure
#' (`FALSE`). `TRUE` is returned if both `taskFunction` and `nextTask` are
#' successful. Otherwise, `FALSE` is returned.}
#' \item{object}{The type of object being run.}
#' \item{name}{The name of the object being run.}
#' \item{item}{The job item.}
#' \item{messages}{Captured error, warning, or informational messages.}
#' \item{nextResult}{Results of running `nextTask`.}
#'
#' @name f2dbRun,f2dbTask-method
#' @docType methods
#' @family f2dbTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbTask",
  function(object, input = NA, item = NA) {
    functionOutput <- f2dbRun(taskFunction(object), input, item)
    result <- list(
      success = functionOutput$success, object = class(object)[1],
      name = name(object), item = item, messages = functionOutput$messages,
      nextResult = list()
    )

    if (functionOutput$success == FALSE) {
      return(result)
    }

    nextResult <- f2dbRun(nextTask(object), functionOutput$output, item)

    result$success <- result$success && nextResult$success
    result$nextResult <- append(result$nextResult, list(nextResult))

    result
  }
)
