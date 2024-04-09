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
#' @returns Logical value indicating success (`TRUE`) or failure
#' (`FALSE`). `TRUE` is returned if both `taskFunction` and `nextTask` are
#' successful. Otherwise, `FALSE` is returned.
#'
#' @name f2dbRun,f2dbTask-method
#' @docType methods
#' @family f2dbTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbTask",
  function(object, input = NA, item = NA) {
    debug("Running ", f2dbShow(object)[["name"]])
    debug("  taskFunction: ", f2dbShow(taskFunction(object))[["name"]])
    debug("  nextTask: ", f2dbShow(nextTask(object))[["name"]])
    debug("  taskInput: ", typeof(input))
    debug("  taskItem: ", item)

    functionOutput <- f2dbRun(taskFunction(object), input, item)

    numCnds <- length(functionOutput[["cnds"]])
    if (numCnds > 0) {
      info(name(object), ": Task function generated ", numCnds, " messages")
      info("  taskFunction: ", f2dbShow(taskFunction(object))[["name"]])
      info("  taskCall: ", rlang::expr_deparse(taskFunction(object)@taskCall))
      info("  taskInput: ", typeof(input))
      info("  taskItem: ", item)
      lapply(functionOutput[["cnds"]], logCondition)
    } else {
      debug(name(object), ": Task function generated no messages")
    }

    if (functionOutput$success == FALSE) {
      error(name(object), ": Task function unsuccessful")
      info("  taskFunction: ", f2dbShow(taskFunction(object))[["name"]])
      info("  taskCall: ", rlang::expr_deparse(taskFunction(object)@taskCall))
      info("  taskInput: ", typeof(input))
      info("  taskItem: ", item)
      return(FALSE)
    }

    debug(name(object), ": Task function returned successfully")
    f2dbRun(nextTask(object), functionOutput$output, item)
  }
)

#-------------------------------------------------------------------------------
# Internal methods
#-------------------------------------------------------------------------------
#' logCondition
#'
#' This is an internal function.
#'
#' @param cnd A condition to be logged.
#'
#' @returns invisible()
#' @noRd
logCondition <- function(cnd) {
  if (rlang::cnd_inherits(cnd, "message") == TRUE) {
    info(rlang::cnd_message(cnd))
  } else if (rlang::cnd_inherits(cnd, "warning") == TRUE) {
    warn(rlang::cnd_message(cnd))
  } else if (rlang::cnd_inherits(cnd, "error") == TRUE) {
    error(rlang::cnd_message(cnd))
  } else if (rlang::cnd_inherits(cnd, "condition") == TRUE) {
    info(rlang::cnd_message(cnd))
  } else {
    error("Unrecognized condition")
  }
}
