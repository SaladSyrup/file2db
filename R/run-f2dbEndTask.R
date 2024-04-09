#' f2dbRun
#'
#' Run an `f2dbEndTask`.
#'
#' `f2dbEndTask`s mark then end of a task list and will always return `TRUE`.
#'
#' @param object An `f2dbEndTask` to run.
#' @inheritParams f2dbRun,f2dbTask-method
#'
#' @returns `TRUE`
#'
#' @name f2dbRun,f2dbEndTask-method
#' @docType methods
#' @family f2dbTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbEndTask",
  function(object, input = NA, item = NA) {
    debug("END: End of task list")
    return(TRUE)
  }
)
