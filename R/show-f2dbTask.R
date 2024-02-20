#' showTask
#'
#' Pretty-prints for show()
#'
#' @param object An `f2dbTask`.
#'
#' @returns A character vector
#'
#' @noRd
showTask <- function(object) {
  rslt <- paste0("Task: ", name(object))
  rslt <- c(rslt, paste0("Function: ", rlang::expr_deparse(taskCall(taskFunction(object)))))
  rslt <- c(rslt, paste0("Next task: ", name(nextTask(object))))
}

#-------------------------------------------------------------------------------
#' show
#'
#' @param object An `f2dbTask` to show.
#'
#' @name show,f2dbTask-method
#' @docType methods
#' @noRd
methods::setMethod("show", "f2dbTask", function(object) cat(showTask(object), sep = "\n"))
