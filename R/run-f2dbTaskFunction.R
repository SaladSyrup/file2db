#' f2dbRun
#'
#' Runs the given task function.
#'
#' @param object An `f2dbTaskFunction` object.
#' @param input Task input to pass to the task function.
#' @param item The current item being processed.
#'
#' @inherit f2dbRun-method return
#'
#' @name f2dbRun,f2dbTaskFunction-method
#' @docType methods
#' @family f2dbTaskFunction
#' @family f2dbTaskFunction methods
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbTaskFunction",
  function(object, input = NA, item = NA) {
    callEnv <- rlang::env(rlang::caller_env(), taskInput = input, batchItem = item)

    output <- eval(taskCall(object), callEnv)

    list(success = TRUE, ouput = output)
  }
)
