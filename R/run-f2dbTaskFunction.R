#' f2dbRun
#'
#' Run an `f2dbTaskFunction`.
#'
#' @param object An `f2dbTaskFunction` to run.
#' @param input Input to pass to the underlying task function.
#' @param item The job item being processed. This is passed to the task function
#' if the `f2dbTaskFunction` was created with an `itemName`.
#'
#' @returns
#' A list:
#' \item{success}{Logical value indicating success (`TRUE`) or failure (`FALSE`).
#'   Success only indicates that it is safe to proceed; it does not mean there
#'   are no warnings or errors.}
#' \item{object}{The type of object being run.}
#' \item{name}{The name of the object being run.}
#' \item{output}{Task function output}
#' \item{messages}{Captured error, warning, or informational messages.}
#'
#' @name f2dbRun,f2dbTaskFunction-method
#' @docType methods
#' @family f2dbTaskFunction
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbTaskFunction",
  function(object, input = NA, item = NA) {
    callEnv <- rlang::env(rlang::caller_env(), taskInput = input, batchItem = item)

    output <- eval(taskCall(object), callEnv)

    list(success = TRUE, object = class(object)[1], name = name(object), output = output, messages = "")
  }
)
