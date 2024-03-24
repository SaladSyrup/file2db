#' f2dbRun
#'
#' Run an `f2dbTaskFunction`.
#'
#' `f2dbRun` only returns `FALSE` if an error is thrown.
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
#'   are no warnings or messages.}
#' \item{object}{The type of object being run.}
#' \item{name}{The name of the object being run.}
#' \item{output}{Task function output}
#' \item{messages}{A list of captured error, warning, or informational messages.}
#'
#' @name f2dbRun,f2dbTaskFunction-method
#' @docType methods
#' @family f2dbTaskFunction
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbTaskFunction",
  function(object, input = NA, item = NA) {
    msgs <- list()
    success <- TRUE
    output <- NULL

    # logInfo(f2dbLogger, "Running", name(object), task)

    saveMsgs <- function(cnd) {
      msgs <<- append(msgs, paste0(class(cnd)[1], ": ", rlang::cnd_message(cnd)))
      rlang::cnd_muffle(cnd)
    }

    callData <- list(taskInput = input, batchItem = item)

    tryCatch(
      error = function(cnd) {
        success <<- FALSE
        msgs <<- append(msgs, paste0(class(cnd)[1], ": ", rlang::cnd_message(cnd)))
      },
      withCallingHandlers(
        message = saveMsgs,
        warning = saveMsgs,
        output <- rlang::eval_tidy(taskCall(object), callData)
      )
    )

    ## TODO: Deal with problem attribute from readr:: methods

    list(success = success, object = class(object)[1], name = name(object), output = output, messages = msgs)
  }
)
