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
#' \item{output}{Task function output}
#' \item{cnds}{Messages generated by the task function.}
#'
#' @name f2dbRun,f2dbTaskFunction-method
#' @docType methods
#' @family f2dbTaskFunction
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbTaskFunction",
  function(object, input = NA, item = NA) {
    debug("Running ", f2dbShow(object)[["name"]])
    debug("taskCall: ", rlang::expr_deparse(object@taskCall))
    debug("taskInput: ", typeof(input))
    debug("taskItem: ", item)

    success <- TRUE
    output <- NULL

    cnds <- list()

    logWarning <- function(cnd) {
      cnds <<- append(cnds, list(cnd))
      debug(name(object), ": ", rlang::cnd_message(cnd))
      rlang::cnd_muffle(cnd)
    }

    logInfo <- function(cnd) {
      cnds <<- append(cnds, list(cnd))
      debug(name(object), ": ", rlang::cnd_message(cnd))
      rlang::cnd_muffle(cnd)
    }

    callData <- list(taskInput = input, taskItem = item)

    tryCatch(
      error = function(cnd) {
        success <<- FALSE
        cnds <<- append(cnds, list(cnd))
        debug(name(object), ": ", rlang::cnd_message(cnd))
      },
      withCallingHandlers(
        message = logInfo,
        warning = logWarning,
        output <- rlang::eval_tidy(taskCall(object), callData)
      )
    )

    ## TODO: Deal with problem attribute from readr:: methods

    list(success = success, output = output, cnds = cnds)
  }
)
