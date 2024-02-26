#' f2dbRun
#'
#' Run an `f2dbEndTask`.
#'
#' `f2dbEndTask`s mark then end of a task list and will always return `TRUE`.
#'
#' @param object An `f2dbEndTask` to run.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Unused.
#'
#' @returns
#' A list:
#' \item{success}{`TRUE`.}
#' \item{object}{The type of object being run.}
#' \item{name}{The name of the object being run.}
#' \item{messages}{Character vector "".}
#'
#' @name f2dbRun,f2dbEndTask-method
#' @docType methods
#' @family f2dbTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbEndTask",
  function(object, ...) {
    list(
      success = TRUE, object = class(object)[1],
      name = name(object), messages = ""
    )
  }
)
