#' f2dbRun
#'
#' Run an `f2dbEndTask`.
#'
#' `f2dbEndTask`s mark then end of a task list and will always return `TRUE`.
#'
#' @param object An `f2dbEndTask` to run.
#' @inheritParams f2dbRun,f2dbTask-method
#'
#' @returns
#' A list:
#' \item{success}{`TRUE`.}
#' \item{object}{The type of object being run.}
#' \item{name}{The name of the object being run.}
#' \item{item}{The job item.}
#' \item{messages}{`character()`.}
#'
#' @name f2dbRun,f2dbEndTask-method
#' @docType methods
#' @family f2dbTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbEndTask",
  function(object, input = NA, item = NA) {
    list(
      success = TRUE, object = class(object)[1],
      name = name(object), item = item, messages = character()
    )
  }
)
