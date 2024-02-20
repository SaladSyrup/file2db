#' f2dbRun
#'
#' `f2dbRun(f2dbEndTask)` will always return `TRUE`.
#'
#' @param object An `f2dbEndTask` object.
#' @param input *Ignored*.
#' @param item *Ignored*
#'
#' @returns A list:
#' \item{success}{`TRUE`}
#' \item{output}{"{<}End of task list{>}"}
#'
#' @name f2dbRun,f2dbEndTask-method
#' @docType methods
#' @family f2dbEndTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbEndTask",
  function(object, input = NA, item = NA) list(success = TRUE, output = name(object))
)
