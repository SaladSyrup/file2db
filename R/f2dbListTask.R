#' f2dbListTask
#'
#' A `f2bListTask` iterates over the output of its `taskFunction`, running
#' subsequent `taskFunction`s with each element as input.
#'
#' @name f2dbListTask-class
#' @docType class
#' @family f2dbTask
#' @family f2dbListTask
#' @family f2db classes
#' @export
methods::setClass("f2dbListTask", contains = c("f2dbTask"))

#-------------------------------------------------------------------------------
#' f2dbListTask
#'
#' `f2dbListTask` constructor.
#'
#' @inheritParams f2dbTask
#'
#' @returns An `f2dbListTask` object.
#'
#' @family f2dbListTask
#' @export
f2dbListTask <- function(taskName,
                         taskFunction,
                         ...,
                         inputName,
                         itemName) {
  params <- rlang::enquos(taskName, taskFunction, ..., inputName, itemName)
  taskCall <- rlang::expr(f2dbTask(!!!params))
  #methods::new("f2dbListTask", rlang::eval_tidy(taskCall))
  rlang::arg_match(formals())
}
