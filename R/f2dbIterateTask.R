#' f2dbIterateTask
#'
#' A `f2dbIterateTask` iterates over `taskInput`, running running
#' subsequent `taskFunction`s with each element as input.
#'
#' @name f2dbIterateTask-class
#' @docType class
#' @family f2dbTask
#' @family f2dbIterateTask
#' @family f2db classes
#' @export
methods::setClass("f2dbIterateTask", contains = c("f2dbTask"))

#-------------------------------------------------------------------------------
#' f2dbIterateTask
#'
#' `f2dbIterateTask` constructor.
#'
#' @inheritParams f2dbTask
#'
#' @returns An `f2dbIterateTask` object.
#'
#' @family f2dbIterateTask
#' @export
f2dbIterateTask <- rlang::new_function(formals(f2dbTask), rlang::expr({
  parentConst <- rlang::call_match()
  parentConst[[1]] <- as.name("f2dbTask")
  methods::new("f2dbIterateTask", rlang::eval_tidy(parentConst))
}))
