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
f2dbListTask <- rlang::new_function(formals(f2dbTask), rlang::expr({
  parentConst <- rlang::call_match()
  parentConst[[1]] <- as.name("f2dbTask")
  listTask <- methods::new("f2dbTask", rlang::eval_tidy(parentConst))

  nextTask(listTask) <- f2dbIterateTask
}))
