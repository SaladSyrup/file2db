#' f2dbJob class
#'
#'
#'
#' @slot jobInput Input supplied to the first task.
#'
#' @name f2dbJob-class
#' @docType class
#' @family f2dbJob
#' @family f2db classes
#' @export
methods::setClass("f2dbJob",
  contains = c("f2dbObject"),
  slots = c(
    jobInput = "ANY"
  ),
  prototype = list(
    jobInput = NULL
  )
)
