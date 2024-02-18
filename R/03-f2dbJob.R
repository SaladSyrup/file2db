#' f2dbJob class
#'
#' @name f2dbJob-class
#' @docType class
#' @family f2dbJob
#' @family f2db classes
#' @export
methods::setClass("f2dbJob",
  contains = c("f2dbObject"),
  slots = c(
    name = "character",
    firstTask = "f2dbTask",
    jobInput = "ANY"
  ),
  prototype = list(
    name = "",
    firstTask = NULL,
    jobInput = NULL
  )
)
