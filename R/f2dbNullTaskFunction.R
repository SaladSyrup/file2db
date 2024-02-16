#' f2dbNullTaskFunction class
#'
#' A do-nothing, pass-through task function. This is the default task function
#' when creating a new `f2dbTask` object.
#'
#' @name f2dbNullTaskFunction.class
#' @docType class
#' @family file2dbTaskFunction
#' @family file2db classes
#' @export
methods::setClass("f2dbNullTaskFunction", contains = "f2dbTaskFunction")

#' @rdname f2dbRun.f2dbTaskFunction
#' @export
methods::setMethod(
  "f2dbRun", "f2dbNullTaskFunction",
  function(object, input = NA, item = NA) {
    list(success = TRUE, ouput = input)
  }
)
