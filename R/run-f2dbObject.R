#' @rdname f2dbRun-method
#' @export
methods::setMethod(
  "f2dbRun", "f2dbObject",
  function(object, ...) {
    list(success = TRUE, output = name(object))
  }
)
