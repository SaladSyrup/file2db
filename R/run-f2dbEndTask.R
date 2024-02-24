#' @rdname f2dbRun-method
#' @export
methods::setMethod(
  "f2dbRun", "f2dbEndTask",
  function(object, input = NA, item = NA) list(success = TRUE, output = name(object))
)
