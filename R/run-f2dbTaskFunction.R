#' @rdname f2dbRun-method
#' @export
methods::setMethod(
  "f2dbRun", "f2dbTaskFunction",
  function(object, input = NA, item = NA) {
    callEnv <- rlang::env(rlang::caller_env(), taskInput = input, batchItem = item)

    output <- eval(taskCall(object), callEnv)

    list(success = TRUE, ouput = output)
  }
)
