#' @rdname f2dbRun-method
#' @export
methods::setMethod(
  "f2dbRun", "f2dbTask",
  function(object, input = NA, item = NA) {
    functionOutput <- f2dbRun(taskFunction(object), input, item)

    if (functionOutput$success == FALSE) {
      return(list(taskName = name(object), success = FALSE, functionOutput = functionOutput$output))
    }

    taskOutput <- f2dbRun(nextTask(object), functionOutput$output, item)
    list(list(taskName = name(object), success = TRUE, functionOutput = functionOutput$output), taskOutput)
  }
)
