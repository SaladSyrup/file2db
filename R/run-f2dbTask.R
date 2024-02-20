#' f2dbRun
#'
#' Executes the given task and passes output to the next task.
#'
#' @param object An `f2dbTask` object.
#' @param input Task input.
#' @inherit f2dbRun,f2dbTaskFunction-method params
#'
#' @inherit f2dbRun-method return
#'
#' @name f2dbRun,f2dbTask-method
#' @docType methods
#' @family f2dbTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbTask",
  function(object, input = NA, item = NA) {
    functionOutput <- f2dbRun(taskFunction(object), input, item)

    if (functionOutput$success == FALSE) {
      return(list(taskName = name(object), success = FALSE, functionOutput = functionOutput$output))
    }

    taskOutput <- f2dbRun(nextTask(object), functionOutput$output, item)
    c(list(taskName = name(object), success = TRUE, functionOutput = functionOutput$output), taskOutput)
  }
)
