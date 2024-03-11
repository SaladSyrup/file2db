#' f2dbRun
#'
#' Run an `f2dbListTask`.
#'
#' When called on an `f2dbListTask` object, `f2dbRun` will run `taskFunction`.
#' If `taskFunction` is successful, `f2dbRun` will then iterate over the output
#' of `taskFucntion`, running `nextTask` with `input` and `item` both set to the
#' output element.
#'
#' @param object An `f2dbListTask` to run.
#' @inheritParams f2dbRun,f2dbTask-method
#'
#' @inherit f2dbRun,f2dbTask-method return
#'
#' @name f2dbRun,f2dbListTask
#' @docType methods
#' @family f2dbListTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbListTask",
  function(object, input = NA, item = NA) {
    functionOutput <- f2dbRun(taskFunction(object), input, item)
    result <- list(
      success = functionOutput$success, object = class(object)[1],
      name = name(object), item = item, messages = functionOutput$messages,
      nextResult = list()
    )

    if (functionOutput$success == FALSE) {
      return(result)
    }

    for (element in as.list(functionOutput$output)) {
      nextResult <- f2dbRun(nextTask(object), element, element)

      result$success <- (result$success && nextResult$success)
      result$nextResult <- append(result$nextResult, list(nextResult))
    }

    result
  }
)
