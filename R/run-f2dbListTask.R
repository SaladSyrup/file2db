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
#' @name f2dbRun,f2dbListTask-method
#' @docType methods
#' @family f2dbListTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbListTask",
  function(object, input = NA, item = NA) {
    debug("Running ", f2dbShow(object)[["name"]])
    debug("taskFunction: ", f2dbShow(taskFunction(object))[["name"]])
    debug("nextTask: ", f2dbShow(nextTask(object))[["name"]])
    debug("taskInput: ", typeof(input))
    debug("taskItem: ", item)

    functionOutput <- f2dbRun(taskFunction(object), input, item)

    numCnds <- length(functionOutput[["cnds"]])
    if (numCnds > 0) {
      info(name(object), ": Task function generated ", numCnds, " messages")
      info("taskFunction: ", f2dbShow(taskFunction(object))[["name"]])
      info("taskCall: ", rlang::expr_deparse(taskFunction(object)@taskCall))
      info("taskInput: ", typeof(input))
      info("taskItem: ", item)
      lapply(functionOutput[["cnds"]], logCondition)
    } else {
      debug(name(object), ": Task function generated no messages")
    }

    if (functionOutput$success == FALSE) {
      error(name(object), ": Task function unsuccessful")
      info("taskFunction: ", f2dbShow(taskFunction(object))[["name"]])
      info("taskCall: ", rlang::expr_deparse(taskFunction(object)@taskCall))
      info("taskInput: ", typeof(input))
      info("taskItem: ", item)
      return(FALSE)
    }

    itemList <- as.list(functionOutput$output)
    numItems <- length(itemList)
    if (numItems == 0) {
      info(name(object), ": List task returned no items")
      return(TRUE)
    }

    success <- TRUE
    failedItems <- 0
    info(name(object), ": List task returned ", numItems, " items")
    for (n in 1:numItems) {
      info("Item ", n, "/", numItems, ": ", itemList[[n]])

      if (f2dbRun(nextTask(object), itemList[[n]], itemList[[n]]) == FALSE) {
        success <- FALSE
        failedItems <- failedItems + 1
        error("Item ", n, "/", numItems, ": Item unsuccessful")
      } else {
        debug("Item ", n, "/", numItems, ": Completed successfully")
      }
    }

    if (success == TRUE) {
      info(name(object), ": Completed ", numItems, " items successfully")
    } else {
      error(name(object), ": ", failedItems, "/", numItems, " items unsuccessful")
    }

    success
  }
)
