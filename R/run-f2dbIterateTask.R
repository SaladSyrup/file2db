#' f2dbRun
#'
#' Run an `f2dbIterateTask`.
#'
#' When called on an `f2dbIterateTask` object, `f2dbRun` will iterate over the
#' `taskInput`, running `nextTask` with `input` and `item` both set to the
#' `taskInput` element.
#'
#' @param object An `f2dbIterateTask` to run.
#' @inheritParams f2dbRun,f2dbTask-method
#'
#' @inherit f2dbRun,f2dbTask-method return
#'
#' @name f2dbRun,f2dbIterateTask-method
#' @docType methods
#' @family f2dbIterateTask
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbIterateTask",
  function(object, input = NA, item = NA) {
    debug("Running ", f2dbShow(object)[["name"]])
    debug("+--> taskFunction: ", f2dbShow(taskFunction(object))[["name"]])
    debug("+--> nextTask: ", f2dbShow(nextTask(object))[["name"]])
    debug("+--> taskInput: ", typeof(input))
    debug("+--> taskItem: ", item)

    itemList <- as.list(input)
    numItems <- length(itemList)
    if (numItems == 0) {
      info(name(object), ": Input has no items")
      return(TRUE)
    }

    success <- TRUE
    failedItems <- 0
    info(name(object), ": Input contains ", numItems, " items")
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
