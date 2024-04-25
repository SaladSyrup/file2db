#' f2dbEndTask class
#'
#' A `f2dbEndTask` object marks the end of a task list.
#'
#' The `taskFunction` and `nextTask` slots of a `f2dbEndTask` object cannot be
#' modified.
#'
#' `f2dbRun(f2dbEndTask)` will always return `TRUE`.
#'
#' @name f2dbEndTask-class
#' @docType class
#' @family f2dbTask
#' @family f2dbEndTask
#' @family f2db classes
#' @export
methods::setClass("f2dbEndTask", contains = c("f2dbTask"))

#-------------------------------------------------------------------------------
#' f2dbEndTask
#'
#' `f2dbEndTask` constructor
#'
#' @returns An `f2dbEndTask` object
#' @family f2dbEndTask
#' @export
f2dbEndTask <- function() {
  methods::new("f2dbEndTask", name = "<End of task list>", taskFunction = f2dbTaskFunction(), nextTask = f2dbObject("END"))
}

#-------------------------------------------------------------------------------
# Accessors
#-------------------------------------------------------------------------------
#' @name taskFunction<-,f2dbEndTask,f2dbTaskFunction-method
#' @rdname taskFunction-set-method
#' @export
methods::setMethod(
  "taskFunction<-",
  signature(object = "f2dbEndTask", value = "f2dbTaskFunction"),
  function(object, value) stop("Cannot modify the taskFunction of a f2dbEndTask object")
)

#-------------------------------------------------------------------------------
#' @name nextTask<-,f2dbEndTask,f2dbTask-method
#' @rdname nextTask-set-method
#' @export
methods::setMethod(
  "nextTask<-",
  signature(object = "f2dbEndTask", value = "f2dbTask"),
  function(object, value) stop("Cannot modify the nextTask of a f2dbEndTask object")
)
