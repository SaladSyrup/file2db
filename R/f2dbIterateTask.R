#' f2dbIterateTask
#'
#' A `f2dbIterateTask` iterates over `taskInput`, running running
#' subsequent `taskFunction`s with each element as input.
#'
#' The `taskFunction` slot of a `f2dbIterateTask` object cannot be modified.
#'
#' @name f2dbIterateTask-class
#' @docType class
#' @family f2dbTask
#' @family f2dbIterateTask
#' @family f2db classes
#' @export
methods::setClass("f2dbIterateTask", contains = c("f2dbTask"))

#-------------------------------------------------------------------------------
#' f2dbIterateTask
#'
#' `f2dbIterateTask` constructor.
#'
#' @param taskName Task name.
#'
#' @returns An `f2dbIterateTask` object.
#'
#' @family f2dbIterateTask
#' @export
f2dbIterateTask <- function(taskName) {
  if (!methods::hasArg(taskName)) {
    taskName <- "f2dbIterateTask"
  } else {
    taskName <- as.character(taskName)
  }

  methods::new("f2dbIterateTask", name = taskName, taskFunction = f2dbTaskFunction(), nextTask = f2dbObject("END"))
}

#-------------------------------------------------------------------------------
# Accessors
#-------------------------------------------------------------------------------
#' @name taskFunction<-,f2dbEndTask,f2dbTaskFunction-method
#' @rdname taskFunction-set-method
#' @export
methods::setMethod(
  "taskFunction<-",
  signature(object = "f2dbIterateTask", value = "f2dbTaskFunction"),
  function(object, value) stop("Cannot modify the taskFunction of a f2dbIterateTask object")
)
