#' f2dbEndTask class
#'
#' A do-nothing task that marks the end of a task list. This is the default
#' `nextTask` for `f2dbTask` instances.
#'
#' @name f2dbEndTask-class
#' @docType class
#' @family f2dbEndTask
#' @family f2dbTask
#' @family f2db classes
#' @export
methods::setClass("f2dbEndTask", contains = c("f2dbTask"))

#-------------------------------------------------------------------------------
#' f2dbEndTask
#'
#' `f2dbEndTask` constructor.
#'
#' @returns An `f2dbEndTask` object.
#' @family f2dbEndTask
#' @family f2dbTask
#' @export
f2dbEndTask <- function() {
  methods::new("f2dbEndTask", name = "<ENDTASK>")
}

#-------------------------------------------------------------------------------
#' f2dbRun
#' @name f2dbRun,f2dbEndTask-method
#' @rdname f2dbRun-f2dbTask-method
#' @export
methods::setMethod(
  "f2dbRun", "f2dbEndTask",
  function(object, input = NA, item = NA) {
    list(success = TRUE, output = object@name)
  }
)

#-------------------------------------------------------------------------------
#' initialize
#'
#' @description
#' Initializer function for `f2dbEndTask` objects.
#'
#' This initializer explicitly does not call the superclass `f2dbTask`
#' initializer to avoid complaints about `taskFunction` and `nextTask` being set
#' to `NULL`. This is acceptable because `f2dbRun` will not attempt to run
#' either one.
#'
#' @param .Object `f2dbEndTask` object.
#' @param name <ENDTASK>
#'
#' @returns `f2dbEndTask` object.
#'
#' @name initialize,f2dbEndTask-method
#' @docType methods
#' @noRd
methods::setMethod(
  "initialize", "f2dbEndTask",
  function(.Object, name) {
    .Object@name <- name
    .Object
  }
)

#-------------------------------------------------------------------------------
# Accessors
#-------------------------------------------------------------------------------
#' @name taskFunction<-,f2dbEndTask,f2dbTaskFunction-method
#' @rdname taskFunction-set-method
#' @export
methods::setMethod(
  "taskFunction<-",
  signature(object = "f2dbEndTask", value = "f2dbTaskFunction"),
  function(object, value) object
)

#-------------------------------------------------------------------------------
#' @name nextTask<-,f2dbEndTask,f2dbTask-method
#' @rdname nextTask-set-method
#' @export
methods::setMethod(
  "nextTask<-",
  signature(object = "f2dbEndTask", value = "f2dbTask"),
  function(object, value) object
)
