#' f2dbTask class
#'
#' A class for encapsulating an individual task.
#'
#' @slot name Task name. This should be unique within the job.
#' @slot taskFunction An `f2dbTaskFunction` object to perform the actual work of
#'   the task.
#' @slot nextTask An `f2dbTaskObject` object that performs the next task in the
#'   job sequence. `nextTask` takes the output of the current task as its input.
#'
#' @name f2dbTask-class
#' @docType class
#' @family f2dbTask
#' @family f2db classes
#' @export
methods::setClass("f2dbTask",
  contains = c("f2dbObject"),
  slots = c(
    taskFunction = "f2dbTaskFunction",
    nextTask = "f2dbObject"
  ),
  prototype = list(
    taskFunction = NULL,
    nextTask = NULL
  )
)

#-------------------------------------------------------------------------------
#' f2dbTask
#'
#' `f2dbTask` constructor.
#'
#' @param taskName Task name.
#' @inheritParams f2dbTaskFunction
#'
#' @returns An `f2dbTask` object.
#'
#' @family f2dbTask
#' @export
f2dbTask <- function(taskName,
                     taskFunction,
                     ...,
                     inputName,
                     itemName) {
  if (!methods::hasArg(taskName)) {
    taskName <- "Unnamed"
  } else {
    taskName <- as.character(taskName)
  }

  params <- list()
  if (methods::hasArg(taskFunction)) params[["taskFunction"]] <- rlang::enquo(taskFunction)
  params <- c(params, rlang::enquos(...))
  if (methods::hasArg(inputName)) params[["inputName"]] <- rlang::enquo(inputName)
  if (methods::hasArg(itemName)) params[["itemName"]] <- rlang::enquo(itemName)
  taskFunctionCall <- rlang::expr(f2dbTaskFunction(!!!params))
  taskFunction <- rlang::eval_tidy(taskFunctionCall)

  methods::new("f2dbTask", name = taskName, taskFunction = taskFunction, nextTask = f2dbObject("END"))
}

#-------------------------------------------------------------------------------
# Accessors
#-------------------------------------------------------------------------------
#' taskFunction
#'
#' Returns the `f2dbTaskFunction` held by the `f2dbTask`
#'
#' @param object An `f2dbTask`
#'
#' @returns An `f2dbTaskFunction`
#'
#' @name taskFunction-method
#' @aliases taskFunction
#' @docType methods
#' @family f2dbTask
#' @export
methods::setGeneric("taskFunction",
  function(object) standardGeneric("taskFunction"),
  signature = "object"
)

#-------------------------------------------------------------------------------
#' @name taskFunction,f2dbTask-method
#' @rdname taskFunction-method
#' @export
methods::setMethod("taskFunction", "f2dbTask", function(object) object@taskFunction)

#-------------------------------------------------------------------------------
#' taskFunction<-
#'
#' Sets the `f2dbTaskFunction` held by the `f2dbTask`.
#'
#' @param object An `f2dbTask`.
#' @param value An `f2dbTaskFunction`.
#'
#' @name taskFunction-set-method
#' @aliases taskFunction<-
#' @docType methods
#' @family f2dbTask
#' @export
methods::setGeneric("taskFunction<-",
  function(object, value) standardGeneric("taskFunction<-"),
  signature = c("object", "value")
)

#-------------------------------------------------------------------------------
#' @name taskFunction<-,f2dbTask,f2dbTaskFunction-method
#' @rdname taskFunction-set-method
#' @export
methods::setMethod(
  "taskFunction<-",
  signature(object = "f2dbTask", value = "f2dbTaskFunction"),
  function(object, value) {
    stopifnot(methods::validObject(value))
    object@taskFunction <- value
    object
  }
)

#-------------------------------------------------------------------------------
#' nextTask
#'
#' Returns the `nextTask`.
#'
#' @param object An `f2dbTask`
#'
#' @returns An `f2dbTask`
#'
#' @name nextTask-method
#' @aliases nextTask
#' @docType methods
#' @family f2dbTask
#' @export
methods::setGeneric("nextTask",
  function(object) standardGeneric("nextTask"),
  signature = "object"
)

#-------------------------------------------------------------------------------
#' @name nextTask,f2dbTask-method
#' @rdname nextTask-method
#' @export
methods::setMethod("nextTask", "f2dbTask", function(object) object@nextTask)

#-------------------------------------------------------------------------------
#' nextTask<-
#'
#' Sets the `nextTask`.
#'
#' @param object An `f2dbTask`.
#' @param value An `f2dbTask`.
#'
#' @name nextTask-set-method
#' @aliases nextTask<-
#' @docType methods
#' @family f2dbTask
#' @export
methods::setGeneric("nextTask<-",
  function(object, value) standardGeneric("nextTask<-"),
  signature = c("object", "value")
)

#-------------------------------------------------------------------------------
#' @name nextTask<-,f2dbTask,f2dbTask-method
#' @rdname nextTask-set-method
#' @export
methods::setMethod(
  "nextTask<-",
  signature(object = "f2dbTask", value = "f2dbTask"),
  function(object, value) {
    stopifnot(methods::validObject(value))
    object@nextTask <- value
    object
  }
)
