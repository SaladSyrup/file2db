#' f2dbTask class
#'
#' A class for encapsulating an individual task.
#'
#' @slot name Task name. This name normally be should be unique within the job.
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
    name = "character",
    taskFunction = "f2dbTaskFunction",
    nextTask = "f2dbObject"
  ),
  prototype = list(
    name = "",
    taskFunction = NULL,
    nextTask = NULL
  )
)

#-------------------------------------------------------------------------------
#' initialize
#'
#' Initializer function for `f2dbTask` objects.
#'
#' @param .Object `f2dbTask` object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Extra parameters.
#'
#' @returns Initialized `f2dbTask` object.
#'
#' @name initialize,f2dbTask-method
#' @docType methods
#' @noRd
methods::setMethod("initialize", "f2dbTask",
  function(.Object, ...) {
    .Object@nextTask <- methods::new("f2dbEndTask")
    methods::callNextMethod()
  }
)

#' f2dbTask
#'
#' Creates a new `f2dbTask` object.
#'
#' @param name Task name.
#' @inheritParams f2dbTaskFunction
#'
#' @family f2dbTask
#' @export
f2dbTask <- function(name = NA,
                    taskFunction = NA,
                    ...,
                    inputName = NA,
                    itemName = NA,
                    env = rlang::caller_env()) {
  if (is.na(name)) {
    name <- "<UNNAMED>"
  }

  if (missing(taskFunction)) {
    # Default do-nothing task function
    taskFunction <- f2dbTaskFunction(function(x) x)
  } else {
    taskFunction <- rlang::enexpr(taskFunction)
    taskFunctionDots <- rlang::enexprs(...)
    inputName <- rlang::enexpr(inputName)
    itemName <- rlang::enexpr(itemName)
    taskFunctionCall <- rlang::expr(f2dbTaskFunction(taskFunction = !!taskFunction, !!!taskFunctionDots, inputName = !!inputName, itemName = !!itemName))
    taskFunction <- eval(taskFunctionCall, env)
  }

  methods::new("f2dbTask", name = name, taskFunction = taskFunction)
}

#-------------------------------------------------------------------------------
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
methods::setMethod("f2dbRun", "f2dbTask",
  function(object, input = NA, item = NA) {
    functionOutput <- f2dbRun(object@taskFunction, input, item)

    if (functionOutput$success == FALSE) {
      return(list(taskName = object@name, success = FALSE, functionOutput = functionOutput$output))
    }

    taskOutput <- f2dbRun(object@nextTask, functionOutput$output, item)
    c(list(taskName = object@name, success = TRUE, functionOutput = functionOutput$output), taskOutput)
  }
)
