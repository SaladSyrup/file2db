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
#' f2dbTask
#'
#' `f2dbTask` constructor.
#'
#' @param name Task name.
#' @inheritParams f2dbTaskFunction
#'
#' @returns An `f2dbTask` object.
#'
#' @family f2dbTask
#' @export
f2dbTask <- function(name,
                     taskFunction,
                     ...,
                     inputName,
                     itemName,
                     env = rlang::caller_env()) {
  if (!methods::hasArg(name)) name <- "<UNNAMED>"

  params <- list()
  if (methods::hasArg(taskFunction)) params[["taskFunction"]] <- rlang::enexpr(taskFunction)
  params <- c(params, rlang::enexprs(...))
  if (methods::hasArg(inputName)) params[["inputName"]] <- rlang::enexpr(inputName)
  if (methods::hasArg(itemName)) params[["itemName"]] <- rlang::enexpr(itemName)
  taskFunctionCall <- rlang::expr(f2dbTaskFunction(!!!params))
  taskFunction <- eval(taskFunctionCall, env)

  methods::new("f2dbTask", name = name, taskFunction = taskFunction, nextTask = f2dbEndTask())
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
methods::setMethod(
  "f2dbRun", "f2dbTask",
  function(object, input = NA, item = NA) {
    functionOutput <- f2dbRun(object@taskFunction, input, item)

    if (functionOutput$success == FALSE) {
      return(list(taskName = object@name, success = FALSE, functionOutput = functionOutput$output))
    }

    taskOutput <- f2dbRun(object@nextTask, functionOutput$output, item)
    c(list(taskName = object@name, success = TRUE, functionOutput = functionOutput$output), taskOutput)
  }
)
