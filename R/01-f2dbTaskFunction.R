#' f2dbTaskFunction class
#'
#' @description
#' The `f2dbTaskFunction` class encapsulates the function that accomplishes the
#' actual work of the task. Along with [f2dbRun()] method, this class is
#' responsible for:
#'
#' 1. Calling the underlying task function.
#' 1. Capturing any warnings or errors.
#' 1. Returning task function output.
#' 1. Returning whether or not the task executed successfully.
#'
#' @details
#' Classes inheriting from `f2dbTaskFunction` should fulfill the
#' responsibilities listed above and implement the [f2dbRun()] generic method.
#'
#' @slot taskCall A call to the underlying task function.
#' @slot env Environment in which the task function will be executed.
#'
#' @name f2dbTaskFunction-class
#' @docType class
#' @family f2dbTaskFunction
#' @family f2db classes
#' @export
methods::setClass("f2dbTaskFunction",
  contains = "f2dbObject",
  slots = c(
    taskCall = "call",
    env = "environment"
  ),
  prototype = list(
    taskCall = NULL,
    env = NULL
  )
)

#-------------------------------------------------------------------------------
#' f2dbTaskFunction
#'
#' `f2dbTaskFunction` constructor.
#'
#' @param taskFunction Function to call when running the task.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Parameters passed to the task
#'   function.
#' @param inputName By default, task input is passed to the task function as an
#'   unnamed first parameter. If `inputName` is provided, task input will be
#'   passed using `inputName`.
#' @param itemName By default, the current batch item is not passed to the task
#'   function. If `itemName` is provided, the batch item will be passed using
#'   `itemName`.
#' @param env Environment in which the task function will be executed.
#'
#' @returns An `f2dbTaskFunction` object.
#'
#' @family f2dbTaskFunction
#' @export
f2dbTaskFunction <- function(taskFunction,
                             ...,
                             inputName,
                             itemName,
                             env = rlang::caller_env()) {
  if (methods::hasArg("taskFunction")) {
    taskFunction <- rlang::enexpr(taskFunction)
  } else {
    taskFunction <- function(x) x
  }
  stopifnot(rlang::is_callable(taskFunction))

  params <- c(rlang::expr(taskInput), rlang::enexprs(...))

  if (methods::hasArg(inputName)) {
    stopifnot(identical(inputName, make.names(inputName)))
    names(params)[1] <- inputName
  }

  if (methods::hasArg(itemName)) {
    stopifnot(identical(itemName, make.names(itemName)))
    params[[itemName]] <- rlang::expr(batchItem)
  }

  methods::new("f2dbTaskFunction", taskCall = rlang::call2(taskFunction, !!!params), env = env)
}

#-------------------------------------------------------------------------------
#' taskCall
#'
#' Returns the task call of an `f2dbTaskFunction`.
#'
#' @param object An `f2dbTaskFunction` object
#'
#' @returns The `taskCall` evaluated when the given `f2dbTaskFunction` is run.
#'
#' @name taskCall-method
#' @aliases taskCall
#' @docType methods
#' @family f2dbTaskFunction
#' @family f2dbTaskFunction methods
#' @export
methods::setGeneric("taskCall",
                    function(object) standardGeneric("taskCall"),
                    signature = "object"
)

#-------------------------------------------------------------------------------
#' @name taskCall,f2dbTaskFunction-method
#' @rdname taskCall-method
#' @export
methods::setMethod("taskCall", "f2dbTaskFunction", function(object) {
  object@taskCall
})

#-------------------------------------------------------------------------------
#' f2dbRun
#'
#' Runs the given task function.
#'
#' @param object An `f2dbTaskFunction` object.
#' @param input Task input to pass to the task function.
#' @param item The current item being processed.
#'
#' @inherit f2dbRun-method return
#'
#' @name f2dbRun,f2dbTaskFunction-method
#' @docType methods
#' @family f2dbTaskFunction
#' @family f2dbTaskFunction methods
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbTaskFunction",
  function(object, input = NA, item = NA) {
    callEnv <- rlang::env(object@env, taskInput = input, batchItem = item)

    output <- eval(object@taskCall, callEnv)

    list(success = TRUE, ouput = output)
  }
)
