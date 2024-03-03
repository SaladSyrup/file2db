#' @name addNewTask-method
#' @aliases addNewTask
#' @rdname addTask-method
#' @export
methods::setGeneric("addNewTask", function(job, ...) standardGeneric("addNewTask"),
  signature = c("job")
)

#-------------------------------------------------------------------------------
#' @name addNewTask,f2dbJob-method
#' @rdname addTask-method
#' @export
methods::setMethod(
  "addNewTask", signature(job = "f2dbJob"),
  function(job, name,
           taskFunction,
           ...,
           inputName,
           itemName) {
    stopifnot(methods::validObject(job))

    n <- sys.parent()
    env <- rlang::caller_env(n)
    params <- list()

    if (methods::hasArg(name)) params[["name"]] <- rlang::enexpr(name)
    if (methods::hasArg(taskFunction)) params[["taskFunction"]] <- rlang::enexpr(taskFunction)
    params <- c(params, rlang::enexprs(...))
    if (methods::hasArg(inputName)) params[["inputName"]] <- rlang::enexpr(inputName)
    if (methods::hasArg(itemName)) params[["itemName"]] <- rlang::enexpr(itemName)

    taskCall <- rlang::expr(f2dbTask(!!!params))
    task <- eval(taskCall, env)

    addTask(job, task)
  }
)
