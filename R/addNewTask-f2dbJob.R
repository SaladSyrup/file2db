#' @name addNewTask-method
#' @docType methods
#' @export
methods::setGeneric("addNewTask", function(job, name,
                                           taskFunction,
                                           ...,
                                           inputName,
                                           itemName,
                                           env = rlang::caller_env()) {
  standardGeneric("addNewTask")
},
signature = c("job")
)

#-------------------------------------------------------------------------------
#' @name addNewTask,f2dbJob-method
#' @export
methods::setMethod(
  "addNewTask", signature(job = "f2dbJob"),
  function(job, name,
           taskFunction,
           ...,
           inputName,
           itemName,
           env = rlang::caller_env()) {
    stopifnot(validObject(job))

    params <- list()
    if (methods::hasArg(name)) params[["name"]] <- rlang::enexpr(name)
    if (methods::hasArg(taskFunction)) params[["taskFunction"]] <- rlang::enexpr(taskFunction)
    params <- c(params, rlang::enexprs(...))
    if (methods::hasArg(inputName)) params[["inputName"]] <- rlang::enexpr(inputName)
    if (methods::hasArg(itemName)) params[["itemName"]] <- rlang::enexpr(itemName)
    taskCall <- rlang::expr(f2dbTask(!!!params))
    task <- eval(taskCall, env)

    jobSymbol <- match.call(addNewTask, rlang::current_call())$job
    stopifnot(is.symbol(jobSymbol))
    jobEnv <- rlang::caller_env()

    appendTask(job, task, jobSymbol, jobEnv)

    invisible()
  }
)
