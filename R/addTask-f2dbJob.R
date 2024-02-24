#' addTask, addNewTask
#'
#' Add a `f2dbTask` object to the end of the task list and update the `nextTask`
#' slot of the next-to-last task to point to the newly added task.
#'
#' @param job An `f2dbJob` to add a task to.
#' @param task An existing `f2dbTask` to add.
#' @param followNext Recursively adds `nextTask`
#' @inheritParams f2dbTask
#'
#' @returns `NULL`, invisibly.
#'
#' @name addTask-method
#' @aliases addTask
#' @docType methods
#' @family f2dbJob
#' @export
methods::setGeneric("addTask", function(job, task, followNext = FALSE, ...) standardGeneric("addTask"),
  signature = c("job", "task")
)

#-------------------------------------------------------------------------------
#' @rdname addTask-method
#' @export
methods::setMethod(
  "addTask", signature(job = "f2dbJob", task = "f2dbTask"),
  function(job, task, followNext = FALSE) {
    stopifnot(methods::validObject(job))
    stopifnot(methods::validObject(task))
    stopifnot(is.logical(followNext))

    n <- sys.parent()
    env <- rlang::caller_env(n)
    addCall <- match.call(addTask, sys.call(-n))

    jobSymbol <- addCall$job
    stopifnot(is.symbol(jobSymbol))

    appendTask(job, task, rlang::as_string(jobSymbol), env)

    if ((followNext == TRUE) && (isa(nextTask(task), "f2dbTask"))) {
      addCall[["task"]] <- nextTask(task)
      eval(addCall, env)
    }

    invisible()
  }
)

#-------------------------------------------------------------------------------
#' @rdname addTask-method
#' @export
methods::setMethod(
  "addTask", signature(job = "f2dbJob", task = "list"),
  function(job, task, followNext = FALSE) {
    stopifnot(methods::validObject(job))

    task <- as.list(task)
    n <- sys.parent()
    env <- rlang::caller_env(n)
    addCall <- match.call(addTask, sys.call(-n))

    for (t in task) {
      addCall[["task"]] <- t
      eval(addCall, env)
    }

    invisible()
  }
)
