#' addTask, addNewTask
#'
#' Add a `f2dbTask` object to the end of the task list and update the `nextTask`
#' slot of the next-to-last task to point to the newly added task.
#'
#' @param job An `f2dbJob` to add a task to.
#' @param task An existing `f2dbTask` to add.
#' @inheritParams f2dbTask
#'
#' @returns `NULL`, invisibly.
#'
#' @name addTask-method
#' @aliases addTask
#' @docType methods
#' @family f2dbJob
#' @export
methods::setGeneric("addTask", function(job, task, ...) standardGeneric("addTask"),
  signature = c("job", "task")
)

#-------------------------------------------------------------------------------
#' @name addTask,f2dbJob-method
#' @rdname addTask-method
#' @export
methods::setMethod(
  "addTask", signature(job = "f2dbJob", task = "f2dbTask"),
  function(job, task) {
    stopifnot(methods::validObject(job))
    stopifnot(methods::validObject(task))

    jobSymbol <- match.call(addTask, rlang::current_call())$job
    stopifnot(is.symbol(jobSymbol))
    env <- rlang::caller_env()

    appendTask(job, task, rlang::as_string(jobSymbol), env)

    invisible()
  }
)
