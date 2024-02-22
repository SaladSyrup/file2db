#' addTask
#'
#' Adds an existing `f2dbTask` object to the end of the task list and updates
#' the `nextTask` slot of the next-to-last task to point to the newly added task.
#'
#' @param job The `f2dbJob` to add a job to.
#' @param task The `f2dbTask` to add.
#'
#' @returns `NULL`, invisibly.
#'
#' @name addTask-method
#' @aliases addTask
#' @docType methods
#' @family f2dbJob
#' @export
methods::setGeneric("addTask", function(job, task) standardGeneric("addTask"),
                    signature = c("job", "task")
)

#-------------------------------------------------------------------------------
#' @name addTask,f2dbJob,f2dbTask-method
#' @rdname addTask-method
#' @export
methods::setMethod("addTask", signature(job = "f2dbJob", task = "f2dbTask"),
                   function(job, task) {

                     stopifnot(methods::validObject(job))
                     stopifnot(methods::validObject(task))

                     jobSymbol <- match.call(appendTask, rlang::current_call())$job
                     stopifnot(is.symbol(jobSymbol))
                     env <- rlang::caller_env()

                     appendTask(job, task, rlang::as_string(jobSymbol), env)

                     invisible()
                   })
