#' f2dbRun
#'
#' Run an `f2dbBatch`.
#'
#' When called on an `f2dbBatch` object, `f2dbRun` will run the jobs in the
#' `jobList`.
#'
#' @param object An `f2dbBatch` to run.
#'
#' @returns Logical value indicating success (`TRUE`) or failure (`FALSE`).
#' There may be warning or informational messages even if success is indicated.
#'
#' @name f2dbRun,f2dbBatch-method
#' @docType methods
#' @family f2dbBatch
#' @family f2dbRun methods
#' @export
methods::setMethod(
  "f2dbRun", "f2dbBatch",
  function(object) {
    return(TRUE)
  }
)
