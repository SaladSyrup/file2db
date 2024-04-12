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
    info("=============================================")
    info("Running ", f2dbShow(object)[["name"]])

    numJobs <- length(object@jobList)
    if (numJobs == 0) {
      info(name(object), ": No jobs to run")
      return(TRUE)
    }

    info(name(object), ": ", numJobs, " jobs in batch:")
    for (n in 1:numJobs) {
      info("+--> ", n, "/", numJobs, ": ", name(object@jobList[[n]]))
    }

    success <- TRUE
    failedJobs <- 0
    for (n in 1:numJobs) {
      debug(name(object), ": Starting job ", n, "/", numJobs)
      if (f2dbRun(jobList(object)[[n]]) == TRUE) {
        info(name(object), ": Job ", n, "/", numJobs, " completed succesfully")
      } else {
        success <- FALSE
        failedJobs <- failedJobs + 1
        error(name(object), ": Job ", n, "/", numJobs, " unsuccessful")
      }
    }

    if (success == TRUE) {
      info(name(object), ": All jobs completed succesfully")
    } else {
      error(name(object), ": ", failedJobs, " jobs failed")
    }

    success
  }
)
