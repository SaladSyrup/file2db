#' f2dbBatch class
#'
#' A container for jobs.
#'
#' @slot name Batch name.
#' @slot jobList List of jobs
#'
#' @name f2dbBatch-class
#' @docType class
#' @family f2dbBatch
#' @family f2db classes
#' @export
methods::setClass("f2dbBatch",
  contains = c("f2dbObject"),
  slots = c(
    jobList = "list"
  ),
  prototype = list(
    jobList = list()
  )
)

#-------------------------------------------------------------------------------
#' f2dbBatch
#'
#' `f2dbBatch` constructor.
#'
#' @param batchName Batch name.
#'
#' @returns An `f2dbBatch` object.
#'
#' @family f2dbBatch
#' @export
f2dbBatch <- function(batchName) {
  if (!methods::hasArg(batchName)) {
    batchName <- "Unnamed"
  } else {
    batchName <- as.character(batchName)
  }

  methods::new("f2dbBatch", name = batchName)
}

#-------------------------------------------------------------------------------
# Accessors
#-------------------------------------------------------------------------------
#' jobList
#'
#' Returns jobList.
#'
#' @param object An `f2dbBatch` object.
#'
#' @returns A list of `f2dbJob`s.
#'
#' @name jobList-method
#' @aliases jobList
#' @docType methods
#' @family f2dbBatch
#' @export
methods::setGeneric("jobList",
  function(object) standardGeneric("jobList"),
  signature = "object"
)

#-------------------------------------------------------------------------------
#' @rdname jobList-method
#' @export
methods::setMethod("jobList", "f2dbBatch", function(object) object@jobList)

#-------------------------------------------------------------------------------
#' jobList<-
#'
#' Add `f2dbJob` objects to the end of the job list.
#'
#' @param object An `f2dbBatch`.
#' @param value An `f2dbJob`. Set to `NULL` to clear jobList.
#'
#' @name jobList-set-method
#' @aliases jobList<-
#' @docType methods
#' @family f2dbBatch
#' @export
methods::setGeneric("jobList<-",
  function(object, value) standardGeneric("jobList<-"),
  signature = c("object", "value")
)

#-------------------------------------------------------------------------------
#' @rdname jobList-set-method
#' @export
methods::setMethod(
  "jobList<-",
  signature(object = "f2dbBatch", value = "f2dbJob"),
  function(object, value) {
    stopifnot(methods::validObject(object))
    stopifnot(methods::validObject(value))

    if (length(object@jobList) == 0) {
      object@jobList <- list(value)
    } else {
      object@jobList <- append(object@jobList, value)
    }

    return(object)
  }
)

#-------------------------------------------------------------------------------
#' @rdname jobList-set-method
#' @export
methods::setMethod(
  "jobList<-",
  signature(object = "f2dbBatch", value = "list"),
  function(object, value) {
    stopifnot(methods::validObject(object))

    for (job in value) {
      jobList(object) <- job
    }

    return(object)
  }
)

#-------------------------------------------------------------------------------
#' @rdname jobList-set-method
#' @export
methods::setMethod(
  "jobList<-",
  signature(object = "f2dbBatch", value = "NULL"),
  function(object, value) {
    stopifnot(methods::validObject(object))

    object@jobList <- list()

    return(object)
  }
)
