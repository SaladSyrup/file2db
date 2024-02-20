#' f2dbJob class
#'
#' A container for job tasks.
#'
#' @slot name Job name. This should be unique within the job batch.
#' @slot jobInput Input supplied to the first task.
#' @slot tasks List of job tasks.
#'
#' @name f2dbJob-class
#' @docType class
#' @family f2dbJob
#' @family f2db classes
#' @export
methods::setClass("f2dbJob",
  contains = c("f2dbObject"),
  slots = c(
    jobInput = "ANY",
    tasks = "list"
  ),
  prototype = list(
    jobInput = NULL,
    tasks = list()
  )
)

#-------------------------------------------------------------------------------
#' f2dbJob
#'
#' `f2dbJob` constructor.
#'
#' @param name Job name. This should be unique within the job batch.
#' @param input Input supplied to the first task.
#'
#' @returns An `f2dbJob` object.
#'
#' @family f2dbJob
#' @export
f2dbJob <- function(name, input) {
  if (!methods::hasArg(name)) {
    name <- "<Unnamed f2dbTask>"
  } else {
    name <- as.character(name)
  }

  methods::new("f2dbJob", name = name, jobInput = input)
}

#-------------------------------------------------------------------------------
# Accessors
#-------------------------------------------------------------------------------
