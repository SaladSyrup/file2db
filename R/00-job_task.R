methods::setClass("job_task")
methods::setClass("job_task",

  slots = c(
    next_task = "job_task"
  ),

  prototype = list(
    next_task = NULL
  )
)

methods::setMethod("initialize", "job_task",

 function(.Object) {
   .Object@next_task <- new("null_job_task")
   .Object
 })

