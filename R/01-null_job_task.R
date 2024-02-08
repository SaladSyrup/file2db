methods::setClass("null_job_task", contains = "job_task")

methods::setMethod("initialize", "null_job_task", function(.Object) .Object)
