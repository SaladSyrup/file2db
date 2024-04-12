#' @name f2dbShow,f2dbJob-method
#' @noRd
methods::setMethod("f2dbShow", "f2dbJob", function(object) {
  x <- c(name = paste0("<", class(object)[1], "> ", name(object)))
  x <- c(x, jobInput = paste0("  jobInput: ", jobInput(object)))
  c(x, taskCount = paste0("  Num tasks: ", length(object@taskList)))
})
