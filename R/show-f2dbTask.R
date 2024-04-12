#' @name f2dbShow,f2dbTask-method
#' @noRd
methods::setMethod("f2dbShow", "f2dbTask", function(object) {
  x <- c(name = paste0("<", class(object)[1], "> ", name(object)))
  x <- c(x, taskFunction = name(taskFunction(object)))
  c(x, nextTask = name(nextTask(object)))
})
