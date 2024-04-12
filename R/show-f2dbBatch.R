#' @name f2dbShow,f2dbBatch-method
#' @noRd
methods::setMethod("f2dbShow", "f2dbBatch", function(object) {
  x <- c(name = paste0("<", class(object)[1], "> ", name(object)))
  c(x, jobCount = paste0("  Num jobs: ", length(object@jobList)))
})
