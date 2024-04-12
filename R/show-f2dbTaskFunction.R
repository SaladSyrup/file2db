#' @name f2dbShow,f2dbTaskFunction-method
#' @noRd
methods::setMethod("f2dbShow", "f2dbTaskFunction", function(object) c(name = paste0("<", class(object)[1], "> ", name(object))))
