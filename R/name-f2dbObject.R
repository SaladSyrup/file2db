#' @name name,f2dbObject-method
#' @rdname name-method
#' @export
methods::setMethod("name", "f2dbObject", function(object) object@name)
