#' f2dbObject class
#'
#' Base class from which all other f2db classes descend. This is a
#' virtual class.
#'
#' @export
methods::setClass("f2dbObject", contains = c("VIRTUAL"))
