#' file2dbTaskObject class
#'
#' Base class from which all other file2db tasks should descend. This is a
#' virtual class.
#'
#' @export
methods::setClass("file2dbTaskObject", contains = c("VIRTUAL"))
