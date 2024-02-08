new_file2db_job <- function(x = list()) {

  stopifnot(is.list(x))

  structure(x, class = "file2db_job")
}

validate_file2db_job <- function(x) {

  x
}

#' @export
file2db_job <- function(){

  x <- list()

  validate(new_file2db_job(x))
}
