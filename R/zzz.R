.onLoad <- function(libname, pkgname) {
  # Setup default logger
  assign("the", rlang::new_environment(), pos = asNamespace("file2db"))
  setf2dbLogger(log4r::logger("INFO", appenders = log4r::file_appender("f2db.log")))
}
