.onLoad <- function(libname, pkgname) {
  ns <- topenv()

  # Setup default logger
  ns$f2dbLogger <- NULL
  setf2dbLogger(log4r::logger("INFO", appenders = log4r::file_appender("f2db.log")))
}
