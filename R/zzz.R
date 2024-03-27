.onLoad <- function(libname, pkgname) {
  # Setup default logger
  setf2dbLogger(log4r::logger("INFO", appenders = log4r::file_appender("f2db.log")))
}
