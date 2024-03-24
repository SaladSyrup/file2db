.onLoad <- function(libname, pkgname) {
  ns <- topenv()

  # Setup default loggers
  ns$f2dbLogger <- NULL
  setf2dbLogger(log4r::logger("INFO", appenders = log4r::file_appender("f2db.log")))

  ns$taskLogger <- NULL
  setTaskLogger(log4r::logger("INFO", appenders = log4r::file_appender("task.log")))
}
