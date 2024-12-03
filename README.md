# file2db

A tool for automating the process of batch loading data from a large number of files into a database. The process is fully configurable and allows for processing/cleaning data at any point. The user can provide alternate file and database functions to allow file2db to work with any file format or database.

file2db generates a log to document tasks performed and capture errors.

While originally written for loading data into a database, file2db can be used for any data processing job. See examples below.

Requires packages `methods` and `log4r`.

## Description

Work is organized as a collection of jobs, where each job defines a sequence of tasks used to operate on the data.

![f2db](https://github.com/user-attachments/assets/d35f12bc-6a6c-4b7e-9c87-377c8e9617b7)

An `f2dbBatch` is a collection of related `f2dbJob`s. Jobs are executed independently of each other; the output of one job is not passed to the next job.

Each `f2dbJob` contains a sequence of `f2dbTask`s to process and move data from source to destination (e.g. file to database). A job should normally process data from a single source type (such as a collection of identically formatted files) and place it in a single, final location (database table, file, etc.).

An `f2dbTask` contains a user-defined `f2dbTaskFunction` to operate on the data. The output of an `f2dbTask` is passed as input to the following `f2dbTask`.

Three special tasks are provided:

* `f2dbEndTask` marks the end of a task sequence. `f2dbEndTask` is transparent during use and is normally only used to shorten an existing task list.
* `f2dbIterateTask` iterates over its `taskInput`, running subsequent `taskFunction`s with each element as input. Used to process lists of files.
* `f2dbListTask` is an `f2dbTask` with its `nextTask` set to an `f2dbIterateTask`. Used to combine a task that creates a list with a task that iterates through the list.

## Data Processing Example

This example was used to extract and process solar irradiation values from a collection of files containing hourly weather records across 25 years. The work was accomplished in two parts: (1) extracting solar irradiation, date, and time from multiple files and placing it in a single intermediate file; and (2) grouping and averaging the solar irradiation by day and time.

```R
# Create and name a new f2dbBatch. Names are used in the logs to identify
# specific batches, jobs, tasks, etc.
clean_ghi_data <- f2dbBatch("Clean and save GHI data")

# Create the first job to extract data and combine it in a single file.
# "*.csv" is passed as input to the first task in the job.
ghi_job <- f2dbJob("Combine GHI data", "*.csv")

# First task is to create a list of all csv files in the ~/HVAC/GHI
# directory. This task will call
#
#   list.files(pattern = taskInput, path = "~/HVAC/GHI", full.names = TRUE)
taskList(ghi_job) <- f2dbTask("List files", list.files, path = "~/HVAC/GHI",
                              full.names = TRUE, inputName = "pattern")

# Iterate through the list of files created by the previous task. Explicitly
# creating a f2dbIterateTask can be avoided by using f2dbListTask.
taskList(ghi_job) <- f2dbIterateTask("Iterate through files")

# Read the file and select the columns we're interested in. This task will call
#
#   readr::read_csv(taskInput, col_names = TRUE, col_select = c(2, 3, 4, 15), col_type = c("i"), skip = 2)
#
# where taskInput is the name of the file passed in by the previous task.
taskList(ghi_job) <- f2dbTask("Read file", readr::read_csv,
                              col_names = TRUE,
                              col_select = c(2, 3, 4, 15),
                              col_type = c("i"),
                              skip = 2)

taskList(ghi_job) <- f2dbTask("Add julian date", dplyr::mutate,
                              JULIAN = (((dplyr::row_number() - 1) %/% 24) + 1),
                              .before = 1)

taskList(ghi_job) <- f2dbTask("Uppercase column names", dplyr::rename_with, stringr::str_to_upper)

taskList(ghi_job) <- f2dbTask("Write file", readr::write_csv, file = "~/HVAC/ghi.csv", append = TRUE)

# Add first job the batch
jobList(clean_ghi_data) <- ghi_job

# And move on the next part
ghi_job <-  f2dbJob("Calculate hourly GHI averages", "~/HVAC/ghi.csv")

# Read the intermediate file created by the previous task
taskList(ghi_job) <- f2dbTask("Read file", readr::read_csv,
                              col_names = c("JULIAN", "MONTH", "DAY", "HOUR", "GHI"),
                              col_type = c("n"))

# Use an anonymous function for more complicated tasks
taskList(ghi_job) <- f2dbTask("Calculate hourly averages", function(x) {
  x <- dplyr::group_by(x, JULIAN, MONTH, DAY, HOUR)
  dplyr::summarise(x, HRLY_AVG_GHI = mean(GHI), .groups = "drop")
})

taskList(ghi_job) <- f2dbTask("Save to file", readr::write_csv, file = "~/HVAC/hourly_ghi.csv")

# input is the output of the previous task.
# item refers to the job item being processed. This is normally the input
# provided by the job to first task, but it can be changed (by f2dbIterateTask
# for example). This task is only interested in the item, so an anonymous
# function is needed to get the item (parameter y) while ignoring the input
# (parameter x).
taskList(ghi_job) <- f2dbTask("Delete intermediate file", function(x, y) unlink(y), inputName = "x", itemName = "y")

jobList(clean_ghi_data) <- ghi_job

rm(ghi_job)
f2dbRun(clean_ghi_data)
```

## Retrosheet Example

I originally wrote file2db to load Retrosheet historical baseball data into a database for easier access. The data is spread across thousands of files. The majority of these are csv files that require minimal massaging; the biggest need is to capture the occasional parsing error for later review. However, some of the Retrosheet data is contained in an alternate format and requires an external program to place the data in an accessible format.

```R
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
load_retrosheet <- f2dbBatch("Load retrosheet to database")

# gamelogs

# A regular expression to capture all the files.
job <- f2dbJob("gamelogs", "^\\d{4}(?:[A-Z]{3}|[A-Z]{2}\\d)?\\.EV[ANF]$")

# f2dbListTask creates an f2dbIterateTask to iterate through the file list.
taskList(job) <- f2dbListTask("List files", list.files,
                              path = "~/retrosheet/events",
                              full.names = TRUE,
                              inputName = "pattern")

# The program cwgame is used to extract data from a binary game file and place
# it in a csv file.
taskList(job) <- f2dbTask("cwgame",
    function(filename) {
      # cwgame expects to find team files in the directory it's called from
      current_wd <- getwd()
      on.exit(setwd(current_wd), add = TRUE)
      setwd("~/retrosheet/teams")

      year <- stringr::str_extract(filename, "(\\d{4})(?:[A-Z]{3}|[A-Z]{2}\\d)?\\.EV[ANF]$", group = 1)
      game_file <- paste(filename, ".csv", sep = "")
      error_file <- paste(filename, ".error", sep = "")

      # Use error_file to capture any errors for logging
      ret_val <- system2("cwgame", args = c("-q", "-y", year, filename), stdout = game_file, stderr = error_file)
      if (ret_val != 0) {

        # f2dbRun(f2dbTaskFunction) will catch warnings and errors for logging
        rlang::warn("cwgame returned non-zero exit value: ", ret_val)

        error_msgs <- readLines(file(error_file, open = "r"))
        if (length(error_msgs) == 0) {
          rlang::warn("cwgame generated no warning messages")
        } else {
          rlang::warn("cwgame generated the following error messages:")
          lapply(error_msgs, function(x) rlang::warn(x))
        }

        unlink(c(game_file, error_file))
        rlang::abort(paste("Error creating gamelog from ", filename))
        return(NULL)
      }

      unlink(error_file)
      return(game_file)
    }
  )

taskList(job) <- f2dbTask("Read file", readr::read_csv,
                          col_names = column_names,
                          name_repair = "minimal",
                          col_types = column_types,
                          show_col_types = FALSE)

taskList(job) <- f2dbTask("Write", DBI::dbWriteTable,
                          conn = con,
                          name = "gamelogs",
                          append = TRUE,
                          inputName = "value")

taskList(job) <- f2dbTask("Delete game file",
  function(data, filename) {
    unlink(paste(filename, ".csv", sep = ""))
    return(data)
  },
  inputName = "data",
  itemName = "filename")

jobList(load_retrosheet) <- job
f2dbRun(load_retrosheet)
```
