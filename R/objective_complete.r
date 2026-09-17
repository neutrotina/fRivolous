#' Mark an analysis session as complete
#'
#' Records the current time and R session information, optionally writes the
#' session information to a log file, and prints a completion message.
#'
#' This function is intended as a lightweight reproducibility checkpoint at
#' the end of an analysis session.
#'
#' @param log_file Optional path to a session log file. Parent directories are
#'   created automatically if they do not exist. If `NULL`, the session
#'   information is returned but not written to disk.
#' @param show_session_info Logical; should the captured session information
#'   also be printed to the console?
#'
#' @return Invisibly returns a list with components:
#' \describe{
#'   \item{timestamp}{A `POSIXct` timestamp recording when the function ran.}
#'   \item{log_file}{The expanded log-file path, or `NULL` if no log file was
#'   requested.}
#'   \item{session_info}{A character vector containing the output of
#'   [utils::sessionInfo()].}
#' }
#'
#' @section Side effects:
#' If `log_file` is supplied, a text file containing the timestamp and session
#' information is written to disk. Progress and completion messages are printed
#' to the console.
#'
#' @examples
#' session_record <- objective_complete(
#'   show_session_info = FALSE
#' )
#'
#' log_path <- file.path(
#'   tempdir(),
#'   "analysis_logs",
#'   "session_info.txt"
#' )
#'
#' session_record <- objective_complete(
#'   log_file = log_path,
#'   show_session_info = FALSE
#' )
objective_complete <- function(
    log_file = NULL,
    show_session_info = TRUE
) {
  timestamp <- Sys.time()

  session_text <- capture.output(
    sessionInfo()
  )

  if (!is.null(log_file)) {
    log_file <- path.expand(
      log_file
    )

    log_dir <- dirname(
      log_file
    )

    if (!dir.exists(log_dir)) {
      dir.create(
        log_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }

    writeLines(
      text = c(
        "R session log",
        paste(
          "Generated:",
          format(
            timestamp,
            "%Y-%m-%d %H:%M:%S"
          )
        ),
        "",
        session_text
      ),
      con = log_file
    )
  }

  cat(
    "\n[+] --- OBJECTIVE COMPLETE: ANALYSIS COMPLETE --- [+]\n"
  )

  if (!is.null(log_file)) {
    cat(
      "    >> SESSION LOG SECURED AT: ",
      log_file,
      "\n",
      sep = ""
    )
  } else {
    cat(
      "    >> SESSION LOG RETAINED IN MEMORY ONLY\n"
    )
  }

  cat(
    "    ----------------------------------------------------------\n"
  )

  if (show_session_info) {
    cat(
      paste(
        session_text,
        collapse = "\n"
      ),
      "\n"
    )

    cat(
      "    ----------------------------------------------------------\n"
    )
  }

  cat(
    "    >> [✔️] SESSION COMPLETE .................................\n"
  )

  cat(
    "    >> ٩(^ᗜ^ )و WELL DONE OPERATOR ...........................\n"
  )

  cat(
    "    >> [!] LOGGING OUT ................................ [!]\n"
  )

  invisible(
    list(
      timestamp = timestamp,
      log_file = log_file,
      session_info = session_text
    )
  )
}

# USAGE
# objective_complete()

# or

# objective_complete(
#   log_file = here::here(
#     "logs",
#     "analysis_session_info.txt"
#   )
# )
