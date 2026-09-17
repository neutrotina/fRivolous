#' Archive an existing output file
#'
#' Checks whether a named output file exists in a target directory. If it
#' exists, the file is moved into a `FAILURE_ARCHIVE` subdirectory with a
#' timestamped filename. If no file exists, the target directory and archive
#' directory are still created and the function reports that the drive is
#' clear.
#'
#' @param target_dir Directory containing the output file. The directory and
#'   its `FAILURE_ARCHIVE` subdirectory are created if necessary.
#' @param object_file File path whose basename should be searched for in
#'   `target_dir`. Only the basename is used as the target filename.
#'
#' @return Invisibly returns a list with components:
#' \describe{
#'   \item{archived}{Logical; whether an existing file was moved to the
#'   archive.}
#'   \item{source}{Path of the expected target file in `target_dir`.}
#'   \item{archive}{Path of the archived file, or `NA_character_` if no file
#'   was present.}
#' }
#'
#' @section Side effects:
#' The target directory and `FAILURE_ARCHIVE` subdirectory may be created.
#' Existing target files are moved into the archive directory and are not
#' overwritten.
#'
#' @examples
#' example_dir <- file.path(
#'   tempdir(),
#'   "clean_log_drive_example"
#' )
#'
#' example_file <- file.path(
#'   example_dir,
#'   "analysis_output.rds"
#' )
#'
#' dir.create(
#'   example_dir,
#'   recursive = TRUE,
#'   showWarnings = FALSE
#' )
#'
#' writeLines(
#'   "previous analysis output",
#'   example_file
#' )
#'
#' archive_result <- clean_log_drive(
#'   target_dir = example_dir,
#'   object_file = example_file
#' )
clean_log_drive <- function(
    target_dir = here::here(),
    object_file
) {
  if (!is.character(target_dir) ||
      length(target_dir) != 1L ||
      is.na(target_dir) ||
      !nzchar(target_dir)) {
    stop(
      "'target_dir' must be a single valid path.",
      call. = FALSE
    )
  }

  if (!is.character(object_file) ||
      length(object_file) != 1L ||
      is.na(object_file) ||
      !nzchar(object_file)) {
    stop(
      "'object_file' must be a single file path.",
      call. = FALSE
    )
  }

  target_dir <- path.expand(target_dir)
  object_file <- path.expand(object_file)

  object_name <- basename(object_file)

  if (!nzchar(object_name) || object_name %in% c(".", "..")) {
    stop(
      "'object_file' does not contain a valid filename.",
      call. = FALSE
    )
  }

  archive_dir <- file.path(
    target_dir,
    "FAILURE_ARCHIVE"
  )

  target_path <- file.path(
    target_dir,
    object_name
  )

  cat(
    "\n[+] --- INITIATING ARCHIVING OF PAST ATTEMPTS: ",
    "TARGETED ARCHIVE --- [+]\n",
    sep = ""
  )

  cat(
    "    >> SCANNING DRIVE FOR: ",
    object_name,
    "\n",
    sep = ""
  )

  if (!dir.exists(target_dir)) {
    dir.create(
      target_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  if (!dir.exists(archive_dir)) {
    created <- dir.create(
      archive_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (!created && !dir.exists(archive_dir)) {
      stop(
        "Could not create archive directory: ",
        archive_dir,
        call. = FALSE
      )
    }
  }

  if (!file.exists(target_path)) {
    cat(
      "    >> [✔️] DRIVE CLEAR. Proceed.\n"
    )

    cat(
      "----------------------------------------------------------\n\n"
    )

    return(
      invisible(
        list(
          archived = FALSE,
          source = target_path,
          archive = NA_character_
        )
      )
    )
  }

  file_time <- file.info(
    target_path
  )$mtime

  if (is.na(file_time)) {
    stop(
      "Could not determine modification time for: ",
      target_path,
      call. = FALSE
    )
  }

  timestamp <- format(
    file_time,
    "%Y%m%d_%H%M%S"
  )

  archived_path <- file.path(
    archive_dir,
    paste0(
      timestamp,
      "_",
      object_name
    )
  )

  # Prevent overwriting an existing archive file.
  if (file.exists(archived_path)) {
    archived_path <- file.path(
      archive_dir,
      paste0(
        timestamp,
        "_",
        format(Sys.time(), "%OS3"),
        "_",
        object_name
      )
    )
  }

  cat(
    "    >> [!] PREVIOUS ATTEMPT DETECTED. ",
    "Relocating to archive...\n",
    sep = ""
  )

  moved <- file.rename(
    from = target_path,
    to = archived_path
  )

  if (!moved) {
    stop(
      "Could not archive file:\n",
      target_path,
      call. = FALSE
    )
  }

  cat(
    "    >> [✔️] DRIVE SECURED. Old intel moved to archive.\n"
  )

  cat(
    "    >> Archive location: ",
    archived_path,
    "\n",
    sep = ""
  )

  cat(
    "----------------------------------------------------------\n\n"
  )

  invisible(
    list(
      archived = TRUE,
      source = target_path,
      archive = archived_path
    )
  )
}


# USAGE
# clean_log_drive(
#  target_dir = here::here(),
#  object_file = objec_file
#)
