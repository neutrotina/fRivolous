#' Transfer a file by copying or moving it
#'
#' @param source_file Existing source file.
#' @param target_dir Directory receiving the file. Created if necessary.
#' @param destination_name Destination filename. By default, the source
#'   basename is retained.
#' @param operation Either `"copy"` or `"move"`.
#' @param overwrite Logical; should an existing destination file be replaced?
#'
#' @return Invisibly returns a list containing the operation, source path,
#'   destination path, file size and whether an existing destination was
#'   overwritten.
#'
#' @keywords internal
.transfer_file <- function(
    source_file,
    target_dir,
    destination_name = basename(source_file),
    operation = c("copy", "move"),
    overwrite = FALSE
) {

  operation <- match.arg(operation)

  if (!is.character(source_file) ||
      length(source_file) != 1L ||
      is.na(source_file) ||
      !nzchar(trimws(source_file))) {
    stop(
      "'source_file' must be a single existing file path.",
      call. = FALSE
    )
  }

  if (!is.character(target_dir) ||
      length(target_dir) != 1L ||
      is.na(target_dir) ||
      !nzchar(trimws(target_dir))) {
    stop(
      "'target_dir' must be a single valid directory path.",
      call. = FALSE
    )
  }

  if (!is.character(destination_name) ||
      length(destination_name) != 1L ||
      is.na(destination_name) ||
      !nzchar(trimws(destination_name))) {
    stop(
      "'destination_name' must be a single non-empty filename.",
      call. = FALSE
    )
  }

  if (!is.logical(overwrite) ||
      length(overwrite) != 1L ||
      is.na(overwrite)) {
    stop(
      "'overwrite' must be one non-missing logical value.",
      call. = FALSE
    )
  }

  source_file <- path.expand(
    trimws(source_file)
  )

  target_dir <- path.expand(
    trimws(target_dir)
  )

  destination_name <- trimws(
    destination_name
  )

  if (!file.exists(source_file)) {
    stop(
      "Source file does not exist:\n",
      source_file,
      call. = FALSE
    )
  }

  if (dir.exists(source_file)) {
    stop(
      "Source path is a directory, not a file:\n",
      source_file,
      call. = FALSE
    )
  }

  if (destination_name %in% c(".", "..") ||
      grepl("[/\\\\]", destination_name)) {
    stop(
      "'destination_name' must contain a filename only, not a directory path.",
      call. = FALSE
    )
  }

  if (!dir.exists(target_dir)) {
    created <- dir.create(
      target_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (!created && !dir.exists(target_dir)) {
      stop(
        "Could not create target directory:\n",
        target_dir,
        call. = FALSE
      )
    }
  }

  destination_file <- file.path(
    target_dir,
    destination_name
  )

  source_normalised <- normalizePath(
    source_file,
    mustWork = TRUE
  )

  destination_normalised <- normalizePath(
    destination_file,
    mustWork = FALSE
  )

  if (identical(source_normalised, destination_normalised)) {
    stop(
      "Source and destination are the same file.",
      call. = FALSE
    )
  }

  was_existing <- file.exists(
    destination_file
  )

  if (was_existing &&
      dir.exists(destination_file)) {
    stop(
      "Destination path is a directory, not a file:\n",
      destination_file,
      call. = FALSE
    )
  }

  if (was_existing &&
      !overwrite) {
    stop(
      "Destination file already exists and 'overwrite = FALSE':\n",
      destination_file,
      call. = FALSE
    )
  }

  source_size <- file.info(
    source_file
  )$size

  cat(
    "\n[+] --- INITIATING FILE ",
    toupper(operation),
    ": TARGETED TRANSFER --- [+]\n",
    sep = ""
  )

  cat(
    "    >> SOURCE: ",
    source_file,
    "\n",
    sep = ""
  )

  cat(
    "    >> DESTINATION: ",
    destination_file,
    "\n",
    sep = ""
  )

  if (was_existing) {
    cat(
      "    >> [!] WARNING: OVERWRITING EXISTING FILE.\n"
    )
  }

  transferred <- FALSE

  if (operation == "copy") {

transferred <- file.copy(
      from = source_file,
      to = destination_file,
      overwrite = overwrite,
      copy.date = TRUE
    )

  } else {

    transferred <- file.rename(
      from = source_file,
      to = destination_file
    )

    # Fallback for cross-filesystem moves.
    if (!transferred &&
        file.exists(source_file)) {

      copied <- file.copy(
        from = source_file,
        to = destination_file,
        overwrite = overwrite,
        copy.date = TRUE
      )

      if (copied) {
        transferred <- file.remove(
          source_file
        )
      }
    }
  }

  if (!isTRUE(transferred) ||
      !file.exists(destination_file)) {
    stop(
      "File transfer failed:\n",
      source_file,
      call. = FALSE
    )
  }

  destination_size <- file.info(
    destination_file
  )$size

  if (!identical(source_size, destination_size) &&
      operation == "copy") {
    stop(
      "Copy completed but source and destination sizes differ.",
      call. = FALSE
    )
  }

  cat(
    "    >> [\u2713] FILE ",
    toupper(operation),
    " COMPLETE.\n",
    sep = ""
  )

  cat(
    "    >> DESTINATION SECURED: ",
    destination_file,
    "\n",
    sep = ""
  )

  cat(
    "----------------------------------------------------------\n\n"
  )

  invisible(
    list(
      operation = operation,
      source = source_file,
      destination = destination_file,
      size_mb = round(
        destination_size / 1024^2,
        4
      ),
      overwritten = was_existing,
      source_exists = file.exists(source_file)
    )
  )
}


#' Copy a file to a target directory
#'
#' @param source_file Existing source file.
#' @param target_dir Directory receiving the copied file. Created if necessary.
#' @param destination_name Optional destination filename. Defaults to the
#'   source basename.
#' @param overwrite Logical; should an existing destination file be replaced?
#'
#' @return Invisibly returns the result returned by `.transfer_file()`.
#'
#' @examples
#' example_source <- tempfile(
#'   fileext = ".txt"
#' )
#'
#' writeLines(
#'   "example file",
#'   example_source
#' )
#' example_target_dir <- tempfile(
#' pattern = "copy_data_file_target_"
#' )
#' dir.create(
#' example_target_dir,
#' recursive = TRUE,
#' showWarnings = FALSE
#' )
#'
#' copy_result <- copy_data_file(
#'   source_file = example_source,
#'   target_dir = example_target_dir
#' )
#'
#' @export
copy_data_file <- function(
    source_file,
    target_dir,
    destination_name = basename(source_file),
    overwrite = FALSE
) {
  .transfer_file(
    source_file = source_file,
    target_dir = target_dir,
    destination_name = destination_name,
    operation = "copy",
    overwrite = overwrite
  )
}

#' Move a file to a target directory
#'
#' @param source_file Existing source file.
#' @param target_dir Directory receiving the moved file. Created if necessary.
#' @param destination_name Optional destination filename. Defaults to the
#'   source basename.
#' @param overwrite Logical; should an existing destination file be replaced?
#'
#' @return Invisibly returns the result returned by `.transfer_file()`.
#'
#' @examples
#' example_source <- tempfile(
#'   fileext = ".txt"
#' )
#'
#' writeLines(
#'   "example file",
#'   example_source
#' )
#' example_target_dir <- tempfile(
#' pattern = "copy_data_file_target_"
#' )
#' dir.create(
#' example_target_dir,
#' recursive = TRUE,
#' showWarnings = FALSE
#' )
#' move_result <- move_data_file(
#'   source_file = example_source,
#'   target_dir = example_target_dir
#' )
#'
#' @export
move_data_file <- function(
    source_file,
    target_dir,
    destination_name = basename(source_file),
    overwrite = FALSE
) {
  .transfer_file(
    source_file = source_file,
    target_dir = target_dir,
    destination_name = destination_name,
    operation = "move",
    overwrite = overwrite
  )
}
