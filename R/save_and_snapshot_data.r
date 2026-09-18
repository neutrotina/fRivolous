#' Save a data object to an RDS file
#'
#' Saves an object to an RDS file, creating missing parent directories when
#' necessary. Existing files are protected unless overwriting is explicitly
#' requested.
#'
#' @param df Object to save.
#' @param filename Destination RDS file path.
#' @param label Optional label used in progress messages.
#' @param overwrite Logical; should an existing file be replaced?
#' @param compress Logical; should the file be compressed
#'
#' @return Invisibly returns a list containing:
#' \describe{
#'   \item{path}{Expanded destination path.}
#'   \item{overwritten}{Logical; whether an existing file was replaced.}
#'   \item{size_mb}{Saved file size in megabytes.}
#' }
#'
#' @section Side effects:
#' Missing parent directories may be created. If `overwrite = TRUE`, an
#' existing destination file is replaced.
#'
#' @examples
#' example_file <- file.path(
#'   tempdir(),
#'   "save_data_file_example.rds"
#' )
#'
#' result <- save_data_file(
#'   df = data.frame(value = 1:3),
#'   filename = example_file,
#'   overwrite = TRUE
#' )
#'
#' @export
save_data_file <- function(
    df,
    filename,
    label = "Final Results",
    compress = TRUE,
    overwrite = TRUE
) {
  if (!is.character(filename) ||
      length(filename) != 1L ||
      is.na(filename) ||
      !nzchar(filename)) {
    stop(
      "'filename' must be a single valid file path.",
      call. = FALSE
    )
  }

  filename <- path.expand(filename)
  parent_dir <- dirname(filename)
was_existing <- file.exists(filename)


if (was_existing &&
    dir.exists(filename)) {
  stop(
    "'filename' points to a directory, not a file:\n",
    filename,
    call. = FALSE
  )
}

if (was_existing &&
    !overwrite) {

    stop(
      "File already exists and 'overwrite = FALSE':\n",
      filename,
      call. = FALSE
    )
  }

if (was_existing) {
  cat(
    "    >> [!] WARNING: OVERWRITING EXISTING FILE.\n"
  )
}


  if (!dir.exists(parent_dir)) {
    dir.create(
      parent_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  cat(
    "\n[+] --- INITIATING DATA STORAGE: ",
    label,
    " --- [+]\n",
    sep = ""
  )

  cat(
    "    >> Preparing data for stasis at: ",
    filename,
    "\n",
    sep = ""
  )

  if (file.exists(filename)) {
    cat(
      "    >> [!] WARNING: OVERWRITING EXISTING FILE.\n"
    )
  }

  saveRDS(
    df,
    file = filename,
    compress = compress
  )

  if (!file.exists(filename)) {
    stop(
      "The file was not found after saving:\n",
      filename,
      call. = FALSE
    )
  }

  file_size_mb <- round(
    file.info(filename)$size / 1024^2,
    4
  )

  cat(
    "    >> [\u2713] FILE SAVING COMPLETE.\n"
  )

  cat(
    "    >> ARCHIVE WEIGHT: ",
    file_size_mb,
    " MB\n",
    sep = ""
  )

  cat(
    "    >> STATUS: Data secure.\n"
  )

  cat(
    "----------------------------------------------------------\n\n"
  )

  invisible(
    list(
      path = filename,
      size_mb = file_size_mb,
    overwritten = was_existing
    )
  )
}

#' Save a timestamped data snapshot
#'
#' Saves an object as an RDS file using a sanitised object name, timestamp and
#' version suffix. Missing snapshot directories are created automatically.
#'
#' @param df Object to snapshot.
#' @param df_name Character name used to construct the snapshot filename.
#' @param snapshot_dir Directory in which the snapshot should be saved.
#' @return Invisibly returns a list containing the snapshot path, overwrite
#' status and saved file size.
#'
#' @section Side effects:
#' The snapshot directory may be created and an RDS file is written.
#'
#' @examples
#' snapshot_result <- data_snapshot(
#'   df = data.frame(
#'     sample = c("A", "B"),
#'     value = c(10, 20)
#'   ),
#'   df_name = "example_data",
#'   snapshot_dir = tempdir()
#' )
#'
#' @export
data_snapshot <- function(
    df,
    df_name = deparse(substitute(df)),
    snapshot_dir = "snapshots"
) {
  if (!is.character(df_name) ||
      length(df_name) != 1L ||
      is.na(df_name) ||
      !nzchar(df_name)) {
    stop(
      "'df_name' must be a single non-empty character value.",
      call. = FALSE
    )
  }

  snapshot_dir <- path.expand(
    snapshot_dir
  )

  if (!dir.exists(snapshot_dir)) {
    dir.create(
      snapshot_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    cat(
      "\n[!] SYSTEM: CREATING SNAPSHOT REPOSITORY AT ",
      snapshot_dir,
      "\n",
      sep = ""
    )
  }

  safe_df_name <- gsub(
    "[^A-Za-z0-9_-]+",
    "_",
    df_name
  )

  timestamp <- format(
    Sys.time(),
    "%Y%m%d_%H%M%S"
  )

  file_name <- paste0(
    safe_df_name,
    "_",
    timestamp,
    "_ver.rds"
  )

  safe_name <- gsub(
  "[^A-Za-z0-9_-]+",
  "_",
  trimws(df_name)
)

safe_name <- gsub(
  "^_+|_+$",
  "",
  safe_name
)

if (!nzchar(safe_name)) {
  stop(
    "'df_name' does not produce a usable filename.",
    call. = FALSE
  )
}


  full_path <- file.path(
    snapshot_dir,
    file_name
  )
if (file.exists(full_path)) {

  suffix <- 1L

  repeat {

    candidate_path <- file.path(
      snapshot_dir,
      paste0(
        safe_df_name,
        "_",
        timestamp,
        "_",
        suffix,
        "_ver.rds"
      )
    )

    if (!file.exists(candidate_path)) {
      full_path <- candidate_path
      break
    }

    suffix <- suffix + 1L
  }
}
if (!nzchar(safe_df_name)) {
  stop(
    "'df_name' contains no usable filename characters.",
    call. = FALSE
  )
}

  cat(
    "\n[+] --- INITIATING DATA SNAPSHOT --- [+]\n"
  )

  cat(
    "    >> TARGET OBJECT : ",
    df_name,
    "\n",
    sep = ""
  )

  cat(
    "    >> VERSION TAG   : _ver\n"
  )

  cat(
    "    >> EXPORT PATH   : ",
    full_path,
    "\n",
    sep = ""
  )

  result <- save_data_file(
    df = df,
    filename = full_path,
    label = paste(
      "DATA SNAPSHOT:",
      df_name
    ),
    compress = TRUE,
    overwrite = FALSE
  )

  cat(
    "    >> TIMESTAMP     : ",
    format(
      Sys.time(),
      "%H:%M:%S | %d %b %Y"
    ),
    "\n",
    sep = ""
  )

  invisible(
    result
  )
}
