#' Source custom R scripts into the global environment
#'
#' Sources a collection of R scripts into `.GlobalEnv` and reports which files
#' loaded successfully, were missing, or failed during sourcing.
#'
#' Script paths are trimmed, expanded with [path.expand()] and deduplicated
#' before loading.
#'
#' @param script_files A non-empty character vector of paths to R scripts.
#'
#' @return Invisibly returns a list with components:
#' \describe{
#'   \item{loaded}{Character vector of scripts sourced successfully.}
#'   \item{missing}{Character vector of scripts that did not exist.}
#'   \item{failed}{Character vector of scripts that existed but produced an
#'   error during sourcing.}
#' }
#'
#' @section Side effects:
#' Existing scripts are sourced into `.GlobalEnv`. Scripts may therefore create,
#' modify or remove objects in the global environment.
#'
#' @examples
#' example_script <- tempfile(
#'   fileext = ".R"
#' )
#'
#' writeLines(
#'   "example_value <- 42",
#'   example_script
#' )
#'
#' load_result <- load_custom_scripts(
#'   script_files = example_script
#' )
#'
#' example_value
#'
#' rm(
#'   example_value,
#'   envir = .GlobalEnv
#' )
load_custom_scripts <- function(
    script_files
) {
  if (!is.character(script_files) ||
      length(script_files) == 0L ||
      anyNA(script_files)) {
    stop(
      "'script_files' must be a non-empty character vector.",
      call. = FALSE
    )
  }

  script_files <- unique(
    path.expand(
      trimws(script_files)
    )
  )

  script_files <- script_files[
    nzchar(script_files)
  ]

  if (length(script_files) == 0L) {
  stop(
    "'script_files' must contain at least one non-empty file path.",
    call. = FALSE
  )
}


  cat(
    "\n[+] --- CUSTOM SCRIPTS DETECTED: BUNDLE UPLOAD --- [+]\n"
  )

  cat(
    "    >> Initiating custom script setup...................\n"
  )

  loaded_files <- character()
  missing_files <- character()
  failed_files <- character()

  for (script_file in script_files) {
    cat(
      "    >> ACCESSING: ",
      script_file,
      "...",
      sep = ""
    )

    if (!file.exists(script_file)) {
      missing_files <- c(
        missing_files,
        script_file
      )

      cat(
        " [MISSING]\n"
      )

      cat(
        "    >> [!] Custom script not detected.\n"
      )

      next
    }

    source_result <- tryCatch(
      {
        source(
          script_file,
          local = .GlobalEnv
        )

        TRUE
      },
      error = function(error) {
        cat(
          " [FAILED]\n"
        )

        cat(
          "    >> [!] Error: ",
          conditionMessage(error),
          "\n",
          sep = ""
        )

        FALSE
      }
    )

    if (source_result) {
      loaded_files <- c(
        loaded_files,
        script_file
      )

      cat(
        " [LOADED]\n"
      )
    } else {
      failed_files <- c(
        failed_files,
        script_file
      )
    }
  }

  cat(
    "\n    >> [✔️] Custom script load complete.\n"
  )

  if (length(missing_files) > 0L) {
    cat(
      "    >> Missing files: ",
      length(missing_files),
      "\n",
      sep = ""
    )
  }

  if (length(failed_files) > 0L) {
    cat(
      "    >> Failed files: ",
      length(failed_files),
      "\n",
      sep = ""
    )
  }

  cat(
    "----------------------------------------------------------\n\n"
  )

  invisible(
    list(
      loaded = loaded_files,
      missing = missing_files,
      failed = failed_files
    )
  )
}
# USAGE
# load_custom_scripts(
#   script_files = custom_scripts
# )
