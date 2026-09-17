#' Install and load a list of R packages
#'
#' Checks whether the requested packages are installed, installs any missing
#' packages from the configured package repositories, and then loads each
#' package without displaying its startup messages.
#'
#' Package names are trimmed of leading and trailing whitespace and duplicate
#' package names are removed before checking or loading.
#'
#' @param lib_list A non-empty character vector of package names.
#'
#' @return Invisibly returns a character vector containing the unique,
#'   non-empty package names that were checked and loaded.
#'
#' @section Side effects:
#' Missing packages are installed using [utils::install.packages()] with
#' `dependencies = TRUE`. Packages are then attached to the search path using
#' [base::library()].
#'
#' @examples
#' packages <- c(
#'   "stats",
#'   "utils",
#'   "stats"
#' )
#'
#' loaded_packages <- load_library_list(
#'   packages
#' )
#'
#' loaded_packages
load_library_list <- function(
    lib_list
) {
  if (!is.character(lib_list) ||
      length(lib_list) == 0L ||
      anyNA(lib_list)) {
    stop(
      "'lib_list' must be a non-empty character vector.",
      call. = FALSE
    )
  }

  lib_list <- unique(
    trimws(lib_list)
  )

  lib_list <- lib_list[
    nzchar(lib_list)
  ]

if (length(lib_list) == 0L) {
  stop(
    "'lib_list' must contain at least one non-empty package name.",
    call. = FALSE
  )
}

  cat(
    "    > Adding installed packages.......\n"
  )

  cat(
    "    >> Checking for missing packages...\n"
  )

  installed_packages <- rownames(
    installed.packages()
  )

  install_if_missing <- setdiff(
    lib_list,
    installed_packages
  )

  if (length(install_if_missing) > 0L) {
    cat(
      "    >> [!] MISSING PACKAGES DETECTED. ",
      "Requisitioning from CRAN...\n",
      sep = ""
    )

    install.packages(
      install_if_missing,
      dependencies = TRUE,
      quiet = TRUE
    )
  } else {
    cat(
      "    >> No missing packages detected.\n"
    )
  }

  cat(
    "    >> Loading packages...\n"
  )

  for (pkg in lib_list) {
    suppressPackageStartupMessages(
      library(
        pkg,
        character.only = TRUE
      )
    )

    cat(
      "      [+] LOADING: ",
      pkg,
      " ... [✔️]\n",
      sep = ""
    )
  }

  cat(
    "    >>> [✔️] Loading complete [✔️]\n"
  )

  cat(
    "----------------------------------------------------------\n\n"
  )

  invisible(
    lib_list
  )
}

# USAGE
# lib_list <- c("tidyverse", "here")
# load_library_list(
#  lib_list
# )
