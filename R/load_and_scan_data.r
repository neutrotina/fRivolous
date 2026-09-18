#' Load multiple data files
#'
#' Imports RDS, CSV, TSV and TXT files from a named character vector or named
#' list of file paths. Missing and failed files are recorded separately rather
#' than aborting the complete import.
#'
#' @param file_list A named character vector or named list of file paths.
#'   Names are used as the names of the imported objects.
#' @param assign_to_global Logical; should successfully imported objects be
#'   assigned to `envir`?
#' @param print_dims Logical; should dimensions or element counts be printed
#'   for successfully imported objects?
#' @param print_parameters Logical; should column or element names be printed?
#' @param envir Environment in which imported objects should be assigned when
#'   `assign_to_global = TRUE`.
#'
#' @return Invisibly returns a list with components:
#' \describe{
#'   \item{data}{A named list containing successfully imported objects.}
#'   \item{missing}{Names of files that did not exist.}
#'   \item{failed}{Names of files that could not be imported.}
#' }
#'
#' @section Supported formats:
#' Supported file extensions are `.rds`, `.csv`, `.tsv` and `.txt`.
#'
#' @section Side effects:
#' If `assign_to_global = TRUE`, successfully imported objects are assigned to
#' the specified environment.
#' @export
#' @examples
#' example_data <- data.frame(
#'   sample = c("A", "B"),
#'   value = c(10, 20)
#' )
#'
#' example_file <- file.path(
#'   tempdir(),
#'   "example_data.csv"
#' )
#'
#' utils::write.csv(
#'   example_data,
#'   example_file,
#'   row.names = FALSE
#' )
#'
#' loaded <- load_data_files(
#'   file_list = c(example = example_file),
#'   assign_to_global = FALSE,
#'   print_dims = FALSE,
#'   print_parameters = FALSE
#' )
load_data_files <- function(
    file_list,
    assign_to_global = TRUE,
    print_dims = TRUE,
    print_parameters = TRUE,
    envir = .GlobalEnv
) {

  if (!is.character(file_list) &&
      !is.list(file_list)) {
    stop(
      "'file_list' must be a named character vector or named list.",
      call. = FALSE
    )
  }

  if (is.null(names(file_list)) ||
      any(!nzchar(names(file_list))) ||
      anyDuplicated(names(file_list)) > 0L) {
    stop(
      "'file_list' must have unique, non-empty names.",
      call. = FALSE
    )
  }

  cat(
    "\n[+] --- DATA FILE DOWNLOAD IN PROGRESS --- [+]\n"
  )

  loaded_data <- list()
  missing_files <- character()
  failed_files <- character()

  for (data_name in names(file_list)) {
    path <- path.expand(
      as.character(file_list[[data_name]])
    )

    cat(
      "\n[+] LOCATING DATA: ",
      data_name,
      "\n",
      sep = ""
    )

    cat(
      "    >> DATA PATH: ",
      path,
      "\n",
      sep = ""
    )

    if (!file.exists(path)) {
      missing_files <- c(
        missing_files,
        data_name
      )

      cat(
        "    >> [!] DATA MISSING: ",
        data_name,
        "\n",
        sep = ""
      )

      next
    }

    ext <- tolower(
      tools::file_ext(path)
    )

    imported_data <- tryCatch(
      {
        switch(
          ext,
          rds = readRDS(path),
          csv = utils::read.csv(
            path,
            stringsAsFactors = FALSE,
            check.names = FALSE
          ),
          tsv = utils::read.delim(
            path,
            stringsAsFactors = FALSE,
            check.names = FALSE
          ),
          txt = utils::read.delim(
            path,
            stringsAsFactors = FALSE,
            check.names = FALSE
          ),
          stop(
            "Unsupported file extension: .",
            ext
          )
        )
      },
      error = function(error) {
        failed_files <<- c(
          failed_files,
          data_name
        )

        cat(
          "    >> [!] DATA LOAD FAILED: ",
          conditionMessage(error),
          "\n",
          sep = ""
        )

        NULL
      }
    )

    if (is.null(imported_data)) {
      next
    }

    loaded_data[[data_name]] <- imported_data

    if (assign_to_global) {
      assign(
        data_name,
        imported_data,
        envir = envir
      )
    }

    data_dimensions <- dim(
      imported_data
    )

    if (!is.null(data_dimensions)) {
      data_size <- paste(
        format(
          data_dimensions[1L],
          big.mark = ","
        ),
        "rows x",
        format(
          data_dimensions[2L],
          big.mark = ","
        ),
        "columns"
      )
    } else {
      data_size <- paste(
        length(imported_data),
        "elements"
      )
    }

    data_parameters <- names(
      imported_data
    )

    cat(
      "    >> [\u2713] DATA LOAD SUCCESSFUL ",
      "(Format: ",
      toupper(ext),
      ")\n",
      sep = ""
    )

   if (
  isTRUE(print_dims)
) {
  cat(
    "    >> SIZE: ",
    data_size,
    "\n",
    sep = ""
  )
}


  if (
  isTRUE(print_parameters) &&
    !is.null(data_parameters)
) {
  cat(
    "    >> PARAMETERS:\n"
  )

  cat(
    "       | ",
    paste(
      data_parameters,
      collapse = " | "
    ),
    "\n",
    sep = ""
  )
}

  }

  cat(
    "\n    >> [\u2713] DATA LOADING COMPLETE\n"
  )

  if (length(missing_files) > 0L) {
    cat(
      "    >> Missing files: ",
      paste(
        missing_files,
        collapse = ", "
      ),
      "\n",
      sep = ""
    )
  }

  if (length(failed_files) > 0L) {
    cat(
      "    >> Failed files: ",
      paste(
failed_files,
        collapse = ", "
      ),
      "\n",
      sep = ""
    )
  }

  cat(
    "----------------------------------------------------------\n\n"
  )

  invisible(
    list(
      data = loaded_data,
      missing = missing_files,
      failed = failed_files
    )
  )
}


#' Scan a data frame for basic integrity issues
#'
#' Reports completeness, duplicated rows, column classes, missing values and
#' ranges for numeric columns.
#'
#' @param df A data frame to inspect.
#' @param object_name Character label used in the printed report. By default,
#'   the expression supplied to `df` is deparsed.
#'
#' @return Invisibly returns a list with components:
#' \describe{
#'   \item{object_name}{The label used in the report.}
#'   \item{rows}{Number of rows in the data frame.}
#'   \item{columns}{Number of columns in the data frame.}
#'   \item{missing_cells}{Total number of missing cells.}
#'   \item{completeness}{Percentage of non-missing cells, or `NA` for an empty
#'   data frame.}
#'   \item{duplicate_rows}{Number of duplicated rows.}
#' }
#' @export
#' @examples
#' example_data <- data.frame(
#'   sample = c("A", "B", "B"),
#'   value = c(10, NA, 20)
#' )
#'
#' result <- data_check(
#'   example_data,
#'   object_name = "example_data"
#' )
data_check <- function(
    df,
    object_name = deparse(substitute(df))
) {
  if (!is.data.frame(df)) {
    stop(
      "'df' must be a data frame.",
      call. = FALSE
    )
  }

  rows <- nrow(df)
  cols <- ncol(df)

  missing_cells <- sum(
    is.na(df)
  )

  total_cells <- rows * cols

  completeness <- if (total_cells == 0L) {
    NA_real_
  } else {
    round(
      (1 - missing_cells / total_cells) * 100,
      2
    )
  }

  cat(
    "\n[+] INITIATING DATA CHECK: ",
    object_name,
    " ......... \u25B7\n",
    sep = ""
  )



  if (is.na(completeness)) {
    cat(
      "    >> COMPLETENESS: Not calculable for an empty data frame\n"
    )
  } else {
    cat(
      "    >> COMPLETENESS: ",
      completeness,
      "% Data Completeness\n",
      sep = ""
    )
  }

  duplicate_count <- sum(
    duplicated(df)
  )

  duplicate_status <- if (duplicate_count == 0L) {
    "[\u2713] CLEAN"
  } else {
    paste(
      "[!] ",
      duplicate_count,
      " REDUNDANT ROWS FOUND",
      sep = ""
    )
  }

  cat(
    "\n[+] --- INTEGRITY SCAN --- [+]\n"
  )

  cat(
    "    >> DUPLICATE CHECK: ",
    duplicate_status,
    "\n",
    sep = ""
  )

  cat(
    "\n[+] --- PARAMETER BREAKDOWN --- [+]\n"
  )

  for (column_name in names(df)) {
    column <- df[[column_name]]

    column_class <- class(column)[1L]
    missing_count <- sum(
      is.na(column)
    )

    status <- if (missing_count == 0L) {
      "[\u2713]"
    } else {
      "[!]"
    }

    cat(
      sprintf(
        "    %s %-20s | TYPE: %-12s | NULLS: %-5d\n",
        status,
        column_name,
        toupper(column_class),
        missing_count
      )
    )

    if (is.numeric(column)) {
      valid_values <- column[
        !is.na(column)
      ]

      if (length(valid_values) > 0L) {
        value_range <- range(
          valid_values
        )

        cat(
          sprintf(
            "       	\u2514\u2500 RANGE: [%s to %s]\n",
            value_range[1L],
            value_range[2L]
          )
        )
      } else {
        cat(
          "       	\u2514\u2500 RANGE: Not available; all values are missing\n"
        )
      }
    }
  }

  cat(
    "\n[\u2713] DATA CHECK COMPLETE ............[\u2713]\n"
  )

  cat(
    "\n----------------------------------------------------------\n"
  )

  invisible(
    list(
      object_name = object_name,
      rows = rows,
      columns = cols,
      missing_cells = missing_cells,
      completeness = completeness,
      duplicate_rows = duplicate_count
    )
  )
}


# USAGE

# load_result <- load_data_files(
#  file_list,
#  assign_to_global = FALSE
# )
# data can be accessed through
# load_result$data$serum
