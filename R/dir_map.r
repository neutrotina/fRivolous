#' Create a directory tree and optionally assign directory paths
#'
#' Creates the directories described by a named nested list. Each directory
#' receives a generated variable name based on its path. Directory paths can
#' optionally be assigned to the global environment.
#'
#' @param directory_tree A named nested list describing the directory tree.
#'   Leaf directories may be represented by `NULL`.
#' @param parent Root directory under which the tree will be created.
#' @param assign_global Logical; should generated directory paths be assigned
#'   as variables in `.GlobalEnv`?
#' @param aliases A named character vector of aliases. Names may refer to a
#'   directory name or to a path relative to `parent`, such as
#'   `"results/figures"`.
#' @param print_sub_dirs Logical; should the generated directory variables be
#'   printed?
#' @param tree Logical; should the complete directory tree be printed after
#'   creation?
#' @param include_files Logical; when `tree = TRUE`, should files be included
#'   in the printed directory tree? This does not affect directory creation.
#'
#' @return Invisibly returns a list with components:
#' \describe{
#'   \item{table}{A data frame recording each created directory, relative path,
#'   absolute path, generated variable and depth.}
#'   \item{paths}{A named character vector of generated directory paths.}
#' }
#'
#' @examples
#' project_dirs <- list(
#'   data = list(
#'     raw = NULL,
#'     processed = NULL
#'   ),
#'   results = list(
#'     figures = NULL,
#'     tables = NULL
#'   )
#' )
#'
#' example_root <- file.path(
#'   tempdir(),
#'   "make_dirs_example"
#' )
#'
#' created_dirs <- make_dirs(
#'   directory_tree = project_dirs,
#'   parent = example_root,
#'   assign_global = FALSE,
#'   print_sub_dirs = FALSE,
#'   tree = TRUE,
#'   include_files = FALSE
#' )
make_dirs <- function(
    directory_tree,
    parent = getwd(),
    assign_global = TRUE,
    aliases = character(),
    print_sub_dirs = TRUE,
    tree = FALSE,
    show_files = TRUE
) {


  if (!is.list(directory_tree)) {
    stop("directory_tree must be a named list.")
  }

if (
  is.null(names(directory_tree)) ||
    any(names(directory_tree) == "")
) {
  stop(
    "Every directory must have a name."
  )
}

  if (length(aliases) > 0L) {

if (
  is.null(names(aliases)) ||
    any(names(aliases) == "")
) {
  stop(
    "aliases must be a named character vector."
  )
}


    if (!is.character(aliases)) {
      stop("aliases must be a named character vector.")
    }
  }

  parent <- normalizePath(
    parent,
    mustWork = FALSE
  )

  records <- data.frame(
    directory = character(),
    relative_path = character(),
    path = character(),
    variable = character(),
    depth = integer(),
    stringsAsFactors = FALSE
  )


  clean_name <- function(x) {

    x <- tolower(x)
    x <- gsub("[^A-Za-z0-9_]", "_", x)

    if (grepl("^[0-9]", x)) {
      x <- paste0("x_", x)
    }

    x
  }


  automatic_variable_part <- function(directory_name) {

    letters_only <- gsub(
      "[^A-Za-z]",
      "",
      directory_name
    )

    if (!nzchar(letters_only)) {
      stop(
        "Directory name contains no usable letters: ",
        directory_name
      )
    }

    tolower(
      substr(
        letters_only,
        1L,
        min(4L, nchar(letters_only))
      )
    )
  }


  variable_part <- function(
      directory_name,
      relative_parts
  ) {

    # Path-specific alias takes priority
    full_key <- paste(
      c(relative_parts, directory_name),
      collapse = "/"
    )

    if (full_key %in% names(aliases)) {
      return(
        clean_name(aliases[[full_key]])
      )
    }

    # General alias
    if (directory_name %in% names(aliases)) {
      return(
        clean_name(aliases[[directory_name]])
      )
    }

    # Otherwise use first four letters
    automatic_variable_part(directory_name)
  }

if (
  !is.logical(show_files) ||
    length(show_files) != 1L ||
    is.na(show_files)
) {
  stop("show_files must be one non-missing logical value.")
}


    walk_tree <- function(
      node,
      relative_parts = character(),
      variable_parts = character(),
      depth = 0L
  ) {

    node_names <- names(node)

    for (i in seq_along(node)) {

      directory_name <- node_names[i]

      current_relative_parts <- c(
        relative_parts,
        directory_name
      )

      current_variable_parts <- c(
        variable_parts,
        variable_part(
          directory_name = directory_name,
          relative_parts = relative_parts
        )
      )

      relative_path <- do.call(
        file.path,
        as.list(current_relative_parts)
      )

      path <- file.path(
        parent,
        relative_path
      )

      variable_name <- paste0(
        paste(
          current_variable_parts,
          collapse = ""
        ),
        ".dir"
      )

      # Collision within this directory tree
      if (variable_name %in% records$variable) {

        previous_path <- records$path[
          records$variable == variable_name
        ][1]

        stop(
          paste0(
            "Variable-name collision detected.\n",
            "Variable: ", variable_name, "\n",
            "Existing directory: ", previous_path, "\n",
            "New directory: ", path, "\n",
            "Use an alias to distinguish them."
          ),
          call. = FALSE
        )
      }

      # Check collisions with existing global variables
      if (
        assign_global &&
        exists(
          variable_name,
          envir = .GlobalEnv,
          inherits = FALSE
        )
      ) {

        existing_value <- get(
          variable_name,
          envir = .GlobalEnv,
          inherits = FALSE
        )

        if (
          is.character(existing_value) &&
          length(existing_value) == 1L
        ) {

          existing_path <- normalizePath(
            existing_value,
            mustWork = FALSE
          )

          new_path <- normalizePath(
            path,
            mustWork = FALSE
          )

          if (!identical(existing_path, new_path)) {

            stop(
              paste0(
                "Global variable collision detected.\n",
                "Variable: ", variable_name, "\n",
                "Existing value: ", existing_path, "\n",
                "New directory: ", new_path, "\n",
                "Remove the object or use a different alias."
              ),
              call. = FALSE
            )
          }

        } else {

          stop(
            paste0(
              "Global variable already exists and is not a path.\n",
              "Variable: ", variable_name, "\n",
              "Remove the object or use a different alias."
            ),
            call. = FALSE
          )
        }
      }

      # Create directory
      if (!dir.exists(path)) {

        dir.create(
          path,
          recursive = TRUE
        )

        cat(
          "Created:",
          path,
          "\n"
        )

      } else {

        cat(
          "Directory already exists:",
          path,
          "\n"
        )
      }

      # Assign global variable
      if (assign_global) {

        assign(
          variable_name,
          normalizePath(
            path,
            mustWork = FALSE
          ),
          envir = .GlobalEnv
        )

        cat(

  "Assigned variable:",
          variable_name,
          "\n"
        )
      }

      # Store record
      records <<- rbind(
        records,
        data.frame(
          directory = directory_name,
          relative_path = relative_path,
          path = normalizePath(
            path,
            mustWork = FALSE
          ),
          variable = variable_name,
          depth = depth,
          stringsAsFactors = FALSE
        )
      )

      cat("\n")

      # Recurse into child directories
      if (
        is.list(node[[i]]) &&
        length(node[[i]]) > 0L
      ) {

        walk_tree(
          node = node[[i]],
          relative_parts = current_relative_parts,
          variable_parts = current_variable_parts,
          depth = depth + 1L
        )
      }
    }

    invisible(NULL)
  }


  # Create the complete directory tree
  walk_tree(directory_tree)


  # Print generated directory variables
  if (isTRUE(print_sub_dirs)) {

    cat("\n")
    cat("OUTPUT SUBDIRECTORIES:\n\n")

    label_width <- max(
      nchar(toupper(records$directory))
    )

    for (i in seq_len(nrow(records))) {

      row <- records[i, ]

      if (row$depth == 0L) {
        prefix <- " >> [+] "
      } else {
        prefix <- paste0(
          strrep(" ", row$depth * 12L),
          " >> "
        )
      }

      label <- sprintf(
        paste0("%-", label_width, "s"),
        toupper(row$directory)
      )

      cat(
        prefix,
        label,
        " ................. ",
        row$variable,
        "\n",
        sep = ""
      )
    }
  }


  # Print complete directory tree
  if (isTRUE(tree)) {

    if (!exists("directory_map", mode = "function")) {
      stop(
        "tree = TRUE requires directory_map() ",
        "to be defined."
      )
    }

    cat("\n")
    cat("DIRECTORY TREE:\n\n")

    directory_map(parent)
  }


  paths <- records$path
  names(paths) <- records$variable

  invisible(
    list(
      table = records,
      paths = paths
    )
  )
}

#' Print a directory tree
#'
#' Prints a tree representation of a directory and its contents. Directories
#' are listed before files and entries are sorted alphabetically within those
#' groups.
#'
#' @param path Directory whose contents should be displayed.
#' @param max_depth Maximum directory depth to display. The default,
#'   `Inf`, displays the complete tree.
#' @param include_hidden Logical; should hidden files and directories be
#'   included?
#' @param include_files Logical; should files be included? If `FALSE`, only
#'   directories are shown.
#'
#' @return Invisibly returns a character vector containing the printed tree
#'   lines.
#'
#' @examples
#' example_root <- file.path(
#'   tempdir(),
#'   "directory_map_example"
#' )
#'
#' dir.create(
#'   file.path(example_root, "results"),
#'   recursive = TRUE,
#'   showWarnings = FALSE
#' )
#'
#' writeLines(
#'   "example output",
#'   file.path(example_root, "results", "output.txt")
#' )
#'
#' directory_map(
#'   path = example_root,
#'   include_files = FALSE
#' )
directory_map <- function(
    path = ".",
    max_depth = Inf,
    include_hidden = FALSE,
    show_files = TRUE
) {

  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }

if (
  !is.logical(show_files) ||
    length(show_files) != 1L ||
    is.na(show_files)
) {
  stop("show_files must be one non-missing logical value.")
}

  path <- normalizePath(
    path,
    mustWork = TRUE
  )

  root_name <- basename(path)

  if (identical(root_name, "")) {
    root_name <- path
  }

  output <- paste0(
    root_name,
    "/"
  )


  walk_directory <- function(
      current_path,
      prefix = "",
      depth = 1L
  ) {

    entries <- list.files(
      path = current_path,
      all.files = include_hidden,
      full.names = TRUE,
      no.. = TRUE
    )

    if (!include_hidden) {
      entries <- entries[
        !grepl(
          "(^|/|\\\\)\\.[^/\\\\]+$",
          entries
        )
      ]
    }

if (!isTRUE(show_files)) {

  entry_info <- file.info(entries)

  entries <- entries[
    !is.na(entry_info$isdir) &
      entry_info$isdir
  ]
}

    if (length(entries) == 0L) {
      return(invisible(NULL))
    }

    entry_info <- file.info(entries)

    # Directories first, then files; alphabetical within each group
    entry_order <- order(
      !entry_info$isdir,
      tolower(basename(entries))
    )

    entries <- entries[entry_order]

    for (i in seq_along(entries)) {

      entry <- entries[i]
      entry_name <- basename(entry)
      entry_is_directory <- isTRUE(
        file.info(entry)$isdir
      )

      is_last <- i == length(entries)

      branch <- if (is_last) {
        "└── "
      } else {
        "├── "
      }

      suffix <- if (entry_is_directory) {
        "/"
      } else {
        ""
      }

      output <<- c(
        output,
        paste0(
          prefix,
          branch,
          entry_name,
          suffix
        )
      )

      if (
        entry_is_directory &&
        depth < max_depth
      ) {

        child_prefix <- if (is_last) {
          paste0(prefix, "    ")
        } else {
          paste0(prefix, "│   ")
        }

        walk_directory(
          current_path = entry,
          prefix = child_prefix,
          depth = depth + 1L
        )
      }
    }

    invisible(NULL)
  }


  walk_directory(path)

  cat(
    paste0(
      output,
      collapse = "\n"
    ),
    "\n",
    sep = ""
  )

  invisible(output)
}



# USAGE

# add directories to a list

#project_dirs <- list(
  #VITREOUS = list(
  #  PCA = NULL,
 #   Diagnostics = NULL,
 #   Residuals = NULL,
#  Comorbidity = NULL,
  #  GO = NULL
 # ),

 # SERUM = list(
 #   PCA = NULL,
 #   Diagnostics = NULL,
  #  Residuals = NULL,
  #  Comorbidity = NULL,
 #   GO = NULL
#  )
#)


## RUN THE FUNCTION

#created_dirs <- make_dirs(
#  directory_tree = project_dirs,
#  parent = getwd(),
#  print_sub_dirs = TRUE,
 # tree = TRUE
#)
