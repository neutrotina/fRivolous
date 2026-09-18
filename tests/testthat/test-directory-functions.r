testthat::test_that(
  "make_dirs creates the requested directory tree",
  {

   parent <- tempfile("make_dirs_test_")


    project_dirs <- list(
      data = list(
        raw = NULL,
        processed = NULL
      ),
      results = list(
        figures = NULL,
        tables = NULL
      )
    )

    created <- make_dirs(
      directory_tree = project_dirs,
      parent = parent,
      assign_global = FALSE,
      print_sub_dirs = FALSE,
      tree = FALSE,
      show_files = FALSE
    )

    expected_paths <- file.path(
      parent,
      c(
        "data",
        "data/raw",
        "data/processed",
        "results",
        "results/figures",
        "results/tables"
      )
    )

    testthat::expect_true(
      all(dir.exists(expected_paths))
    )

    testthat::expect_true(
      is.list(created)
    )

    testthat::expect_true(
      all(c("table", "paths") %in% names(created))
    )

    testthat::expect_equal(
      sort(unname(created$paths)),
      sort(normalizePath(
        expected_paths,
        mustWork = FALSE
      ))
    )
  }
)


testthat::test_that(
  "directory_map can exclude files",
  {

    parent <- tempfile("directory_map_test_")

    dir.create(
      file.path(parent, "results"),
      recursive = TRUE
    )

    writeLines(
      "example output",
      file.path(parent, "results", "output.txt")
    )

    folders_only <- testthat::capture_output(
      folder_tree <- directory_map(
        path = parent,
        show_files = FALSE
      )
    )

    testthat::expect_false(
      any(grepl(
        "output.txt",
        folder_tree,
        fixed = TRUE
      ))
    )

    with_files <- testthat::capture_output(
      complete_tree <- directory_map(
        path = parent,
        show_files = TRUE
      )
    )

    testthat::expect_true(
      any(grepl(
        "output.txt",
        complete_tree,
        fixed = TRUE
      ))
    )
  }
)


testthat::test_that(
  "make_dirs passes show_files to directory_map",
  {

    parent <- tempfile("make_dirs_tree_test_")

    dir.create(
      parent,
      recursive = TRUE
    )

    writeLines(
      "existing file",
      file.path(parent, "existing.txt")
    )

    output <- testthat::capture_output(
      make_dirs(
        directory_tree = list(
          results = NULL
        ),
        parent = parent,
        assign_global = FALSE,
        print_sub_dirs = FALSE,
        tree = TRUE,
        show_files = FALSE
      )
    )

    testthat::expect_false(
      any(grepl(
        "existing.txt",
        output,
        fixed = TRUE
      ))
    )
  }
)


testthat::test_that(
  "directory functions reject invalid show_files values",
  {

    testthat::expect_error(
      directory_map(
        path = tempdir(),
        show_files = c(TRUE, FALSE)
      ),
      "show_files"
    )

    testthat::expect_error(
      directory_map(
        path = tempdir(),
        show_files = NA
      ),
      "show_files"
    )

    testthat::expect_error(
      make_dirs(
        directory_tree = list(
          results = NULL
        ),
        parent = tempdir(),
        show_files = "no"
      ),
      "show_files"
    )
  }
)
