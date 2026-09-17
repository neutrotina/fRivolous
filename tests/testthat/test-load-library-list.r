testthat::test_that(
  "load_library_list trims, deduplicates and loads installed packages",
  {

    output <- testthat::capture_output(
      loaded_packages <- load_library_list(
        c(
          " stats ",
          "utils",
          "stats"
        )
      )
    )

    testthat::expect_identical(
      loaded_packages,
      c("stats", "utils")
    )

    testthat::expect_true(
      "package:stats" %in% search()
    )

    testthat::expect_true(
      "package:utils" %in% search()
    )

    testthat::expect_match(
      output,
      "Loading complete"
    )
  }
)


testthat::test_that(
  "load_library_list rejects invalid input",
  {

    testthat::expect_error(
      load_library_list(NULL),
      "non-empty character vector"
    )

    testthat::expect_error(
      load_library_list(character()),
      "non-empty character vector"
    )

    testthat::expect_error(
      load_library_list(42),
      "non-empty character vector"
    )

    testthat::expect_error(
      load_library_list(c("stats", NA_character_)),
      "non-empty character vector"
    )

    testthat::expect_error(
      load_library_list("   "),
      "non-empty package name"
    )
  }
)
