testthat::test_that(
  "load_custom_scripts loads valid scripts and reports failures",
  {

    good_script <- tempfile(
      fileext = ".R"
    )

    failed_script <- tempfile(
      fileext = ".R"
    )

    missing_script <- tempfile(
      fileext = ".R"
    )

    marker_name <- paste0(
      "load_custom_scripts_test_",
      as.integer(Sys.getpid())
    )

    on.exit(
      rm(
        list = marker_name,
        envir = .GlobalEnv
      ),
      add = TRUE
    )

    writeLines(
      paste0(
        marker_name,
        " <- TRUE"
      ),
      good_script
    )

    writeLines(
      "stop('intentional test failure')",
      failed_script
    )

    output <- testthat::capture_output(
      result <- load_custom_scripts(
        script_files = c(
          paste0(" ", good_script, " "),
          good_script,
          missing_script,
          failed_script
        )
      )
    )

    expanded_good_script <- path.expand(
      good_script
    )

    expanded_missing_script <- path.expand(
      missing_script
    )

    expanded_failed_script <- path.expand(
      failed_script
    )

    testthat::expect_identical(
      result$loaded,
      expanded_good_script
    )

    testthat::expect_identical(
      result$missing,
      expanded_missing_script
    )

    testthat::expect_identical(
      result$failed,
      expanded_failed_script
    )

    testthat::expect_true(
      exists(
        marker_name,
        envir = .GlobalEnv,
        inherits = FALSE
      )
    )

    testthat::expect_true(
      get(
        marker_name,
        envir = .GlobalEnv,
        inherits = FALSE
      )
    )

    testthat::expect_match(
      output,
      "Custom script load complete"
    )

    testthat::expect_match(
      output,
      "Missing files: 1"
    )

    testthat::expect_match(
      output,
      "Failed files: 1"
    )
  }
)


testthat::test_that(
  "load_custom_scripts rejects invalid input",
  {

    testthat::expect_error(
      load_custom_scripts(NULL),
      "non-empty character vector"
    )

    testthat::expect_error(
      load_custom_scripts(character()),
      "non-empty character vector"
    )

    testthat::expect_error(
      load_custom_scripts(42),
      "non-empty character vector"
    )

    testthat::expect_error(
      load_custom_scripts(
        c("example.R", NA_character_)
      ),
      "non-empty character vector"
    )

    testthat::expect_error(
      load_custom_scripts("   "),
      "non-empty file path"
    )
  }
)
