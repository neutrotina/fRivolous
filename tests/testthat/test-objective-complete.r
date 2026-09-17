testthat::test_that(
  "objective_complete returns session information without writing a log",
  {

    output <- testthat::capture_output(
      result <- objective_complete(
        show_session_info = FALSE
      )
    )

    testthat::expect_s3_class(
      result$timestamp,
      "POSIXct"
    )

    testthat::expect_null(
      result$log_file
    )

    testthat::expect_type(
      result$session_info,
      "character"
    )

    testthat::expect_true(
      length(result$session_info) > 0L
    )

    testthat::expect_match(
      output,
      "OBJECTIVE COMPLETE: ANALYSIS COMPLETE"
    )

    testthat::expect_match(
      output,
      "SESSION LOG RETAINED IN MEMORY ONLY"
    )

    testthat::expect_false(
      any(grepl(
        "^R version",
        output
      ))
    )
  }
)


testthat::test_that(
  "objective_complete writes a session log and creates parent directories",
  {

    log_path <- file.path(
      tempdir(),
      "objective_complete_test",
      "logs",
      "session_info.txt"
    )

    output <- testthat::capture_output(
      result <- objective_complete(
        log_file = log_path,
        show_session_info = FALSE
      )
    )

    expanded_log_path <- path.expand(log_path)

    testthat::expect_true(
      file.exists(expanded_log_path)
    )

    testthat::expect_identical(
      result$log_file,
      expanded_log_path
    )

    log_text <- readLines(
      expanded_log_path
    )

    testthat::expect_true(
      any(log_text == "R session log")
    )

    testthat::expect_true(
      any(grepl(
        "^Generated:",
        log_text
      ))
    )

    testthat::expect_true(
      any(grepl(
        "^R version",
        log_text
      ))
    )

    testthat::expect_match(
      output,
      "SESSION LOG SECURED AT"
    )
  }
)


testthat::test_that(
  "objective_complete prints session information when requested",
  {

    output <- testthat::capture_output(
      objective_complete(
        show_session_info = TRUE
      )
    )

    testthat::expect_match(
      output,
      "R version"
    )

    testthat::expect_match(
      output,
      "SESSION COMPLETE"
    )
  }
)
