testthat::test_that(
  "clean_log_drive creates archive directories when no file exists",
  {

    target_dir <- tempfile(
      "clean_log_drive_clear_"
    )

    object_file <- file.path(
      target_dir,
      "analysis_output.rds"
    )

    output <- testthat::capture_output(
      result <- clean_log_drive(
        target_dir = target_dir,
        object_file = object_file
      )
    )

    testthat::expect_false(
      result$archived
    )

    testthat::expect_identical(
      result$source,
      object_file
    )

    testthat::expect_true(
      is.na(result$archive)
    )

    testthat::expect_true(
      dir.exists(target_dir)
    )

    testthat::expect_true(
      dir.exists(
        file.path(
          target_dir,
          "FAILURE_ARCHIVE"
        )
      )
    )

    testthat::expect_match(
      output,
      "DRIVE CLEAR"
    )
  }
)


testthat::test_that(
  "clean_log_drive archives an existing file",
  {

    target_dir <- tempfile(
      "clean_log_drive_archive_"
    )

    object_file <- file.path(
      target_dir,
      "analysis_output.rds"
    )

    dir.create(
      target_dir,
      recursive = TRUE
    )

    writeLines(
      "previous analysis output",
      object_file
    )

    output <- testthat::capture_output(
      result <- clean_log_drive(
        target_dir = target_dir,
        object_file = object_file
      )
    )

    testthat::expect_true(
      result$archived
    )

    testthat::expect_identical(
      result$source,
      object_file
    )

    testthat::expect_true(
      is.character(result$archive)
    )

    testthat::expect_true(
      file.exists(result$archive)
    )

    testthat::expect_false(
      file.exists(object_file)
    )

    testthat::expect_identical(
      readLines(result$archive),
      "previous analysis output"
    )

    testthat::expect_match(
      basename(result$archive),
      "analysis_output\\.rds$"
    )

    testthat::expect_match(
      output,
      "PREVIOUS ATTEMPT DETECTED"
    )

    testthat::expect_match(
      output,
      "DRIVE SECURED"
    )
  }
)


testthat::test_that(
  "clean_log_drive uses the basename of object_file",
  {

    target_dir <- tempfile(
      "clean_log_drive_basename_"
    )

    source_subdirectory <- file.path(
      tempdir(),
      "clean_log_drive_source"
    )

    object_file <- file.path(
      source_subdirectory,
      "analysis_output.rds"
    )

    target_file <- file.path(
      target_dir,
      "analysis_output.rds"
    )

    dir.create(
      target_dir,
      recursive = TRUE
    )

   dir.create(
  source_subdirectory,
  recursive = TRUE,
  showWarnings = FALSE
)


    writeLines(
      "target file",
      target_file
    )

    result <- testthat::capture_output(
      archive_result <- clean_log_drive(
        target_dir = target_dir,
        object_file = object_file
      )
    )

    testthat::expect_true(
      archive_result$archived
    )

    testthat::expect_false(
      file.exists(target_file)
    )

    testthat::expect_true(
      file.exists(archive_result$archive)
    )

    testthat::expect_match(
      archive_result$source,
      "analysis_output\\.rds$"
    )
  }
)


testthat::test_that(
  "clean_log_drive rejects invalid paths",
  {

    testthat::expect_error(
      clean_log_drive(
        target_dir = character(),
        object_file = "output.rds"
      ),
      "target_dir"
    )

    testthat::expect_error(
      clean_log_drive(
        target_dir = c("one", "two"),
        object_file = "output.rds"
      ),
      "target_dir"
    )

    testthat::expect_error(
      clean_log_drive(
        target_dir = tempdir(),
        object_file = character()
      ),
      "object_file"
    )

    testthat::expect_error(
      clean_log_drive(
        target_dir = tempdir(),
        object_file = c("one.rds", "two.rds")
      ),
      "object_file"
    )

    testthat::expect_error(
      clean_log_drive(
        target_dir = tempdir(),
        object_file = NA_character_
      ),
      "object_file"
    )
  }
)
