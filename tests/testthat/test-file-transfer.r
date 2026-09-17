testthat::test_that(
  "copy_data_file copies a file and retains the source",
  {

    source_file <- tempfile(
      "copy_source_",
      fileext = ".txt"
    )

    target_dir <- tempfile(
      "copy_target_"
    )

    writeLines(
      c(
        "first line",
        "second line"
      ),
      source_file
    )

    output <- testthat::capture_output(
      result <- copy_data_file(
        source_file = source_file,
        target_dir = target_dir,
        destination_name = "copied_output.txt"
      )
    )

    destination_file <- file.path(
      target_dir,
      "copied_output.txt"
    )

    testthat::expect_true(
      file.exists(source_file)
    )

    testthat::expect_true(
      file.exists(destination_file)
    )

    testthat::expect_identical(
      result$operation,
      "copy"
    )

    testthat::expect_identical(
      result$source,
      path.expand(source_file)
    )

    testthat::expect_identical(
      result$destination,
      path.expand(destination_file)
    )

    testthat::expect_false(
      result$overwritten
    )

    testthat::expect_true(
      result$source_exists
    )

    testthat::expect_identical(
      readLines(source_file),
      readLines(destination_file)
    )

    testthat::expect_match(
      output,
      "FILE COPY COMPLETE"
    )
  }
)


testthat::test_that(
  "move_data_file moves a file and removes the source",
  {

    source_file <- tempfile(
      "move_source_",
      fileext = ".rds"
    )

    target_dir <- tempfile(
      "move_target_"
    )

    example_data <- data.frame(
      value = c(1, 2, 3)
    )

    saveRDS(
      example_data,
      source_file
    )

    output <- testthat::capture_output(
      result <- move_data_file(
        source_file = source_file,
        target_dir = target_dir,
        destination_name = "moved_data.rds"
      )
    )

    destination_file <- file.path(
      target_dir,
      "moved_data.rds"
    )

    testthat::expect_false(
      file.exists(source_file)
    )

    testthat::expect_true(
      file.exists(destination_file)
    )

    testthat::expect_identical(
      result$operation,
      "move"
    )

    testthat::expect_false(
      result$source_exists
    )

    testthat::expect_equal(
      readRDS(destination_file),
      example_data
    )

    testthat::expect_match(
      output,
      "FILE MOVE COMPLETE"
    )
  }
)


testthat::test_that(
  "file transfer retains the source basename by default",
  {

    source_file <- tempfile(
      "basename_source_",
      fileext = ".csv"
    )

    target_dir <- tempfile(
      "basename_target_"
    )

    writeLines(
      "example",
      source_file
    )

    result <- testthat::capture_output(
      copy_result <- copy_data_file(
        source_file = source_file,
        target_dir = target_dir
      )
    )

    testthat::expect_identical(
      basename(copy_result$destination),
      basename(source_file)
    )

    testthat::expect_true(
      file.exists(copy_result$destination)
    )
  }
)


testthat::test_that(
  "file transfer refuses to overwrite by default",
  {

    source_file <- tempfile(
      "overwrite_source_",
      fileext = ".txt"
    )

    target_dir <- tempfile(
      "overwrite_target_"
    )

    destination_file <- file.path(
      target_dir,
      "result.txt"
    )

    dir.create(
      target_dir,
      recursive = TRUE
    )

    writeLines(
      "new content",
      source_file
    )

    writeLines(
      "old content",
      destination_file
    )

    testthat::expect_error(
      copy_data_file(
        source_file = source_file,
        target_dir = target_dir,
        destination_name = "result.txt",
        overwrite = FALSE
      ),
      "overwrite = FALSE"
    )

    testthat::expect_identical(
      readLines(destination_file),
      "old content"
    )

    testthat::capture_output(
      result <- copy_data_file(
        source_file = source_file,
        target_dir = target_dir,
        destination_name = "result.txt",
