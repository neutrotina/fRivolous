new_test_dir <- function(
    pattern
) {
  path <- tempfile(
    pattern = pattern
  )

  dir.create(
    path,
    recursive = TRUE,
    showWarnings = FALSE
  )

  path
}


testthat::test_that(
  "save_data_file reports new and overwritten files correctly",
  {

    filename <- tempfile(
      fileext = ".rds"
    )

    first_data <- data.frame(
      value = c(1, 2, 3)
    )

    second_data <- data.frame(
      value = c(4, 5, 6)
    )

    testthat::capture_output(
      first_result <- save_data_file(
        df = first_data,
        filename = filename,
        label = "First save",
        overwrite = FALSE
      )
    )

    testthat::expect_true(
      file.exists(filename)
    )

    testthat::expect_false(
      first_result$overwritten
    )

    testthat::expect_equal(
      readRDS(filename),
      first_data
    )

    testthat::expect_error(
      save_data_file(
        df = second_data,
        filename = filename,
        overwrite = FALSE
      ),
      "overwrite = FALSE"
    )

    testthat::capture_output(
      second_result <- save_data_file(
        df = second_data,
        filename = filename,
        label = "Second save",
        overwrite = TRUE
      )
    )

    testthat::expect_true(
      second_result$overwritten
    )

    testthat::expect_equal(
      readRDS(filename),
      second_data
    )
  }
)

testthat::test_that(
  "save_data_file creates missing parent directories",
  {
    test_dir <- new_test_dir(
      pattern = "save_data_file_test_"
    )

    filename <- file.path(
      test_dir,
      "nested",
      "result.rds"
    )

    result <- NULL

    utils::capture.output({

      result <- save_data_file(
        df = data.frame(
          value = 1:3
        ),
        filename = filename,
        overwrite = FALSE
      )
    })

    testthat::expect_true(
      file.exists(filename)
    )

    testthat::expect_identical(
      result$path,
      path.expand(filename)
    )

    testthat::expect_false(
      result$overwritten
    )

    testthat::expect_true(
      is.numeric(result$size_mb)
    )
  }
)



testthat::test_that(
  "save_data_file rejects directories as filenames",
  {

    target_dir <- tempfile(
      "save_data_file_directory_"
    )

    dir.create(
      target_dir,
      recursive = TRUE
    )

    testthat::expect_error(
      save_data_file(
        df = data.frame(value = 1),
        filename = target_dir
      ),
      "directory, not a file"
    )
  }
)


testthat::test_that(
  "data_snapshot creates a timestamped RDS snapshot",
  {

    snapshot_dir <- tempfile(
      "snapshot_test_"
    )

    example_data <- data.frame(
      sample = c("A", "B"),
      value = c(10, 20),
      stringsAsFactors = FALSE
    )

    testthat::capture_output(
      result <- data_snapshot(
        df = example_data,
        df_name = "example results",
        snapshot_dir = snapshot_dir
      )
    )


    testthat::expect_true(
      file.exists(result$path)
    )

    testthat::expect_false(
      result$overwritten
    )

    testthat::expect_match(
      basename(result$path),
      "^example_results_[0-9]{8}_[0-9]{6}(_[0-9]+)?_ver\\.rds$"
    )

    testthat::expect_equal(
      readRDS(result$path),
      example_data
    )
  }
)


testthat::test_that(
  "data_snapshot rejects unusable object names",
  {

    testthat::expect_error(
      data_snapshot(
        df = data.frame(value = 1),
        df_name = "!!!",
        snapshot_dir = tempdir()
      ),
      "usable filename"
    )
  }
)
