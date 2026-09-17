testthat::test_that(
  "load_data_files imports supported file formats",
  {

    example_data <- data.frame(
      sample = c("A", "B"),
      value = c(10, 20),
      stringsAsFactors = FALSE
    )

    csv_file <- tempfile(
      fileext = ".csv"
    )

    tsv_file <- tempfile(
      fileext = ".tsv"
    )

    rds_file <- tempfile(
      fileext = ".rds"
    )

    txt_file <- tempfile(
      fileext = ".txt"
    )

    unsupported_file <- tempfile(
      fileext = ".xyz"
    )

    write.csv(
      example_data,
      csv_file,
      row.names = FALSE
    )

    write.table(
      example_data,
      tsv_file,
      sep = "\t",
      row.names = FALSE,
      quote = FALSE
    )

    saveRDS(
      example_data,
      rds_file
    )

    write.table(
      example_data,
      txt_file,
      sep = "\t",
      row.names = FALSE,
      quote = FALSE
    )

    writeLines(
      "unsupported format",
      unsupported_file
    )

    file_list <- list(
      csv_data = csv_file,
      tsv_data = tsv_file,
      rds_data = rds_file,
      txt_data = txt_file,
      unsupported_data = unsupported_file
    )

    output <- testthat::capture_output(
      result <- load_data_files(
        file_list = file_list,
        assign_to_global = FALSE,
        print_dims = FALSE,
        print_parameters = FALSE
      )
    )

    testthat::expect_setequal(
      names(result$data),
      c(
        "csv_data",
        "tsv_data",
        "rds_data",
        "txt_data"
      )
    )

    testthat::expect_identical(
      result$missing,
      character()
    )

    testthat::expect_identical(
      result$failed,
      "unsupported_data"
    )

    testthat::expect_equal(
      result$data$rds_data,
      example_data
    )

    testthat::expect_match(
      output,
      "DATA LOADING COMPLETE"
    )
  }
)


testthat::test_that(
  "load_data_files reports missing files",
  {

    missing_file <- tempfile(
      fileext = ".csv"
    )

    result <- testthat::capture_output(
      load_result <- load_data_files(
        file_list = c(
          missing_data = missing_file
        ),
        assign_to_global = FALSE,
        print_dims = FALSE,
        print_parameters = FALSE
      )
    )

    testthat::expect_identical(
      load_result$data,
      list()
    )

    testthat::expect_identical(
      load_result$missing,
      "missing_data"
    )

    testthat::expect_identical(
      load_result$failed,
      character()
    )
  }
)


testthat::test_that(
  "load_data_files assigns imported objects to the requested environment",
  {

    example_data <- data.frame(
      value = c(1, 2, 3)
    )

    rds_file <- tempfile(
      fileext = ".rds"
    )

    saveRDS(
      example_data,
      rds_file
    )

    target_environment <- new.env(
      parent = emptyenv()
    )

    testthat::capture_output(
      load_data_files(
        file_list = c(
          example_data = rds_file
        ),
        assign_to_global = TRUE,
        envir = target_environment,
        print_dims = FALSE,
        print_parameters = FALSE
      )
    )

    testthat::expect_true(
      exists(
        "example_data",
        envir = target_environment,
        inherits = FALSE
      )
    )

    testthat::expect_equal(
      get(
        "example_data",
        envir = target_environment
      ),
      example_data
    )
  }
)


testthat::test_that(
  "load_data_files rejects unnamed or duplicated file lists",
  {

    testthat::expect_error(
      load_data_files(
        file_list = c("file.csv")
      ),
      "unique, non-empty names"
    )

    testthat::expect_error(
      load_data_files(
        file_list = c(
          first = "one.csv",
          second = "two.csv"
        )
      ),
      "unique, non-empty names"
    )

    testthat::expect_error(
      load_data_files(
        file_list = c(
          duplicate = "one.csv",
          duplicate = "two.csv"
        )
      ),
      "unique, non-empty names"
    )
  }
)

testthat::test_that(
  "data_check reports missing cells and duplicated rows",
  {

    example_data <- data.frame(
      sample = c("A", "B", "A"),
      value = c(10, NA, 10),
      stringsAsFactors = FALSE
    )

    output <- testthat::capture_output(
      result <- data_check(
        example_data,
        object_name = "example_data"
      )
    )

    testthat::expect_identical(
      result$object_name,
      "example_data"
    )

    testthat::expect_identical(
      result$rows,
      3L
    )

    testthat::expect_identical(
      result$columns,
      2L
    )

    testthat::expect_identical(
      result$missing_cells,
      1L
    )

    testthat::expect_equal(
      result$completeness,
      83.33
    )

    testthat::expect_identical(
      result$duplicate_rows,
      1L
    )

    testthat::expect_match(
      output,
      "DATA CHECK COMPLETE"
    )
  }
)


testthat::test_that(
  "data_check handles a complete data frame",
  {

    example_data <- data.frame(
      a = c(1, 2),
      b = c("x", "y"),
      stringsAsFactors = FALSE
    )

    result <- testthat::capture_output(
      check_result <- data_check(example_data)
    )

    testthat::expect_identical(
      check_result$missing_cells,
      0L
    )

    testthat::expect_equal(
      check_result$completeness,
      100
    )

    testthat::expect_identical(
      check_result$duplicate_rows,
      0L
    )
  }
)


testthat::test_that(
  "data_check handles an empty data frame",
  {

    empty_data <- data.frame()

    result <- testthat::capture_output(
      check_result <- data_check(empty_data)
    )

    testthat::expect_identical(
      check_result$rows,
      0L
    )

    testthat::expect_identical(
      check_result$columns,
      0L
    )

    testthat::expect_identical(
      check_result$missing_cells,
      0L
    )

    testthat::expect_true(
      is.na(check_result$completeness)
    )

    testthat::expect_identical(
      check_result$duplicate_rows,
      0L
    )
  }
)


testthat::test_that(
  "data_check rejects non-data-frame input",
  {

    testthat::expect_error(
      data_check(matrix(1:4, nrow = 2)),
      "'df' must be a data frame"
    )

    testthat::expect_error(
      data_check(c(1, 2, 3)),
      "'df' must be a data frame"
    )
  }
)
