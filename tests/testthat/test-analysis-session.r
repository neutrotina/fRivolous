testthat::test_that(
  "start_analysis creates a session and log file",
  {

    project <- tempfile(
      "analysis_project_"
    )

    log_file <- file.path(
      project,
      "logs",
      "analysis.log"
    )

    dir.create(
      project,
      recursive = TRUE
    )

    testthat::capture_output(
      session <- start_analysis(
        project = project,
        log_file = log_file,
        activate_renv = FALSE
      )
    )

    testthat::expect_s3_class(
      session,
      "Analysis_session"
    )

    testthat::expect_identical(
      session$project,
      normalizePath(
        project,
        mustWork = FALSE
      )
    )

    testthat::expect_true(
      file.exists(log_file)
    )

    testthat::expect_true(
      is.list(session$git)
    )

    testthat::capture_output(
      result <- end_analysis(
        session = session,
        snapshot_renv = FALSE,
        commit = FALSE
      )
    )

    testthat::expect_false(
      result$git$committed
    )

    testthat::expect_match(
      paste(
        readLines(log_file),
        collapse = "\n"
      ),
"OBJECTIVE COMPLETE: ANALYSIS COMPLETE"
    )
  }
)


testthat::test_that(
  "start_analysis creates a default log path",
  {

    project <- tempfile(
      "analysis_project_default_log_"
    )

    dir.create(
      project,
      recursive = TRUE
    )

    testthat::capture_output(
      session <- start_analysis(
        project = project,
        activate_renv = FALSE
      )
    )

    testthat::expect_true(
      grepl(
        "analysis_[0-9]{8}_[0-9]{6}\\.log$",
        session$log_file
      )
    )

    testthat::expect_true(
      file.exists(session$log_file)
    )

    testthat::capture_output(
      end_analysis(
        session = session,
        commit = FALSE
      )
    )
  }
)


testthat::test_that(
  "end_analysis rejects a commit without an explicit file list",
  {

    project <- tempfile(
      "analysis_project_commit_validation_"
    )

    dir.create(
      project,
      recursive = TRUE
    )

    testthat::capture_output(
      session <- start_analysis(
        project = project,
        activate_renv = FALSE
      )
    )

    testthat::expect_error(
      end_analysis(
        session = session,
        commit = TRUE,
        commit_message = "test commit"
      ),
      "commit_files"
    )

    # Clean up the active sink after the intentional validation failure.
    testthat::capture_output(
      end_analysis(
        session = session,
        commit = FALSE
      )
    )
  }
)


testthat::test_that(
  "end_analysis rejects absolute commit paths",
  {

    project <- tempfile(
      "analysis_project_absolute_path_"
    )

    dir.create(
      project,
      recursive = TRUE
    )

    testthat::skip_if(
      !nzchar(Sys.which("git")),
      "Git is not available."
    )

    testthat::capture_output(
      system2(
        "git",
        c(
          "-C",
          project,
          "init"
        )
      )
    )

    testthat::capture_output(
      session <- start_analysis(
        project = project,
        activate_renv = FALSE
      )
    )

    testthat::expect_error(
      end_analysis(
        session = session,
        commit = TRUE,
        commit_files = file.path(
          project,
          "file.R"
        ),
        commit_message = "test commit"
      ),
      "project-relative"
    )

    testthat::capture_output(
      end_analysis(
        session = session,
        commit = FALSE
      )
    )
  }
)


testthat::test_that(
  "analysis session rejects invalid inputs",
  {

    testthat::expect_error(
      start_analysis(
        project = tempfile(),
        activate_renv = FALSE
      ),
      "does not exist"
    )

    testthat::expect_error(
      start_analysis(
        project = tempdir(),
        activate_renv = NA
      ),
      "activate_renv"
    )

    testthat::expect_error(
      end_analysis(
        session = list()
      ),
      "start_analysis"
    )
  }
)
