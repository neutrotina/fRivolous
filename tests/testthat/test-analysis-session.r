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
  "start_analysis creates a session and log file",
  {
 project <- new_test_dir(
  "analysis_project_"
)



    log_file <- file.path(
      project,
      "logs",
      "analysis.log"
    )

    session <- fRivolous::start_analysis(
      project = project,
      log_file = log_file,
      activate_renv = FALSE
    )

    testthat::expect_s3_class(
      session,
      "Analysis_session"
    )

    completion <- fRivolous::end_analysis(
      session = session,
      snapshot_renv = FALSE,
      commit = FALSE
    )

    testthat::expect_true(
      file.exists(log_file)
    )

    testthat::expect_true(
      is.list(completion)
    )

    log_text <- paste(
      readLines(log_file),
      collapse = "\n"
    )

    testthat::expect_match(
      log_text,
      "ANALYSIS SESSION STARTED"
    )

    testthat::expect_match(
      log_text,
      "OBJECTIVE COMPLETE: ANALYSIS COMPLETE"
    )
  }
)
testthat::test_that(
  "start_analysis creates a default log path",
  {
    project <- new_test_dir(
      pattern = "analysis_project_default_log_"
    )

    session <- fRivolous::start_analysis(
      project = project,
      activate_renv = FALSE
    )

    fRivolous::end_analysis(
      session = session,
      snapshot_renv = FALSE,
      commit = FALSE
    )

    testthat::expect_true(
      file.exists(session$log_file)
    )

    testthat::expect_match(
      basename(session$log_file),
      "^analysis_[0-9]{8}_[0-9]{6}\\.log$"
    )

    testthat::expect_match(
      dirname(session$log_file),
      "logs$"
    )
  }
)

testthat::test_that(
  "end_analysis rejects a commit without an explicit file list",
  {
    project <- new_test_dir(
      pattern = "analysis_project_invalid_commit_"
    )

    session <- fRivolous::start_analysis(
      project = project,
      activate_renv = FALSE
    )

    testthat::expect_error(
      fRivolous::end_analysis(
        session = session,
        commit = TRUE,
        commit_files = character(),
        commit_message = "test"
      ),
      "'commit_files'"
    )


    fRivolous::end_analysis(
      session = session,
      commit = FALSE
    )
  }
)

testthat::test_that(
  "end_analysis rejects absolute commit paths",
  {
    project <- new_test_dir(
      pattern = "analysis_project_absolute_path_"
    )

    testthat::skip_if(
      !nzchar(Sys.which("git")),
      "Git is not available."
    )

    system2(
      "git",
      c(
        "-C",
        project,
        "init"
      )
    )

    session <- fRivolous::start_analysis(
      project = project,
      activate_renv = FALSE
    )

    testthat::expect_error(
      fRivolous::end_analysis(
        session = session,
        commit = TRUE,
        commit_files = "/tmp/absolute/file.R",
        commit_message = "test"
      ),
      "project-relative"
    )

    fRivolous::end_analysis(
      session = session,
      commit = FALSE
    )
  }
)


testthat::test_that(
  "analysis session rejects invalid inputs",
  {
    testthat::expect_error(
      fRivolous::start_analysis(
        project = tempfile(),
        activate_renv = FALSE
      ),
      "does not exist"
    )

    testthat::expect_error(
      fRivolous::start_analysis(
        project = tempdir(),
        activate_renv = NA
      ),
      "activate_renv"
    )

    testthat::expect_error(
      fRivolous::end_analysis(
        session = list()
      ),
      "start_analysis"
    )
  }
)
