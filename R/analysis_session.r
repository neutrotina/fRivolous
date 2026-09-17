#' Run a Git command in a project
#'
#' @param project Project directory.
#' @param args Character vector of Git arguments.
#'
#' @return A list containing command output and exit status.
#'
#' @keywords internal
.git_run <- function(
    project,
    args
) {
  if (!nzchar(Sys.which("git"))) {
    stop(
      "Git executable was not found on PATH.",
      call. = FALSE
    )
  }

  output <- system2(
    command = "git",
    args = c("-C", project, args),
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(
    output,
    "status",
    exact = TRUE
  )

  if (is.null(status)) {
    status <- 0L
  }

  list(
    output = output,
    status = as.integer(status)
  )
}


#' Capture the Git state of a project
#'
#' @param project Project directory.
#'
#' @return A list containing Git availability, repository status, branch and
#'   working-tree status.
#'
#' @keywords internal
.git_state <- function(
    project
) {
  if (!nzchar(Sys.which("git"))) {
    return(
      list(
        available = FALSE,
        repository = FALSE,
        branch = NA_character_,
        status = character()
      )
    )
  }

  repository_check <- .git_run(
    project = project,
    args = c(
      "rev-parse",
      "--is-inside-work-tree"
    )
  )

  is_repository <- repository_check$status == 0L &&
    any(
      trimws(repository_check$output) == "true"
    )

  if (!is_repository) {
    return(
      list(
        available = TRUE,
        repository = FALSE,
        branch = NA_character_,
        status = character()
      )
    )
  }

  branch_result <- .git_run(
    project = project,
    args = c(
      "branch",
      "--show-current"
    )
  )

  status_result <- .git_run(
    project = project,
    args = c(
      "status",
      "--short"
    )
  )

  list(
    available = TRUE,
    repository = TRUE,
    branch = trimws(
      branch_result$output[1L]
    ),
    status = status_result$output
  )
}


#' Start an analysis session
#'
#' Activates the project-local `renv` environment when requested, records the
#' initial Git state and starts writing console output to a log file while
#' continuing to display it on screen.
#'
#' The returned session object must be passed to [end_analysis()] to close the
#' sink safely.
#'
#' @param project Project directory. It must already exist.
#' @param log_file Optional path to the analysis log. If `NULL`, a timestamped
#'   log is created under `project/logs`.
#' @param activate_renv Logical; should [renv::activate()] be called?
#'
#' @return An object of class `"Analysis_session"` containing the
#'   project path, log path, start time, output connection and initial Git
#'   state.
#'
#' @section Side effects:
#' The project-local `renv` may be activated, a log file may be created and an
#' output sink is started.
#'
#' @examples
#' \dontrun{
#' session <- start_analysis(
#'   project = here::here(),
#'   log_file = here::here(
#'     "logs",
#'     "analysis_session.log"
#'   )
#' )
#' }
#'
#' @export
start_analysis <- function(
    project = getwd(),
    log_file = NULL,
    activate_renv = TRUE
) {
  if (!is.character(project) ||
      length(project) != 1L ||
      is.na(project) ||
      !nzchar(trimws(project))) {
    stop(
      "'project' must be a single non-empty path.",
      call. = FALSE
    )
  }

  if (!is.logical(activate_renv) ||
      length(activate_renv) != 1L ||
      is.na(activate_renv)) {
    stop(
      "'activate_renv' must be one non-missing logical value.",
      call. = FALSE
    )
  }

  project <- normalizePath(
    path.expand(trimws(project)),
    mustWork = FALSE
  )

  if (!dir.exists(project)) {
    stop(
      "Project directory does not exist:\n",
      project,
      call. = FALSE
    )
  }

  if (isTRUE(activate_renv)) {
    if (!requireNamespace("renv", quietly = TRUE)) {
      stop(
        "Package 'renv' is required when activate_renv = TRUE.",
call. = FALSE
      )
    }

    renv::activate(
      project = project
    )
  }

  if (is.null(log_file)) {
    timestamp <- format(
      Sys.time(),
      "%Y%m%d_%H%M%S"
    )

    log_file <- file.path(
      project,
      "logs",
      paste0(
        "analysis_",
        timestamp,
        ".log"
      )
    )
  }

  if (!is.character(log_file) ||
      length(log_file) != 1L ||
      is.na(log_file) ||
      !nzchar(trimws(log_file))) {
    stop(
      "'log_file' must be NULL or a single non-empty path.",
      call. = FALSE
    )
  }

  log_file <- path.expand(
    trimws(log_file)
  )

  log_dir <- dirname(
    log_file
  )

  if (!dir.exists(log_dir)) {
    dir.create(
      log_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  if (!dir.exists(log_dir)) {
    stop(
      "Could not create log directory:\n",
      log_dir,
      call. = FALSE
    )
  }

  git_state <- .git_state(
    project
  )

  output_connection <- file(
    description = log_file,
    open = "wt",
    encoding = "UTF-8"
  )

  sink(
    file = output_connection,
    split = TRUE
  )

  start_time <- Sys.time()

  cat(
    "\n[+] --- ANALYSIS SESSION STARTED --- [+]\n",
    "    >> PROJECT: ",
    project,
    "\n",
    "    >> LOG: ",
    log_file,
    "\n",
    "    >> STARTED: ",
    format(start_time),
    "\n",
    sep = ""
  )

  if (git_state$repository) {
    cat(
      "    >> GIT BRANCH: ",
      git_state$branch,
      "\n",
      sep = ""
    )
  } else {
    cat(
      "    >> GIT: No repository detected\n"
    )
  }

  structure(
    list(
      project = project,
      log_file = log_file,
      started = start_time,
      output_connection = output_connection,
      git = git_state,
      ended = NULL
    ),
    class = "Analysis_session"
  )
}


#' End an analysis session
#'
#' Completes an analysis session using [objective_complete()], closes the
#' analysis log sink, and optionally snapshots `renv` and creates an explicitly
#' scoped Git commit.
#'
#' @param session An object returned by [start_analysis()].
#' @param snapshot_renv Logical; should [renv::snapshot()] be called before
#'   completing the session?
#' @param commit Logical; should the explicitly listed files be staged and
#'   committed?
#' @param commit_files Character vector of project-relative files or
#'   directories to stage. Required when `commit = TRUE`.
#' @param commit_message Commit message. Required when `commit = TRUE`.
#'
#' @return Invisibly returns a list containing completion time, session log
#'   path, `objective_complete()` output and Git commit information.
#'
#' @section Side effects:
#' The output sink is closed. When requested, `renv::snapshot()` modifies
#' `renv.lock`, and Git files are staged and committed.
#'
#' @examples
#' \dontrun{
#' end_analysis(
#'   session = session,
#'   snapshot_renv = FALSE,
#'   commit = TRUE,
#'   commit_files = c(
#'     "R/",
#'   "tests/",
#'     "renv.lock"
#'   ),
#'   commit_message = "analysis: update utilities"
#' )
#' }
#'
#' @export
end_analysis <- function(
    session,
    snapshot_renv = FALSE,
    commit = FALSE,
    commit_files = character(),
    commit_message = NULL
) {
  if (!inherits(
    session,
    "Analysis_session"
  )) {
    stop(
      "'session' must be returned by start_analysis().",
      call. = FALSE
    )
  }

  if (!is.logical(snapshot_renv) ||
      length(snapshot_renv) != 1L ||
      is.na(snapshot_renv)) {
    stop(
      "'snapshot_renv' must be one non-missing logical value.",
      call. = FALSE
    )
  }

  if (!is.logical(commit) ||
      length(commit) != 1L ||
      is.na(commit)) {
    stop(
      "'commit' must be one non-missing logical value.",
      call. = FALSE
    )
  }

  if (isTRUE(commit)) {
    if (!is.character(commit_files) ||
        length(commit_files) == 0L ||
        anyNA(commit_files) ||
        any(!nzchar(trimws(commit_files)))) {
      stop(
        "'commit_files' must be a non-empty character vector when commit = TRUE.",
        call. = FALSE
      )
    }

if (!is.character(commit_message) ||
        length(commit_message) != 1L ||
        is.na(commit_message) ||
        !nzchar(trimws(commit_message))) {
      stop(
        "'commit_message' must be a single non-empty value when commit = TRUE.",
        call. = FALSE
      )
    }

    if (!session$git$repository) {
      stop(
        "Cannot commit because the project is not a Git repository.",
        call. = FALSE
      )
    }

    if (any(
      grepl(
        "^(/|[A-Za-z]:[\\\\/])",
        commit_files
      )
    )) {
      stop(
        "'commit_files' must contain project-relative paths.",
        call. = FALSE
      )
    }
  }

  if (isTRUE(snapshot_renv)) {
    if (!requireNamespace("renv", quietly = TRUE)) {
      stop(
        "Package 'renv' is required when snapshot_renv = TRUE.",
        call. = FALSE
      )
    }

    renv::snapshot(
      project = session$project,
      prompt = FALSE
    )
  }

  completion <- objective_complete(
    log_file = NULL,
    show_session_info = TRUE
  )

  sink(
    type = "output"
  )

  if (isOpen(session$output_connection)) {
    close(
      session$output_connection
    )
  }

  commit_result <- list(
    requested = isTRUE(commit),
    committed = FALSE,
    output = character()
  )

  if (isTRUE(commit)) {
    add_result <- .git_run(
      project = session$project,
      args = c(
        "add",
        "--",
        commit_files
      )
    )

    if (add_result$status != 0L) {
      stop(
        "Git staging failed:\n",
        paste(add_result$output, collapse = "\n"),
        call. = FALSE
      )
    }

    staged_diff <- .git_run(
      project = session$project,
      args = c(
        "diff",
        "--cached",
        "--quiet"
      )
    )

    if (staged_diff$status > 1L) {
      stop(
        "Could not inspect the staged Git diff:\n",
        paste(staged_diff$output, collapse = "\n"),
        call. = FALSE
      )
    }

    if (staged_diff$status == 1L) {
      commit_result_raw <- .git_run(
        project = session$project,
        args = c(
          "commit",
          "-m",
          commit_message
        )
      )

      if (commit_result_raw$status != 0L) {
        stop(
          "Git commit failed:\n",
          paste(commit_result_raw$output, collapse = "\n"),
          call. = FALSE
        )
      }

      commit_result$committed <- TRUE
      commit_result$output <- commit_result_raw$output
    }
  }

  session$ended <- Sys.time()

  invisible(
    list(
      started = session$started,
      ended = session$ended,
      log_file = session$log_file,
      completion = completion,
      git = commit_result
    )
  )
}
