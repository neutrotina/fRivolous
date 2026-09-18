testthat::test_that(
  "add_h returns the expected ASCII headings",
  {

    testthat::capture_output(
      success_heading <- add_h(
        opt = "success",
        text = "ANALYSIS COMPLETE"
      )
    )

    testthat::expect_identical(
      success_heading,
      "[+] --- ANALYSIS COMPLETE --- [+]"
    )

    testthat::capture_output(
      warning_heading <- add_h(
        opt = 2L,
        text = "WARNING"
      )
    )

    testthat::expect_identical(
      warning_heading,
      "[!] --- WARNING --- [!]"
    )

    testthat::capture_output(
      arrows_heading <- add_h(
        opt = 3L,
        text = "STATUS"
      )
    )

    testthat::expect_identical(
      arrows_heading,
      "  >>>>>>>>> STATUS <<<<<<<<<<"
    )
  }
)


testthat::test_that(
  "add_h rejects invalid input",
  {

    testthat::expect_error(
      add_h(
        opt = 4L,
        text = "TEST"
      ),
      "integer from 1 to 3"
    )

    testthat::expect_error(
      add_h(
        opt = "unknown",
        text = "TEST"
      )
    )

    testthat::expect_error(
      add_h(
        opt = 1L,
        text = ""
      ),
      "non-empty"
    )

    testthat::expect_error(
      add_h(
        opt = 1L,
        text = NA_character_
      ),
      "non-empty"
    )
  }
)


testthat::test_that(
  "add_subh produces the requested display width",
  {

    styles <- c(
      "arrow",
      "double_arrow",
      "bracket",
      "ok",
      "neg",
      "load",
      "basic"
    )

    for (style_name in styles) {

      testthat::capture_output(
        result <- add_subh(
          opt = style_name,
          text = "STATUS",
          width = 40L
        )
      )

      testthat::expect_identical(
        nchar(
          result,
          type = "width"
        ),
        40L
      )
    }
  }
)


testthat::test_that(
  "add_subh rejects text that cannot fit",
  {

    testthat::expect_error(
      add_subh(
        opt = "basic",
        text = "A very long heading that cannot fit",
        width = 10L
      ),
      "too long"
    )

    testthat::expect_error(
      add_subh(
        opt = 8L,
        text = "TEST"
      ),
      "integer from 1 to 7"
    )
  }
)


testthat::test_that(
  "add_sep produces the requested display width",
  {

    styles <- c(
      "block",
      "dash",
      "dot",
      "equals"
    )

    for (style_name in styles) {

      testthat::capture_output(
        result <- add_sep(
          opt = style_name,
          width = 58L
        )
      )

      testthat::expect_identical(
        nchar(
          result,
          type = "width"
        ),
        58L
      )
    }
  }
)


testthat::test_that(
  "add_sep returns predictable ASCII separators",
  {

    testthat::capture_output(
      dash <- add_sep(
        opt = "dash",
        width = 10L
      )
    )

    testthat::expect_identical(
      dash,
      "----------"
    )

    testthat::capture_output(
      equals <- add_sep(
        opt = "equals",
        width = 10L
      )
    )

    testthat::expect_identical(
      equals,
      "=========="
    )
  }
)


testthat::test_that(
  "add_box preserves display width for Unicode and ASCII styles",
  {

    for (box_style in c("unicode", "ascii")) {

      testthat::capture_output(
        result <- add_box(
          text = c(
            "ANALYSIS",
            "COMPLETE"
          ),
          width = 20L,
          style = box_style
        )
      )

      testthat::expect_length(
        result,
        4L
      )

      testthat::expect_true(
        all(
          nchar(
            result,
            type = "width"
          ) == 22L
        )
      )
    }
  }
)


testthat::test_that(
  "add_box rejects lines wider than the text area",
  {

    testthat::expect_error(
      add_box(
        text = "TOO LONG",
        width = 4L
      ),
      "fit within"
    )

    testthat::expect_error(
      add_box(
        text = NA_character_
      ),
      "without NA"
    )
  }
)


testthat::test_that(
  "add_clock formats a supplied time",
  {

fixed_time <- as.POSIXct(
      "2026-09-14 15:30:45",
      tz = "UTC"
    )

    testthat::capture_output(
      result <- add_clock(
        time = fixed_time,
        width = 12L,
        style = "ascii"
      )
    )

    testthat::expect_length(
      result,
      4L
    )

    testthat::expect_true(
      any(grepl(
        "2026-09-14",
        result,
        fixed = TRUE
      ))
    )

    testthat::expect_true(
      any(grepl(
        "15:30:45",
        result,
        fixed = TRUE
      ))
    )
  }
)


testthat::test_that(
  "add_checklist produces fixed-width dotted output",
  {

    items <- c(
      "libraries loaded",
      "data imported",
      "integrity checked"
    )

    testthat::capture_output(
      result <- add_checklist(
        items = items,
        done = c(TRUE, TRUE, FALSE),
        style = "dotted",
        width = 68L
      )
    )

    testthat::expect_length(
      result,
      length(items)
    )

    testthat::expect_true(
      all(
        nchar(
          result,
          type = "width"
        ) == 68L
      )
    )
  }
)


testthat::test_that(
  "add_checklist compact style reports all items",
  {

    items <- c(
      "first task",
      "second task"
    )

    testthat::capture_output(
      result <- add_checklist(
        items = items,
        done = c(TRUE, FALSE),
        style = "compact"
      )
    )

    testthat::expect_length(
      result,
      2L
    )

    testthat::expect_true(
      all(
        vapply(
          items,
          function(item) {
            any(grepl(item, result, fixed = TRUE))
          },
          logical(1L)
        )
      )
    )
  }
)


testthat::test_that(
  "add_key_list aligns output to the requested width",
  {

    testthat::capture_output(
      result <- add_key_list(
        left = c(
          "[MODEL]:",
          "[GROUPS]:"
        ),
        right = c(
          "LINEAR MODEL",
          "CONTROL VS LASER"
        ),
        width = 50L
      )
    )

    testthat::expect_length(
      result,
      2L
    )

    testthat::expect_true(
      all(
        nchar(
          result,
          type = "width"
        ) == 50L
      )
    )
  }
)


testthat::test_that(
  "formatting functions reject invalid widths",
  {

    testthat::expect_error(
      add_subh(
        text = "TEST",
        width = 9L
      ),
      "at least 10"
    )

    testthat::expect_error(
      add_sep(
        width = 2L
      ),
      "at least 3"
    )

    testthat::expect_error(
      add_box(
        text = "TEST",
        width = 0L
      ),
      "greater than zero"
    )

    testthat::expect_error(
      add_checklist(
        items = "TEST",
        width = 19L
      ),
      "at least 20"
    )

    testthat::expect_error(
      add_key_list(
        left = "A",
        right = "B",
        width = 19L
      ),
      "at least 20"
    )
  }
)
