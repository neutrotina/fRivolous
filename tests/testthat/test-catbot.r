testthat::test_that(
  "catbot_say uses robocat by default",
  {

    output <- testthat::capture_output(
      result <- catbot_say(
        text = "ANALYSIS COMPLETE"
      )
    )

    testthat::expect_identical(
      result[1:5],
      c(
        "            o",
        "            |",
        "        /\\-----/\\",
        "       [  o _ o  ]",
        "       |   __    |"
      )
    )

    testthat::expect_true(
      any(
        result == "ANALYSIS COMPLETE"
      )
    )

    testthat::expect_match(
      output,
      "ANALYSIS COMPLETE"
    )
  }
)



testthat::test_that(
  "catbot_say rejects invalid text",
  {

    testthat::expect_error(
      catbot_say(
        text = character()
      ),
      "non-empty character vector"
    )

    testthat::expect_error(
      catbot_say(
        text = NA_character_
      ),
      "without blank values"
    )

    testthat::expect_error(
      catbot_say(
        text = ""
      ),
      "without blank values"
    )

    testthat::expect_error(
      catbot_say(
        text = c(
          "valid",
          " "
        )
      ),
      "without blank values"
    )
  }
)

testthat::test_that(
  "catbot_say rejects invalid styles",
  {

    testthat::expect_error(
      catbot_say(
        text = "TEST",
        style = "unknown"
      )
    )

    testthat::expect_error(
      catbot_say(
        text = "TEST",
        style = c("robocat", "catfloat")
      )
    )
  }
)

testthat::test_that(
  "catbot_say renders catbox text inside the box",
  {

    testthat::capture_output(
      result <- catbot_say(
        text = "hello",
        style = "catbox",
        width = 24L
      )
    )

    testthat::expect_identical(
      result[1:2],
      c(
        "  ∧,,,∧",
        " (• ⩊ •)"
      )
    )

    testthat::expect_match(
      result[3],
      "^┌─ U U "
    )

    testthat::expect_match(
      result[3],
      "┐$"
    )

    testthat::expect_match(
      result[4],
      "hello"
    )

    testthat::expect_match(
      result[4],
      "^│"
    )

    testthat::expect_match(
      result[4],
      "│$"
    )

    testthat::expect_match(
      result[5],
      "^└"
    )

    testthat::expect_match(
      result[5],
      "┘$"
    )

    testthat::expect_false(
      any(
        result[-4L] == "hello"
      )
    )

    testthat::expect_true(
      all(
        nchar(
          result[3:5],
          type = "width"
        ) == 26L
      )
    )
  }
)

testthat::test_that(
  "catbot_say supports aligned multi-line catbox messages",
  {
    message_lines <- c(
      "DATA CHECK COMPLETE",
      "NO PROBLEMS DETECTED"
    )

    for (alignment in c("left", "center", "right")) {

      testthat::capture_output({

        result <- catbot_say(
          text = message_lines,
          style = "catbox",
          width = 30L,
          align = alignment
        )
      })

      testthat::expect_length(
        result,
        6L
      )

      message_present <- vapply(
        message_lines,
        function(line) {
          any(
            grepl(
              line,
              result,
              fixed = TRUE
            )
          )
        },
        logical(1L)
      )

      testthat::expect_true(
        all(message_present)
      )

      testthat::expect_true(
        all(
          grepl(
            "^│.*│$",
            result[4:5]
          )
        )
      )

      testthat::expect_true(
        all(
          nchar(
            result[3:6],
            type = "width"
          ) == 32L
        )
      )
    }
  }
)
