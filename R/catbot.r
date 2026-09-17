#' Render a cat-topped message box
#'
#' @param text Character vector containing one or more message lines.
#' @param width Width of the text area inside the box.
#' @param align Text alignment: `"center"`, `"left"` or `"right"`.
#'
#' @return The rendered catbox as a character vector.
#'
#' @keywords internal
.render_catbox <- function(
    text,
    width = 24L,
    align = c(
      "center",
      "left",
      "right"
    )
) {
  if (
    !is.character(text) ||
      length(text) == 0L ||
      anyNA(text) ||
      any(!nzchar(trimws(text)))
  ) {
    stop(
      "'text' must be a non-empty character vector without blank values.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(width) ||
      length(width) != 1L ||
      is.na(width) ||
      !is.finite(width) ||
      width %% 1 != 0 ||
      width < 10L
  ) {
    stop(
      "'width' must be a single finite integer of at least 10.",
      call. = FALSE
    )
  }

  width <- as.integer(width)
  align <- match.arg(align)

  text_width <- nchar(
    text,
    type = "width"
  )

  if (any(text_width > width)) {
    stop(
      "Each line in 'text' must fit within the catbox width.",
      call. = FALSE
    )
  }

  format_line <- function(line) {

    remaining <- width - nchar(
      line,
      type = "width"
    )

    if (align == "left") {
      left_padding <- 0L
      right_padding <- remaining
    } else if (align == "right") {
      left_padding <- remaining
      right_padding <- 0L
    } else {
      left_padding <- floor(remaining / 2L)
      right_padding <- remaining - left_padding
    }

    paste0(
      "│",
      strrep(" ", left_padding),
      line,
      strrep(" ", right_padding),
      "│"
    )
  }

  cat_top <- c(
    "  ∧,,,∧",
    " (• ⩊ •)"
  )

  box_top <- paste0(
    "┌─ U U ",
    strrep(
      "─",
      width -6
    ),
    "┐"
  )

  box_bottom <- paste0(
    "└",
    strrep(
      "─",
      width
    ),
    "┘"
  )

  output <- c(
    cat_top,
    box_top,
    vapply(
      text,
      format_line,
      character(1L)
    ),
    box_bottom
  )

  output
}
#' Print a small catbot message
#'
#' Prints a catbot-style message using a robot cat, floating cat, or catbox
#'
#' @param text Character vector containing one or more message lines.
#' @param style Catbot style: `"robot_cat"`, `"catfloat"`, `"catbox"`
#' @param width Width of the text area when `style = "catbox"`.
#' @param align Text alignment when `style = "catbox"`:
#'   `"center"`, `"left"` or `"right"`.
#'
#' @return Invisibly returns the complete printed output as a character vector.
#'
#' @examples
#' catbot_say(
#'   text = "ANALYSIS COMPLETE"
#' )
#'
#' catbot_say(
#'   text = "DATA CHECK COMPLETE",
#'   style = "catbox"
#' )
#'
#'
#' @export
catbot_say <- function(
    text,
    style = c(
      "robot_cat",
      "catfloat",
      "catbox",
         ),
    width = 24L,
    align = c(
      "center",
      "left",
      "right"
    )
) {
  if (
    !is.character(text) ||
      length(text) == 0L ||
      anyNA(text) ||
      any(!nzchar(trimws(text)))
  ) {
    stop(
      "'text' must be a non-empty character vector without blank values.",
      call. = FALSE
    )
  }

  style <- match.arg(style)
  align <- match.arg(align)

  catbot_art <- list(

    robot_cat = c(
      "            o",
      "            |",
      "        /\\-----/\\",
      "       [  o _ o  ]",
      "       |   __    |"
    ),

    catfloat = c(
      " ノヽ",
      "∠)・/   ∧∧",
      " / /  ( ･w･)",
      "(  ￣￣∪∪￣)",
      " ~~~~~~~~~~~~"
    )


  )

  if (style == "catbox") {

    output <- .render_catbox(
      text = text,
      width = width,
      align = align
    )

  } else {

    output <- c(
      catbot_art[[style]],
      "",
      text
    )
  }

  cat(
    "\n",
    paste(
      output,
      collapse = "\n"
    ),
    "\n\n",
    sep = ""
  )

  invisible(
    output
  )
}
