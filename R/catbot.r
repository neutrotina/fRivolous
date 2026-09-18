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
      "\u2502",
      strrep(" ", left_padding),
      line,
      strrep(" ", right_padding),
      "\u2502"
    )
  }

cat_top <- c(
  "  \u2227,,,\u2227",
  " (\u2022 \u2a4a \u2022)"
)

box_top <- paste0(
  "\u250c\u2500 U U ",
  strrep(
    "\u2500",
    width - 6L
  ),
  "\u2510"
)

box_bottom <- paste0(
  "\u2514",
  strrep(
    "\u2500",
    width
  ),
  "\u2518"
)

box_lines <- unname(
  vapply(
    text,
    format_line,
    character(1L)
  )
)

output <- c(
  cat_top,
  box_top,
  box_lines,
  box_bottom
)

unname(output)

}
#' Print a small catbot message
#'
#' Prints a catbot-style message using a robot cat, floating cat, or catbox
#'
#' @param text Character vector containing one or more message lines.
#' @param style Catbot style: `"robocat"`, `"catfloat"`, `"catbox"`
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
      "robocat",
      "catfloat",
      "catbox"
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
  robocat = c(
    "            o",
    "            |",
    "        /\\-----/\\",
    "       [  o _ o  ]",
    "       |   __    |"
  ),

  catfloat = c(
    " \u30ce\u30fd",
    "\u2220)\u30fb/   \u2227\u2227",
    " / /  ( \uff65\u2a4a\uff65)",
    "(  \uffe3\uffe3\u222a\u222a\uffe3)",
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
output <- unname(output)

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
#'
#' Add formatted separators to R output with cats!
#'
#' @param opt Separator style, supplied as a number or name.
#' @param width Total width of the separator, including brackets where used.
#'
#' @return The printed separator invisibly.
#' @export
#' @examples
#' add_cat_sep(
#'   opt = "cat2"
#' )
#'
#' add_cat_sep(
#'   opt = "cat1",
#'   width = 58L
#' )
#'
add_cat_sep <- function(
    opt = 1L,
    width = 58L
) {
  separator_styles <- list(
    cat1 = list(
      left = "",
      right = "",
      symbol = " \u269e^\u2022\u2a4a\u2022^\u269f "
    ),
    cat2 = list(
      left = "",
      right = "",
      symbol = " \u269e^. .^\u269f "
    ),
    cat3 = list(
      left = "",
      right = "",
      symbol = "\u208d^. .^\u208e\u27c6 "
    )
  )


  if (
    !is.numeric(width) ||
      length(width) != 1L ||
      is.na(width) ||
      !is.finite(width) ||
      width %% 1 != 0 ||
      width < 3
  ) {
    stop(
      "'width' must be a single finite integer of at least 3.",
      call. = FALSE
    )
  }

  width <- as.integer(width)

  if (
    is.numeric(opt)
  ) {
    if (
      length(opt) != 1L ||
        is.na(opt) ||
        !is.finite(opt) ||
        opt %% 1 != 0 ||
        opt < 1 ||
        opt > length(separator_styles)
    ) {
      stop(
        "'opt' must be an integer from 1 to 3.",
        call. = FALSE
      )
    }

    style_name <- names(separator_styles)[as.integer(opt)]
  } else if (
    is.character(opt) &&
      length(opt) == 1L &&
      !is.na(opt) &&
      nzchar(opt)
  ) {
    style_name <- match.arg(
      opt,
      choices = names(separator_styles)
    )
  } else {
    stop(
      "'opt' must be a single number or separator name.",
      call. = FALSE
    )
  }

  style <- separator_styles[[style_name]]

  fixed_width <- nchar(
    style$left,
    type = "width"
  ) +
    nchar(
      style$right,
      type = "width"
    )

  symbol_width <- nchar(
    style$symbol,
    type = "width"
  )

  available_width <- width - fixed_width

  if (
    available_width < symbol_width
  ) {
    stop(
      "'width' is too small for the selected separator style.",
      call. = FALSE
    )
  }

  n_symbols <- floor(
    available_width / symbol_width
  )

  separator <- paste0(
    style$left,
    strrep(
      style$symbol,
      n_symbols
    ),
    style$right
  )

  cat(
    "\n",
    separator,
    "\n\n",
    sep = ""
  )

  invisible(
    separator
  )
}
