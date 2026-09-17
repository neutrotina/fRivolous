#' Add custom headers to R scripts
#' @param opt Heading style, supplied as a number or name.
#' @param text text string for the header
#' @return the printed header
#'@export
#' @examples
#' add_h(
#'  opt = 1,
#'  text = "OBJECTIVE COMPLETE: ANALYSIS COMPLETE"
#' )
#'
#' add_h(
#'  opt = "warning",
#'  text = "OBJECTIVE COMPLETE: ANALYSIS COMPLETE"
#' )
#'
add_h <- function(
    opt = 1L,
    text
) {
  if (
    !is.character(text) ||
      length(text) != 1L ||
      is.na(text) ||
      !nzchar(trimws(text))
  ) {
    stop(
      "'text' must be a single non-empty character value.",
      call. = FALSE
    )
  }

  heading_styles <- list(
    success = list(
      prefix = "[+] ---",
      suffix = " --- [+]"
    ),
    warning = list(
      prefix = "[!] ---",
      suffix = " --- [!]"
    ),
    arrows = list(
    prefix = "  >>>>>>>>>",
    suffix = "<<<<<<<<<<<"
    )
    )


  if (
    is.numeric(opt)
  ) {
    if (
      length(opt) != 1L ||
        is.na(opt) ||
        !is.finite(opt) ||
        opt %% 1 != 0 ||
        opt < 1 ||
        opt > length(heading_styles)
    ) {
      stop(
        "'opt' must be an integer from 1 to 3.",
        call. = FALSE
      )
    }

    style_name <- names(
      heading_styles
    )[as.integer(opt)]
  } else if (
    is.character(opt) &&
      length(opt) == 1L &&
      !is.na(opt) &&
      nzchar(opt)
  ) {
    style_name <- match.arg(
      opt,
      choices = names(heading_styles)
    )
  } else {
    stop(
      "'opt' must be an integer from 1 to 3 or a valid style name.",
      call. = FALSE
    )
  }

  style <- heading_styles[[style_name]]

  heading <- paste0(
    style$prefix,
    " ",
    text,
    style$suffix
  )

  cat(
    "\n",
    heading,
    "\n\n",
    sep = ""
  )

  invisible(
    heading
  )
}
#' Add formatted subheadings to R output
#'
#' @param opt Subheading style, supplied as a number or name.
#' @param text A single non-empty character value.
#' @param width Width of the printed subheading.
#'
#' @return The printed subheading invisibly.
#' @export
add_subh <- function(
    opt = 1L,
    text,
    width = 60L
) {
  if (
    !is.character(text) ||
      length(text) != 1L ||
      is.na(text) ||
      !nzchar(trimws(text))
  ) {
    stop(
      "'text' must be a single non-empty character value.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(width) ||
      length(width) != 1L ||
      is.na(width) ||
      !is.finite(width) ||
      width %% 1 != 0 ||
      width < 10
  ) {
    stop(
      "'width' must be a single finite integer of at least 10.",
      call. = FALSE
    )
  }

  width <- as.integer(width)

  subheading_styles <- list(
    arrow = list(
      prefix = "▷▷▷ ",
      suffix = " ▷",
      fill = "."
    ),
    double_arrow = list(
      prefix = "»» ",
      suffix = " «",
      fill = "·"
    ),
    bracket = list(
      prefix = "[+] ",
      suffix = " [+]",
      fill = "-"
    ),
      ok = list(
      prefix = "▷ ",
      suffix = " [✔OK]",
      fill = "."
      ),
   neg = list(
   prefix = "[!] ",
      suffix = " [✘]",
      fill = "."
   ),
load = list(
   prefix = "[⚙] --- ",
      suffix = " --- [⚙]",
      fill = "."
   ),
  basic = list(
   prefix = " ▷ ",
      suffix = " ◁",
      fill = " "
   )
  )

  if (
    is.numeric(opt)
  ) {
    if (
      length(opt) != 1L ||
        is.na(opt) ||
        !is.finite(opt) ||
        opt %% 1 != 0 ||
        opt < 1 ||
        opt > length(subheading_styles)
    ) {
      stop(
        "'opt' must be an integer from 1 to 7.",
        call. = FALSE
      )
    }

    style_name <- names(
      subheading_styles
    )[as.integer(opt)]
  } else if (
    is.character(opt) &&
      length(opt) == 1L &&
      !is.na(opt) &&
      nzchar(opt)
  ) {
    style_name <- match.arg(
      opt,
      choices = names(subheading_styles)
    )
  } else {
    stop(
      "'opt' must be a number or a valid style name.",
      call. = FALSE
    )
  }

  style <- subheading_styles[[style_name]]

  fixed_width <- nchar(
    style$prefix,
    type = "width"
  ) +
    nchar(
      text,
      type = "width"
    ) +
    nchar(
      style$suffix,
      type = "width"
    )

  fill_width <- width - fixed_width

  if (
    fill_width < 1L
  ) {
    stop(
      "'text' is too long for the requested width.",
      call. = FALSE
    )
  }

  subheading <- paste0(
    style$prefix,
    text,
    strrep(
      style$fill,
      fill_width
    ),
    style$suffix
  )

  cat(
    "\n",
    subheading,
    "\n\n",
    sep = ""
  )

  invisible(
    subheading
  )
}

#'
#' Add formatted separators to R output
#'
#' @param opt Separator style, supplied as a number or name.
#' @param width Total width of the separator, including brackets where used.
#'
#' @return The printed separator invisibly.
#' @export
#' @examples
#' add_sep(
#'   opt = "block"
#' )
#'
#' add_sep(
#'   opt = "dash",
#'   width = 58L
#' )
#'
#' add_sep(
#'   opt = 3L,
#'   width = 58L
#' )
#'
add_sep <- function(
    opt = 1L,
    width = 58L
) {
  separator_styles <- list(
    dash = list(
      left = "",
      right = "",
      symbol = "-"
    ),
    dot = list(
      left = "",
      right = "",
      symbol = "."
    ),
    equals = list(
      left = "",
      right = "",
      symbol = "="
    )
    block = list(
      left = "[ ",
      right = " ]",
      symbol = "░"
    ),
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
        "'opt' must be an integer from 1 to 4.",
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
#' Add a boxed message to R output
#'
#' @param text Character vector of one or more lines to print.
#' @param width Width of the text area inside the box.
#' @param align Text alignment: `"center"`, `"left"` or `"right"`.
#' @param style Box style: `"unicode"` or `"ascii"`.
#'
#' @return The printed box invisibly as a character vector.
#' @export
#'
#' @examples
#' add_box(
#'   text = "ANALYSIS"
#' )
#'
#' add_box(
#'   text = c(
#'     "ANALYSIS",
#'     "COMPLETE"
#'   )
#' )
add_box <- function(
    text,
    width = 35L,
    align = c(
      "center",
      "left",
      "right"
    ),
    style = c(
      "unicode",
      "ascii"
    )
) {
  if (
    !is.character(text) ||
      length(text) == 0L ||
      anyNA(text)
  ) {
    stop(
      "'text' must be a non-empty character vector without NA values.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(width) ||
      length(width) != 1L ||
      is.na(width) ||
      !is.finite(width) ||
      width %% 1 != 0 ||
      width < 1
  ) {
    stop(
      "'width' must be a single finite integer greater than zero.",
      call. = FALSE
    )
  }

  width <- as.integer(width)

  align <- match.arg(align)
  style <- match.arg(style)

  if (
    style == "unicode"
  ) {
    border <- list(
      top_left = "╔",
      top = "═",
      top_right = "╗",
      side = "║",
      bottom_left = "╚",
      bottom = "═",
      bottom_right = "╝"
    )
  } else {
    border <- list(
      top_left = "+",
      top = "-",
      top_right = "+",
      side = "|",
      bottom_left = "+",
      bottom = "-",
      bottom_right = "+"
    )
  }

  text_width <- nchar(
    text,
    type = "width"
  )

  if (
    any(text_width > width)
  ) {
    stop(
      "Each line in 'text' must fit within 'width'.",
      call. = FALSE
    )
  }

  format_line <- function(
      line
  ) {
    remaining <- width - nchar(
      line,
      type = "width"
    )

    if (
      align == "left"
    ) {
      left_padding <- 0L
      right_padding <- remaining
    } else if (
      align == "right"
    ) {
      left_padding <- remaining
      right_padding <- 0L
    } else {
      left_padding <- floor(
        remaining / 2
      )
      right_padding <- remaining - left_padding
    }

    paste0(
      border$side,
      strrep(
        " ",
        left_padding
      ),
      line,
      strrep(
        " ",
        right_padding
      ),
      border$side
    )
  }

  output <- c(
    paste0(
      border$top_left,
      strrep(
        border$top,
        width
      ),
      border$top_right
    ),
    vapply(
      text,
      format_line,
      character(1L)
    ),
    paste0(
      border$bottom_left,
      strrep(
        border$bottom,
        width
      ),
      border$bottom_right
    )
  )

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
#' ADD the current date and time in a box
#'
#' @param time A date-time object. Defaults to the current time.
#' @param date_format Format passed to [base::format()] for the date.
#' @param time_format Format passed to [base::format()] for the time.
#' @param width Width of the box interior.
#' @param style Box style passed to [add_box()].
#'
#' @return The printed box invisibly as a character vector.
#' @export
#'
#' @examples
#' add_clock()
#'
#' add_clock(
#'   date_format = "%d/%m/%Y"
#' )
add_clock <- function(
    time = Sys.time(),
    date_format = "%Y-%m-%d",
    time_format = "%H:%M:%S",
    width = 12L,
    style = c(
      "unicode",
      "ascii"
    )
) {
  if (
    !inherits(
      time,
      c(
        "POSIXct",
        "POSIXlt"
      )
    )
  ) {
    stop(
      "'time' must be a POSIXct or POSIXlt object.",
      call. = FALSE
    )
  }

  if (
    !is.character(date_format) ||
      length(date_format) != 1L ||
      is.na(date_format) ||
      !nzchar(date_format)
  ) {
    stop(
      "'date_format' must be a single non-empty character value.",
      call. = FALSE
    )
  }

  if (
    !is.character(time_format) ||
      length(time_format) != 1L ||
      is.na(time_format) ||
      !nzchar(time_format)
  ) {
    stop(
      "'time_format' must be a single non-empty character value.",
      call. = FALSE
    )
  }

  style <- match.arg(style)

  current_date <- format(
    time,
    format = date_format
  )

  current_time <- format(
    time,
    format = time_format
  )

  add_box(
    text = c(
      current_date,
      current_time
    ),
    width = width,
    align = "center",
    style = style
  )
}
#' Add a formatted checklist to R output
#'
#' @param items Character vector containing task descriptions.
#' @param done Logical vector indicating which tasks are complete.
#' @param style Checklist style: `"dotted"` or `"compact"`.
#' @param width Target width for the dotted style.
#'
#' @return The printed checklist invisibly as a character vector.
#' @export
#'
#' @examples
#' add_checklist(
#'   items = c(
#'     "libraries added",
#'     "loading data",
#'     "checking data"
#'   ),
#'   done = c(
#'     TRUE,
#'     TRUE,
#'     FALSE
#'   )
#' )
#'
#' add_checklist(
#'   items = c(
#'     "libraries added",
#'     "loading data",
#'     "checking data"
#'   ),
#'   done = c(
#'     TRUE,
#'     TRUE,
#'     FALSE
#'   ),
#'   style = "compact"
#' )
add_checklist <- function(
    items,
    done = rep(
      FALSE,
      length(items)
    ),
    style = c(
      "dotted",
      "compact"
    ),
    width = 68L
) {
if (
  !is.character(items) ||
    length(items) == 0L ||
    anyNA(items) ||
    any(!nzchar(trimws(items)))
) {
    stop(
      "'items' must be a non-empty character vector without blank values.",
      call. = FALSE
    )
  }

  if (
    !is.logical(done) ||
      length(done) != length(items) ||
      anyNA(done)
  ) {
    stop(
      "'done' must be a logical vector matching the length of 'items'.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(width) ||
      length(width) != 1L ||
      is.na(width) ||
      !is.finite(width) ||
      width %% 1 != 0 ||
      width < 20
  ) {
    stop(
      "'width' must be a single finite integer of at least 20.",
      call. = FALSE
    )
  }

  style <- match.arg(style)
  width <- as.integer(width)

  task_labels <- sprintf(
    "[TASK %d]",
    seq_along(items)
  )

  if (
    style == "dotted"
  ) {
    status_labels <- ifelse(
      done,
      "[✓]",
      "[ ]"
    )

    prefix_width <- nchar(
      task_labels,
      type = "width"
    ) +
      2L

    item_width <- nchar(
      items,
      type = "width"
    )

    status_width <- nchar(
      status_labels,
      type = "width"
    )

    dot_width <- width -
  prefix_width -
  item_width -
  status_width -
  2L

if (any(dot_width < 1L)) {
  stop(
    "'items' are too long for the requested width.",
    call. = FALSE
  )
}
    output <- paste0(
      task_labels,
      ":",
      strrep(
        " ",
        1L
      ),
      strrep(
        ".",
        dot_width
      ),
      " ",
      items,
      " ",
      status_labels
    )
  } else {
    status_labels <- ifelse(
      done,
      "✓",
      "☐"
    )

    output <- paste0(
      status_labels,
      " ",
      task_labels,
      " - ",
      items
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
#' Add a formatted key-value list to R output
#'
#' Creates aligned left-hand labels and right-hand values separated by
#' a dotted leader. Blank left-hand labels can be used for continuation
#' rows.
#'
#' @param left Character vector containing the left-hand labels.
#' @param right Character vector containing the right-hand values.
#' @param width Target output width.
#'
#' @return The formatted lines, invisibly.
#' @export
#'
#' @examples
#' add_key_list(
#'   left = c(
#'     "[MODEL]:",
#'     "[GROUPS]:",
#'     "[CONTRASTS]:",
#'     "[STRATA]:",
#'     "",
#'     "[PAIRS]:"
#'   ),
#'   right = c(
#'     "LINEAR MIXED EFFECTS MODEL",
#'     "AMD v Control",
#'     "EMMEANS",
#'     "COMORBIDITY: TRUE",
#'     "COMORBIDITY: FALSE",
#'     "endo vs control"
#'   )
#' )
add_key_list <- function(
    left,
    right,
    width = 68L
  ) {
  if (
    !is.character(left) ||
      length(left) == 0L ||
      anyNA(left)
  ) {
    stop(
      "`left` must be a non-empty character vector without NA values.",
      call. = FALSE
    )
  }

  if (
    !is.character(right) ||
      length(right) != length(left) ||
      anyNA(right)
  ) {
    stop(
      "`right` must be a character vector matching the length of `left`.",
      call. = FALSE
    )
  }

  if (
    length(width) != 1L ||
      !is.numeric(width) ||
      is.na(width) ||
      !is.finite(width) ||
      width %% 1 != 0 ||
      width < 20
  ) {
    stop(
      "`width` must be a single finite integer of at least 20.",
      call. = FALSE
    )
  }

  width <- as.integer(width)

  left_widths <- nchar(
    left,
    type = "width"
  )

  right_widths <- nchar(
    right,
    type = "width"
  )

  label_width <- max(
    left_widths
  )

  if (
    any(
      label_width +
        1L +
        right_widths >
        width
    )
  ) {
    stop(
      "At least one left/right pair is longer than `width`.",
      call. = FALSE
    )
  }

  dot_widths <- width -
    label_width -
    right_widths -
    1L

  output <- paste0(
    left,
    strrep(
      " ",
      label_width - left_widths
    ),
    strrep(
      ".",
      dot_widths
    ),
    " ",
    right
  )

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
