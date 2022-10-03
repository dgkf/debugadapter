#' Verify DAP sourceBreakpoint and return a Breakpoint
#'
#' Verifying a breakpoint provided by the client means searching for the line of
#' code within the source file and verifying that a breakpoint can be placed
#' there.
#'
#' @note
#' The DAP sourceBreakpoint can provide a column, though this is currently
#' unimplemented.
#'
#' `r spec("#Requests_SetBreakpoints")`
#' `r spec("#Types_SourceBreakpoint")`
#' `r spec("#Types_Breakpoint")`
#'
verify_breakpoint <- function(path, line, id = NULL) {
  pkg <- find_package_name(path)

  envir <- if (is.null(pkg)) globalenv() else getNamespace(pkg)
  ln <- utils::findLineNum(path, line, envir = envir)

  if (length(ln) < 1) {
    message <- paste0(capture.output(ln), collapse = " ")
    bp <- breakpoint(verified = FALSE, message = message)
  } else {
    bp <- find_line_num_result_to_breakpoint(ln[[1L]])
  }

  bp$id <- id
  bp
}

breakpoint_key <- function(path, lines) {
  paste0(path, "#", lines)
}

find_line_num_result_to_breakpoint <- function(ln) {
  start_end <- line_code_span(ln$filename, ln$line)

  breakpoint(
    verified = TRUE,
    source = source(
      name = basename(ln$filename),
      path = ln$filename,
      checksum = checksum(
        algorithm = "MD5",
        checksum  = unname(tools::md5sum(ln$filename))
      )
    ),
    line = ln$line,
    column = start_end[[1]],
    endColumn = start_end[[2]]
  )
}

set_breakpoints <- function(adapter, breakpoints, sourceModified, source, lines) {
  lines <- lapply(breakpoints, `[[`, "line")
  ids <- next_id(adapter, length(lines))
  bps <- mfapply(verify_breakpoint, line = lines, path = source$path, id = ids)
  keys <- vcapply(lines, breakpoint_key, path = source$path)

  adapter$breakpoints[keys] <- bps
  bps
}
