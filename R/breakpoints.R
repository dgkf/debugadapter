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
verify_breakpoint <- function(breakpoint) {
  ln <- get_breakpoint_locations(breakpoint$source$path, breakpoint$line)

  if (length(ln) < 1) {
    breakpoint$verified <- FALSE
    breakpoint$reason <- "failed"
    breakpoint$message <- paste0(capture.output(ln), collapse = " ")
    return(breakpoint)
  } else {
    # TODO: handle multiple locations for a single breakpoint
    ln <- ln[[1]]
    start_end <- line_code_span(ln$filename, ln$line)
    breakpoint$verified <- TRUE
    breakpoint$reason <- NULL
    breakpoint$line <- ln$line
    breakpoint$column <- start_end[[1]]
    breakpoint$endColumn <- start_end[[2]]
    breakpoint$source$name <- basename(ln$filename)
    breakpoint$source$checksum <- checksum(
      algorithm = "MD5",
      checksum = unname(tools::md5sum(ln$filename))
    )

    breakpoint
  }
}

get_breakpoint_locations <- function(path, line) {
  # TODO:
  #   will fail to verify for files with unknown source. This could be
  #   standalone scripts or packages not yet installed.
  #
  # for scripts:
  #   - should source the file
  #   - maybe consider isolating only the code for that object to avoid
  #     long-running code unrelated to the breakpoint?
  #
  # for packages:
  #   - ideally error so that we don't need to manage package installation

  pkg <- find_package_name(path)
  envir <- if (is.null(pkg)) globalenv() else getNamespace(pkg)
  utils::findLineNum(path, line, envir = envir)
}

as_pending_breakpoints <- function(args, ids) {
  mfapply(
    breakpoint,
    id = ids,
    verified = FALSE,
    source = list(args$source),
    line = if (is.null(args$breakpoints)) {
      args$lines  # deprecated
    } else {
      vnapply(args$breakpoints, `[[`, "line")
    },
    reason = "pending"
  )
}

breakpoint_key <- function(breakpoint) {
  paste0(breakpoint$source$path, "#", breakpoint$line)
}
