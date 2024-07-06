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
#' @importFrom tools md5sum
verify_breakpoint <- function(breakpoint) {
  ln <- get_breakpoint_locations(breakpoint$source$path, breakpoint$line)

  if (length(ln) < 1) {
    breakpoint$verified <- FALSE
    breakpoint$reason <- "failed"
    breakpoint$message <- paste0(capture.output(ln), collapse = " ")
    return(breakpoint)
  } else {
    # if we have even just a single location, then the source is known 
    # and we can verify the breakpoint
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

#' Find Source Locations based Breakpoint Location
#'
#' A single breakpoint may be best served by tracing multiple source code
#' locations. For example, functions loaded from a package are traced
#' separately from the namespaced function calls, resulting in multiple
#' traced locations based on a single source and line number.
#'
#' @param path A file path of the source code to trace
#' @param line The line number in the source file to trace
#'
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

trace_breakpoints <- function(breakpoints) {
  for (b in breakpoints) trace_breakpoint(b)
}

trace_breakpoint <- function(b) {
  locations <- get_breakpoint_locations(b$source$path, b$line)
  for (location in locations) {
    suppressMessages(trace(
      what = location$name,
      signature = location$signature,
      tracer = bquote({
        browser(
          condition = .(list(
            location = location,
            breakpoint = b            
          )),
          skipCalls = 4L
        )
      }),
      where = location$env,
      at = location$at,
      print = FALSE
    ))
  }
}

untrace_breakpoint <- function(b) {
  locations <- get_breakpoint_locations(b$source$path, b$line)
  for (location in locations) {
    suppressMessages(untrace(
      what = location$name,
      signature = location$signature,
      where = location$env
    ))
  }
}
