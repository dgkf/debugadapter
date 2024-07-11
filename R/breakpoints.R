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
breakpoint_verify <- function(
    breakpoint,
    locations = locations_from_line(breakpoint$source$path, breakpoint$line)) {

  if (length(locations) < 1) {
    breakpoint$verified <- FALSE
    breakpoint$reason <- "failed"
    breakpoint$message <- paste0(capture.output(locations), collapse = " ")
  } else {
    # if we have even just a single location, then the source is known
    # and we can verify the breakpoint
    location <- locations[[1]]
    start_end <- line_code_span(location$filename, location$line)
    breakpoint$verified <- TRUE
    breakpoint$reason <- NULL
    breakpoint$line <- location$line
    breakpoint$column <- start_end[[1]]
    breakpoint$endColumn <- start_end[[2]]
    breakpoint$source$name <- basename(location$filename)
    breakpoint$source$checksum <- checksum(
      algorithm = "MD5",
      checksum = unname(tools::md5sum(location$filename))
    )
  }

  breakpoint
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
locations_from_line <- function(path, line) {
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

breakpoint_locations <- function(breakpoint) {
  locs <- locations_from_line(breakpoint$source$path, breakpoint$line)
  locs <- lapply(locs, `[[<-`, "id", breakpoint$id)
  locs
}

breakpoints_parse_as_pending <- function(args, ids) {
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

location_trace <- function(location) {
  obj <- get0(location$name, location$env, inherits = FALSE)
  if (is.null(obj)) return(FALSE)
  # TODO(r-bug): ideally would use condition parameter to pass locations, but
  # seems broken?
  debug(obj)
  TRUE
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
