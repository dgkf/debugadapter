.onLoad <- function(libname, pkgname) {
  session_has_browser <- has_browser_hook()
  child_has_browser <- has_child_browser_hook()

  if (!session_has_browser) {
    packageStartupMessage(message_no_browser_hook())
  } else if (!child_has_browser) {
    packageStartupMessage(message_no_child_browser_hook())
  }
}

#' Tests whether the current version of R has recent changes to the browser
#'
#' @note Specifically, tests whether argument `ignoreHook` is accepted by
#'   [`browser()`]. If this feature is reverted, this function will need to
#'   be updated to respond to changes in devel.
#'
#' @return `logical[1L]` indicating whether R is of a satisfactory version
#'
has_browser_hook <- function() {
  tryCatch(
    {
      eval(quote(browser(expr = FALSE, ignoreHook = TRUE)), globalenv())
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

has_child_browser_hook <- function() {
  tryCatch(
    {
      callr::r(
        function(pkg, ...) {
          # make sure we don't recursively spawn child processes
          if (!is.na(Sys.getenv("CALLR_IS_RUNNING"))) {
            return(TRUE)
          }
          getNamespace(pkg)[["has_browser_hook"]]()
        },
        args = list(pkg = packageName())
      )
    },
    error = function(e) {
      FALSE
    }
  )
}

message_no_browser_hook <- function() {
  paste0(
    paste0(collapse = "\n", strwrap(paste0(
      packageName(),
      " requires a version of R with `option(browser.hook)`, ",
      "currently only available in development versions of R ",
      "(svn rev >= 85116) with the browser hook enabled through a feature ",
      "flag:"
    ))),
    "\n\n",
    "  $ svn checkout https://svn.r-project.org/R/trunk R\n",
    "  $ ./R/tools/rsync-recommended\n",
    "  $ ./configure CPPFLAGS=-DUSE_BROWSER_HOOK\n",
    "  $ ./make",
    "\n"
  )
}

message_no_child_browser_hook <- function() {
  paste0(
    "Subprocesses spawned using `callr` are not using a version of R ",
    "that has browser hooks enabled. ",
    packageName(),
    " will not work properly when using background processes."
  )
}
