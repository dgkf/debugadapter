.onLoad <- function(libname, pkgname) {
  if (!has_browser_hook()) warn_no_browser_hook()
  options(browser.hook = browser_hook)
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
      eval(quote(browser(ignoreHook = TRUE)), globalenv())
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Emit a warning that necessary browser hook feature is not available
#'
#' @return Used for side effect of emitting a warning
#'
warn_no_browser_hook <- function() {
  warning(
    call. = FALSE,
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
