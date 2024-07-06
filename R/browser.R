#' @export
browser_hook_sync_debugger <- function(debuggee) {
  function(hook, condition, envir) {
    # disallow auto-termination of option
    options(browser.hook = hook)

    # ignore the hook until we exit
    hook <- options(browser.hook = NULL)
    on.exit(options(browser.hook = hook, add = TRUE))

    cat("[enter browser]\n")
    repeat {
      tryCatch(
        {
          val <- browser("", condition, envir, 0L)
          return(val)
        },
        finally = cat("[exit browser]\n")
      )
    }
  }
}
