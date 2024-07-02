#' @export
dap_browser_hook <- function(hook, condition, envir) {
  options(browser.hook = hook)

  cat(
    sep = "",
    "[enter browser]\n",
    "  env:\n",
    paste0(" ", capture.output(str(as.list(envir)))[-1], "\n"),
    "  TODO:\n",
    "    * emit state change: stopped\n"
  )

  # ignore the hook until we exit
  hook <- options(browser.hook = NULL)
  # on.exit(options(browser.hook = hook, add = TRUE))

  repeat {
    tryCatch(
      {
        val <- browser("", condition, envir, 0L)
        return(val)
      },
      finally = {
        cat(
          sep = "",
          "[exit browser]\n",
          "  TODO:\n",
          "    * emit state change: executing\n",
          "    * listen for breakpoint updates\n"
        )
      }
    )
  }
}
