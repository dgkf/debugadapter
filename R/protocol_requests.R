request <- function(command, arguments) {
  req <- list(type = "request", command = command)

  if (!missing(arguments))
    req$arguments <- arguments

  class(req) <- c("reverse_request", command, class(req))
  req
}

request_run_in_terminal <- function(cmd, path = ".") {
  log(DEBUG, "requesting runInTerminal")
  cwd <- find_package_root(path)

  args <- list(
    # kind?:
    # title?:
    # env?:
    # argsCanBeInterpretedByShell?:
    cwd = cwd,
    args = cmd
  )

  request("runInTerminal", args)
}
