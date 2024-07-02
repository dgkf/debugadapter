> [!IMPORTANT]  
> This project is in early development. It's probably not useful. These steps
> are only for confirming that your configuration is correct.

1. [Setup Guide](#setup-guide)
2. [Editor Configurations](#editor-configurations)
   1. [neovim](#neovim)
   2. [helix](#helix)

# Setup Guide

## Start the Server

Before we worry about a client, let's make sure we can start a server and begin
listening for a client to connect.

```r
options(debugadapter.log = 2) # enable logging at 'log level 2'
debugadapter::run()
#> [DEBUG] Starting background tcp server, awaiting DAP client ...
```

This launches a background process that will serve the debugger protocol. Both
our editor of choice and our R repl can be clients of this server.

## Configure Editor & Start Debugging

1. [neovim](#neovim)
2. [helix](#helix)

## Confirm

> [!NOTE]  
> Due to limitations in an active R REPL, communication with our R session
> always _lags by one top-level command_.

If everything is working properly, **the next command you execute** in your R
session should emit a flurry of debug statements starting with:

```r
#> [BG<pid22527>][DEBUG] Connection established
```

---

# Editor Configurations

## `neovim`

### Pre-requisites

* Install the [`nvim-dap`](https://github.com/mfussenegger/nvim-dap) neovim plugin

### Configure

In your `neovim` configuration, register the R debugger with `nvim-dap`

```lua
local dap = require('dap')
dap.adapters.r = function(cb, config)
  if config.request == 'attach' then
    ---@diagnostic disable-next-line: undefined-field
    local port = (config.connect or config).port
    ---@diagnostic disable-next-line: undefined-field
    local host = (config.connect or config).host or '127.0.0.1'
    cb({
      type = 'server',
      port = assert(port, '`connect.port` is required for a R `attach` configuration'),
      host = host,
      options = {
        source_filetype = 'r',
      },
    })
  else
    -- work-in-progress, execute/launch mode
    error("R DAP only configured for attach-mode")
  end
end

dap.configurations.r = {
  {
    type = 'r',
    request = 'attach',
    name = 'Attach to R process',
    host = "localhost",
    port = 18721
  }
}
```

### Start Debugging

In a new `neovim` session, hop into any `.R` file. You run `:DapContinue` to
attempt a connection with your R repl. Next, head to the section on
[confirming your setup](#confirm).

## `helix`

### Configure

In your [`languages.toml`](https://docs.helix-editor.com/languages.html)

```toml
[[language]]
name = "r"

[language.debugger]
name = "r debugger"
transport = "tcp"

[[language.debugger.templates]]
name = "attach"
request = "attach"
completion = [ ]
args = { }
```

### Start Debugging

Open a `.R` file in a new `helix` session and run 
`:debug-remote 127.0.0.1:18721`. If you don't immediately get an error saying
you've failed to connect, it's probably working. Next, head to the section on
[confirming your setup](#confirm).

