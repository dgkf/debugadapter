# `debugadapter` 

![developing](https://img.shields.io/badge/lifecycle-developing-orange)

An implementation of the [Debug Adapter
Protocol](https://microsoft.github.io/debug-adapter-protocol/) for R

> **Status**
> 
> Still heavily under development. Some of the examples provided here
> serve as targets for eventual user interfaces and are not yet 
> functional.

## Getting Started

> IDE-specific getting-started guides will come when the project is 
> more mature

`debugadapter` operates in one of two ways:

1. `execute` mode, where A file is used as a script for execution, in which 
   case `debugadapter` will launch a new debug server, register breakpoints and
   provide a debug `REPL` within your client if it provides the capability and
   allow you to step through the script.

2. `attach` mode, where your client will attach to a running server, running
   as a background process to an interactive session.

   ```r
   # start a server in the background
   debugadapter::run()  

   # execute some code which will hit a breakpoint and debug as usual
   my_code_to_debug()
   # debugging at my_nested_fn_call(...) #123
   # Browse[0]> 
   # ...
   ```

## Architecture

Attaching an interactive R session relies on the coordination of
multiple R processes:

1. **DAP Server**  
   A socket-based server used for communicating with the debug client. 
   When run in the background, the debugger state is synchronized after
   each top-level task and before entering a REPL-based debugger.

1. **R Session**  
   The interactive R session serves as the parent to the DAP server and
   the `browser()` REPL, allowing for iterative debugging in a persistent
   R session.

1. **forked `browser()` process**  
   When `browser()` would be called, it is instead launched in a forked
   process, which allows the interactive R session to step through the 
   debugger while also querying for various debugger context required
   by the debug client. Unfortunately, this means that this approach is
   currently not supported on Windows. 

### Debugging Loop

```mermaid
sequenceDiagram

participant ide as Client
participant dap as DAP Server
participant r   as REPL
participant b   as Forked Process

ide->>dap: attach DAP

rect rgba(128, 128, 128, 0.1)
    Note over dap,r: Task Callback
    r->>+dap: request sync debug state
    dap->>-r: reply sync debug state
end

r->>b: shadow_browser()

loop Debugger REPL
    b->>+r: stdout `Browse[1]>`

    rect rgba(128, 128, 128, 0.1)
        Note over ide,dap: Output suppressed
        r->>+b: request debug state
        b->>-r: reply debug state
        r->>dap: update server state
        dap->>ide: update client state
    end

    alt Client-based stepping
        ide->>dap: step request
        dap->>r: step signal
        r->>b: step signal
    else REPL-based stepping
        r->>-b: stdin
    end
end
```

This is all orchestrated using a web of connections between each 
of the processes. The exact layout is in flux as the project matures.

```mermaid
flowchart LR

repl[fa:fa-terminal R Session]
browser[Shadow Browser]
server[DAP Server]
client[DAP Client]

subgraph R Session
    server -->|<i>stdout</i> <br> debugger <br> breakpoint <br>messages| repl
    repl -->|"<i>stdin</i> <br> browser() commands"| browser
    browser -->|"<i>stderr</i> <br> browser() output"| repl
    browser -->|"<i>tcp socket</i> <br> stopped events"| server

    subgraph fork[Forked <br> Process]
        browser
    end

    subgraph bg[Background <br> Process]
        server
    end
end

subgraph IDE
    client
end

client --->|<i>tcp socket</i> <br> requests| server
server --->|"<i>tcp socket</i> <br> responses"| client
```

### The _Shadow_ Browser

The "shadow" browser ("shadow" here in the vein of the "shadow DOM"
in the web world - a mirror of the current, displayed environment 
for managing state) is a `dapr`-specific browser prompt, which
executes additional code in the background to synchronize debugger
state with a client. Since the `browser()` prompt is rather rigidly 
defined in the internals of the R language, the way this works is 
messy to say the least. 

When active, `stdin` and `stdout` are redirected to a child process.
User commands are sent back to the parent process's `stdin` and 
the output is sent back to the parent using `stderr` (using messages)
Additional calls are made to inspect the state of the debugger after
each step and to send this information to the client. These calls
are omitted from the output that is relayed back to the parent. 

A task callback is used to restore the original `stdin` and `stdout`, 
and to close all the connections used to communicate to the child
process once the browser state has returns to the top level.

## Prior Art

### [`vscDebugger`](https://github.com/ManuelHentschel/vscDebugger)

An existing implementation that originated as a VSCode extension. 
While that package seems to work well in VSCode, it has been on
a long path to being more editor agnostic. `debugadapter` deviates from 
the design choices of this package in two key ways: 

- A pure R package for simpler portability
- Prioritzing a use case where an arbitrary terminal can be 
  attached to the debug client and integrate with an interactive 
  R session.
