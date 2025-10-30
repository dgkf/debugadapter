# Manual `neovim` testing

We've built a custom image that bundles the devel version of `R`, built with
the necessary flags, and `neovim` to make it easier and more consistent to
test.

```sh
podman-compose -f ./tests/fixtures/containers/neovim/compose.yaml build
podman-compose -f ./tests/fixtures/containers/neovim/compose.yaml run neovim ./R/utils.R +":set splitbelow" +":split term://R"
```

Launching the `neovim` container will do two things.

1. It will open up `R/utils.R`, which provides a few standalone functions which
   make great candidates for testing the debugger.
2. It will launch an R session in a local terminal.




