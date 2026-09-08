<div align="center">

# TypR

**A typed superset of R** — static type checking, modules, sum types and
interfaces, compiled to idiomatic, readable R.

[![Downloads](https://img.shields.io/crates/d/typr)](https://crates.io/crates/typr)
[![Latest release](https://img.shields.io/github/v/release/we-data-ch/typr)](https://github.com/we-data-ch/typr/releases/latest)
[![CI](https://github.com/we-data-ch/typr/actions/workflows/ci.yml/badge.svg)](https://github.com/we-data-ch/typr/actions)
[![Docker Pulls](https://img.shields.io/docker/pulls/fabricehategekimana/typr)](https://hub.docker.com/r/fabricehategekimana/typr)
[![Open VSX](https://img.shields.io/open-vsx/v/wedata-ch.typr-language)](https://open-vsx.org/extension/wedata-ch/typr-language)
[![License](https://img.shields.io/github/license/we-data-ch/typr)](LICENSE)

[Documentation](https://we-data-ch.github.io/typr.github.io/) ·
[Playground](https://we-data-ch.github.io/typr-playground.github.io/) ·
[Blog](https://we-data-ch.github.io/typr.github.io/blog)

</div>

---

TypR is not a new runtime. It is a static verification and desugaring layer that
compiles entirely to conventional R before execution — every generated `R/*.R`
file stays readable and runs on a stock R installation. It targets the step
beyond exploratory scripting: package production, maintenance, and code that has
to survive in production.

```typr
type Shape <- .Circle(num) | .Square(num);

let area <- fn(s: Shape): num {
    match s {
        .Circle(r) => 3.14159 * r * r,
        .Square(side) => side * side,
    }
};

# UFCS, pipes and plain calls are all equivalent
(3.0) |> .Circle() |> area() |> print();
```

## Install

| Channel | Command |
|---|---|
| Cargo | `cargo install typr` |
| Binaries | [latest release](https://github.com/we-data-ch/typr/releases/latest) — Linux, macOS, Windows (x86_64 + aarch64) |
| Docker | `docker run --rm -it fabricehategekimana/typr:latest` |
| RStudio / Positron | `typr.runner_*.tar.gz` from the [latest release](https://github.com/we-data-ch/typr/releases/latest) |
| VS Code / Positron | search **TypR** in the Marketplace |

All channels are published from the same tag and carry the same version number.
See [RELEASING.md](RELEASING.md).

## Usage

```bash
typr new my_package     # scaffold a project
typr check              # type-check without emitting
typr build              # transpile to R/
typr run                # build and run
typr test               # run testthat suites
typr document           # generate .Rd documentation
typr pkgdown            # build a documentation website
typr repl               # interactive session
typr lsp                # language server (used by the editor extensions)
```

## Repository layout

```
crates/typr-core     type checking and transpilation
crates/typr-cli      command-line interface
crates/typr-lsp      language server
crates/typr-wasm     WASM build powering the playground
editors/vscode       VS Code / Positron extension
editors/rstudio      RStudio addins (typr.runner)
docker/              container image
cases/               reproducible bug catalog (`typr case`)
```

## Contributing

`cases/` holds a numbered, reproducible catalog of past bugs with golden
outputs — the best entry point for understanding a behaviour is usually the case
that pinned it down. `cargo test --workspace` runs the suite.

## License

Apache-2.0
