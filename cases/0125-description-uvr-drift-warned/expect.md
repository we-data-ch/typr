The project's `DESCRIPTION` lists `jsonlite` under `Imports` but `uvr.toml` does not, and
`TypR/main.ty` calls `library(ggplot2)` with `ggplot2` declared nowhere. `typr check` still
succeeds (drifts are warnings) but prints one warning for each, suggesting `typr use <pkg>`.
`devtools`/`usethis` sit in `[dev-dependencies]` only (tooling) and must not be reported.

Code: `crates/typr-cli/src/r_manifest.rs` (pure `diagnose`), called from `project::check_project`
and `check_file`. Projects without `uvr.toml` are not checked.
