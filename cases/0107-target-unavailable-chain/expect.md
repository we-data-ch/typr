`typr.toml` declares `targets = ["r", "rust"]`. `main` calls `load`, which calls `summarise`,
which contains an `R { ... }` block. `typr check` must reject the project on `rust` with T052,
pointing at the use in `main` and spelling out the chain down to the R block
(rfcs/0000-compilation-targets.md §4). On `r` alone the same project is fine.

Code: `crates/typr-core/src/processes/availability.rs`.
