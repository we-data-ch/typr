`uvr.toml` pins `r_version = ">=4.3"` and no R is reachable (sandbox `PATH` empty, empty `HOME`,
so rig's `~/.local` layout is empty too). `typr check` must stop before checking anything, print
`R is not installed` and the fix `typr setup-r`, and exit 1. Code: `toolchain/rig.rs::require_project_r`,
called by `project.rs::enforce_r_constraint`. Without a pin, R is not required (not covered here).
