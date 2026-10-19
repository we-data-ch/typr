`int` never holds `NA` (rfcs/0000-compilation-targets.md, §2). R's `+`/`-`/`*` turn
`2147483647L + 1L` into `NA` with a warning, which broke that invariant on the `r` target.
`Ops.Integer` in `std.R` now raises `integer overflow` instead. The first `print`s (`10`,
`2147483646`) must still work; the last statement stops the program.
Fixed in `crates/typr-cli/configs/src/std.R` (`Ops.Integer`), mirrored in `typr-mcp/data/std.R`.
