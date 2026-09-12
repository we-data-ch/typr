---
description: "A side-by-side comparison of R and TypR."
---

# TypR vs R — What Really Changes

A side-by-side comparison of R and TypR.

<!-- truncate -->

| Aspect | R (classic) | TypR |
|--------|------------|------|
| End of instruction | newline is sufficient | `;` expected (parser warning if missing) |
| Typing | dynamic, no annotations | static; `fn(...)` always requires a return type |
| Name casing | free convention | enforced by parser: `snake_case` for `let`, `PascalCase` for `type`/aliases |
| Lists | `list(a = 1, b = 2)`, no validation | `list{a=1, b=2}` generates a real constructor + validator (`as.T`, `validate_T`) |
| Sum types | no native type — ad-hoc `class` conventions | tags `.A(T) \| .B` + exhaustive `match` |
| Method dispatch | `UseMethod`/S3, or `$` on R6/environment objects | general UFCS: `x.f(y)` ≡ `f(x, y)` for *any* type |
| Interfaces | no formal notion | `interface { ... }` + structural validator `I(x)` at compile time |
| Pattern matching | `switch()`, barely typed | `match` with tag/type/record/tuple/wildcard patterns |
| Modules | packages / `local()` / ad-hoc environments | `module M { ... }` → R environment, explicit `@pub` visibility |
| Generics | none (dynamic S3 dispatch) | type parameters `T`, kind sigils (`%R @I ^S ?B #N`) |
| Mutation | `x <- f(x)` explicit | `expr!;` sugar + `State` for real shared mutation |
| Output | R directly executed | transpiles to idiomatic R (`R/*.R`) — 100% of generated R is readable and executable as-is |

---

## Key takeaway

> TypR is not a new runtime — it is a layer of static verification and syntactic sugar
> (UFCS, tags, match, interfaces, modules, generics) that fully desugars into
> conventional R before execution. None of this exists at R runtime; everything is
> resolved by the TypR compiler.

---

## Migration effort

TypR is designed for **gradual adoption**:

- Start with a single `.ty` file in your existing R package
- Call R functions from TypR using `@` signatures
- Write new functions in TypR incrementally
- Generated R code is standard — CRAN, devtools, testthat all work unchanged

See [Compatibility with R](r-typr.md) for a detailed walkthrough.
