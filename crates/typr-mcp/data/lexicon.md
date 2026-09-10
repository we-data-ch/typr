---
description: "The fundamental tokens and literal types of TypR."
---

# Lexicon & Literals

This page covers the fundamental tokens and literal types in TypR.

<!-- truncate -->

## Semicolons

Every top-level instruction ends with `;`. The parser tolerates missing semicolons (emitting a `SyntaxError::ForgottenSemicolon` warning), except for the **last expression in a block**, which acts as an implicit return value — just like in R.

```typr
let x <- 42;        # explicit semicolon
let y <- 42         # tolerated, warning emitted
```

---

## Literals

| Literal | Syntax | Note |
|---------|--------|------|
| Integer | `42`, `-7` | `Lang::Integer` |
| Number | `3.14`, `-0.5` | a decimal point is **required** for `Number`; otherwise it's an `Integer` |
| String | `"text"` or `'text'` | single and double quotes are interchangeable; escapes: `\" \' \\ \n \t` |
| Boolean | `true` / `TRUE`, `false` / `FALSE` | both lowercase and uppercase forms accepted |
| Null | `null` / `NULL` | `Lang::Null` — distinct from `NA` |
| Missing | `na` / `NA` | `Lang::NA` — distinct from `null` |

### Literal types

Literals can also appear as **types** (singleton types): `3`, `3.14`, `true`, `"chat"` are valid types, more precise than `int`/`num`/`bool`/`char`.

---

## Identifiers

| Kind | Convention | Example | Enforcement |
|------|-----------|---------|-------------|
| Variable | `snake_case` | `my_var` | must start with `a-z` or `_` |
| Type | `PascalCase` | `MyType` | **required** for `type`/`opaque`/alias declarations |
| Quoted | backticks | `` `+` ``, `` `weird name` `` | useful for naming custom operators |

The parser explicitly rejects casing mistakes:

- `let PascalCase <- ...` triggers `LetInsteadOfType`
- `type snake_case <- ...` triggers `TypeInsteadOfLet`

---

## Comments

```typr
# This is a comment
```

Only `#` is recognized. There is **no** `//` syntax — a `.ty` file containing `//` will fail silently (see [Known Pitfalls](../concepts/known-pitfalls)).

---

## Reserved words

Everything below is **generated** from the compiler's syntax manifest, the same
single source of truth the editor grammars are built from. A keyword that is not
in this list is not a keyword: it is an ordinary identifier. See
[Where these tables come from](#where-these-tables-come-from).

<!-- BEGIN GENERATED lexicon — généré depuis syntaxes/typr.syntax.json par scripts/gen-syntax-reference.mjs ; ne pas éditer à la main -->

### Control-flow keywords

| Keyword | Meaning |
|---|---|
| `if` | Conditional. An expression, not a statement — it returns the value of the taken branch. |
| `else` | Alternative branch of an `if`; chains as `else if`. |
| `match` | Exhaustive pattern match over tags, types, records, tuples and `_`. |
| `for` | Iteration over a collection: `for (item in items) { ... };`. |
| `while` | Loop while a condition holds. |
| `loop` | Unconditional loop, left with `break`. |
| `break` | Leaves the innermost loop. |
| `next` | Skips to the next iteration. The R spelling — TypR has no `continue`. |
| `return` | Early return. The last expression of a block is already its value, so it is rarely needed. |

### Declaration keywords

| Keyword | Meaning |
|---|---|
| `let` | Binds a value. The name must be `snake_case`. |
| `fn` | Typed function literal — the return type is mandatory: `fn(x: int): int { ... }`. |
| `function` | R's untyped function form. Accepted, and typed as `UnknownFunction`. |
| `type` | Transparent type alias. The name must be `PascalCase`. |
| `opaque` | Opaque alias: the underlying type is hidden from callers. |
| `typeconstructor` | Registers a generic record/recursive constructor: `typeconstructor Tibble[N] record;`. |
| `recursive` | Kind of a `typeconstructor` whose parameters may recur: `typeconstructor Matrix[N, M, T] recursive;`. |
| `interface` | Structural capability: any type carrying the listed functions satisfies it. |
| `record` | Record literal type (`record { x: int }`), and the record kind of a `typeconstructor`. |
| `object` | Third spelling of the record literal type, alongside `list { ... }` and `record { ... }`. |
| `module` | Declares a module, transpiled to an R environment. |
| `mod` | Pulls in a module held in another file: `mod utils;`. |
| `import` | Imports a module as a whole: `import Math;`, `import Math as M;`. |
| `use` | Four grammars in one keyword: `use M::f;`, `use M::{f, g as h};`, `use M::*;` and the legacy R adapter `use("dplyr", c("filter"));`. |
| `extern` | Opens a raw R body whose signature TypR checks: `extern (x: int) -> int r#"..."#`. |
| `embed` | Named type embedding on a record field: `list { embed coords: Position }`. A soft keyword — a field genuinely named `embed` still parses. |

### Block heads

| Form | Meaning |
|---|---|
| `R { ... }` | Escape hatch: a block of raw R, left untouched by the transpiler. |
| `JS { ... }` | Escape hatch for the JavaScript target. |
| `Test { ... }` | Test block, transpiled to testthat. `Test[...]` is the file-level form. |

### Annotations

| Annotation | Meaning |
|---|---|
| `@export` | Exports the binding from the generated R package (roxygen2 `@export`). |
| `@pub` | Makes a module member visible outside its `module`. |
| `@testable` | Exposes a private member as `M$.test_<name>` under `typr build --test` only. |
| `@extern` | Declares an R function that already exists: `@extern stats::sd: (x: [Any, num]) -> num;`. |
| `@importFrom` | Hoists a roxygen2 `@importFrom`: `@importFrom dplyr filter select;`. |

### Constants

| Constant | Meaning |
|---|---|
| `true` | Boolean truth. `TRUE` is the R spelling of the same value. |
| `TRUE` | R spelling of `true`. |
| `false` | Boolean falsity. `FALSE` is the R spelling of the same value. |
| `FALSE` | R spelling of `false`. |
| `null` | Absence of a value (R's `NULL`) — distinct from `na`. |
| `NULL` | R spelling of `null`. |
| `na` | Missing value (R's `NA`) — distinct from `null`. |
| `NA` | R spelling of `na`. |

### Primitive types

| Type | Meaning |
|---|---|
| `int` | Integer. |
| `num` | Floating-point number. |
| `char` | Character string. |
| `bool` | Boolean. |
| `logic` | Accepted alias of `bool`. |
| `Any` | Top type — every value satisfies it. |
| `Empty` | Bottom type — no value satisfies it. The return type of a side-effect-only function. |
| `Self` | Inside an `interface`, the type that implements it. |

### Built-in type names

| Type | Meaning |
|---|---|
| `Vec[...]` | Native R vector: `Vec[num]`, `Vec[#N, num]`. |
| `Array[...]` | S3 array with an indexed size: `Array[3, int]`. Short form: `[#N, int]`. |
| `Tuple[...]` | Positional tuple: `Tuple[int, char]`, variadic `Tuple[T..., U]`. |
| `Record[...]` | Bracket record type: `Record[name: char]`, variadic `Record[Fs..., id: int]`. |
| `UnknownFunction` | The type given to an R function TypR knows nothing about. |
| `dataframe[...]{...}` | Data frame with typed columns: `dataframe[#N]{ name: char }`. |
| `data.frame` | R's own data-frame name, accepted as a type. |
| `data__frame` | The `__` spelling of `data.frame` — `__` becomes `.` in the emitted R. |
| `list { ... }` | Record literal type: `list { x: int, y: int }`. |
| `tuple { ... }` | Tuple literal type: `tuple { int, char }`. |
| `df[...]{...}` | Short spelling of `dataframe`. |

### Built-in constructors

| Form | Meaning |
|---|---|
| `c(...)` | R's vector constructor. |
| `seq[...]` | Sequence literal. The range sugar `1:10` desugars to the R `seq(1, 10, 1)`. |
| `Class(...)` | Type denoting an existing R class: `Class("data.frame", "tbl")`. |
| `library(...)` | Declares an R package dependency, as in R. |

### Kind sigils

A sigil prefixes a single-uppercase-letter generic to fix its kind.

| Example | Kind | Sigil |
|---|---|---|
| `#N` | Number | `#` |
| `%R` | Record | `%` |
| `@I` | Interface | `@` |
| `^S` | String | `^` |
| `?B` | Boolean | `?` |
| `$L` | Label | `$` |

Reserved for future kinds and **not parsed today**: `~`, `&`, `!`. Each is already a live operator, so writing one as a sigil does not do what it looks like.

<!-- END GENERATED lexicon -->

---

## Where these tables come from

TypR's lexemes are described once, in the compiler
([`components/syntax/mod.rs`](https://github.com/we-data-ch/typr/blob/main/crates/typr-core/src/components/syntax/mod.rs)),
and every grammar is generated from that manifest — VS Code, Vim, the
playground, and this site's own syntax colouring. The tables above are generated
from the very same file, so a keyword cannot exist in the compiler and be
missing here, nor survive here after the language drops it: the check runs in
CI, in both directions.

What the manifest does *not* hold is what each lexeme **means**. Those one-line
glosses live in this repository (`scripts/syntax-glossary.mjs`), and adding a
keyword to TypR without writing its line of documentation fails this site's
build.
