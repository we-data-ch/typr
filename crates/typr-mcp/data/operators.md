---
description: "Every operator in TypR and its precedence rules."
---

# Operators & Precedence

This page covers all operators in TypR and their precedence rules.

<!-- truncate -->

## Inventory

Everything in this section is **generated** from the compiler's syntax manifest —
the same single source of truth the editor grammars are built from. An operator
that is not listed here does not exist in TypR; see
[Where these tables come from](#where-these-tables-come-from).

<!-- BEGIN GENERATED operators — généré depuis syntaxes/typr.syntax.json par scripts/gen-syntax-reference.mjs ; ne pas éditer à la main -->

### Precedence

From strongest (evaluated first) to weakest. Member access and the pipe bind **more tightly** than arithmetic, unlike most languages.

| Rank | Operators | Role |
|---|---|---|
| 4 (strongest) | `as!` `in` `\|>` `::` `$` `.` | member access / UFCS, pipe, membership, validating cast |
| 3 | `*` `/` `%` | multiplicative |
| 2 | `+` `-` | additive |
| 1 (weakest) | `and` `or` `==` `!=` `<=` `>=` `<` `>` `&&` `\|\|` `&` `\|` `%op%` | comparison, logical, custom operators |

Only infix operators have a precedence. `<-`, `->`, `=`, `...` and `;` never take part in a binary expression, so they appear in the tables below and not here.

### Binding, arrows and separators

| Token | Meaning |
|---|---|
| `<-` | Binds, in `let` and `type`. Never an operator inside an expression. |
| `->` | Return type of a function type: `(x: int) -> int`. |
| `=>` | Arm separator in a `match`. |
| `=` | Named-field and default-value separator (`greeting: char = "Hi"`). Never a comparison — that is `==`. |
| `;` | Ends an instruction. Omitting it is tolerated with a warning, except on the last expression of a block. |
| `,` | Separates arguments, fields and type parameters. |
| `:` | Type annotation (`x: int`), and the range operator (`1:10`, `1:2:10`). |

### Access and pipe

| Operator | Meaning |
|---|---|
| `::` | Module member access. A historical alias of `$`, which it parses to. |
| `$` | Record field and module member access. |
| `.` | UFCS call `x.f(y)` ≡ `f(x, y)`, and positional tuple access `t.1` (1-based). |
| `\|>` | Pipe: `x \|> f()` ≡ `f(x)`. |
| `\(x) ...` | R 4.1's lambda shorthand. |

### Arithmetic

| Operator | Meaning |
|---|---|
| `+` | Addition; also type-level arithmetic on indices (`type Combined <- A + B;`). |
| `-` | Subtraction, and unary minus. |
| `*` | Multiplication. |
| `/` | Division. |
| `%` | Modulo. |

### Comparison and logic

| Operator | Meaning |
|---|---|
| `==` | Equality. |
| `!=` | Inequality. |
| `<=` | Less than or equal. |
| `>=` | Greater than or equal. |
| `<` | Less than. |
| `>` | Greater than. |
| `&&` | Logical *and* (scalar), spelled `and` in words. |
| `\|\|` | Logical *or* (scalar), spelled `or` in words. |
| `&` | Vectorized *and*, as in R. |
| `!` | Negation as a prefix; postfixed to an expression (`x!;`) it is the mutation sugar. |
| `\|` | Union of types (`.A(int) \| .B`), and the vectorized *or* of R. |

### Cast and word operators

| Operator | Meaning |
|---|---|
| `as!` | Validating cast: calls the generated `validate_T(x)` at runtime. |
| `as` | Renames on import: `import Math as M;`, `use Math::{sin as s};`. Never a cast — that is `as!`. |
| `and` | Logical *and*, word spelling of `&&`. |
| `or` | Logical *or*, word spelling of `\|\|`. |
| `in` | Membership. Iterates in `for (x in xs)`, and refines in the conditional type `T if T1 in T2`. |

### Spread and blocks

| Token | Meaning |
|---|---|
| `...` | Runtime spread and variadic parameter. |
| `..` | Nominal spread, in a record literal: `Point:{ ..source, x = 1 }`. |
| `@{` | Opens a vectorized block, `@{ ... }@`. |
| `}@` | Closes a vectorized block. |
| `%op%` | R-style custom infix operator, declared with a backquoted name: `` `%+%` ``. |

<!-- END GENERATED operators -->

---

## UFCS — `.` and `|>`

TypR supports the **Uniform Function Call Syntax**: `x.f(y)` is equivalent to `f(x, y)`.

```typr noplayground
x.f(y)            # ≡ f(x, y) — method-style call
x |> f() |> g()   # pipe — same desugaring
t.1                # positional tuple access (1-based index)
mod$member         # record field / module access — "::" is a historical alias for "$"
```

### Comparison with R

```typr
# --- setup ---
let data <- [1, 2, 3, -4];

# TypR — filter takes a real function, not a captured expression
let r <- data |> filter(fn(x: int): bool { x > 0 }) |> mean();
```
```r
# R — dplyr captures the expression (NSE)
data |> filter(x > 0) |> mean()
# or with magrittr: data %>% filter(...) %>% mean()
```

---

## Validating cast

```typr
# --- setup ---
type Point <- list { x: int, y: int };
let p <- Point:{ x = 1, y = 2 };
let xs <- [1, 2, 3];
# ---------------

p as! Point;                # calls validate_Point(p) at runtime
xs as! [Any, int];          # cast to an inline structural type (not an alias)
```

---

## Ranges

```typr
1:10;       # ≡ seq(1, 10, 1)
1:2:10;     # ≡ seq(1, 10, 2) — step in the middle
```

---

## Removed operators

The following doubled operators were removed from the tokenizer: `++ -- ** // %% @@ .. $$ |>>`, as well as `@`/`@@`/`=` in infix position. None had typing/transpilation branches or a stdlib `` `op` `` signature to support them. A stray `//` (common C-style comment mistake) is now recognized by a dedicated parser and treated as a valid comment (see [Known Pitfalls](../concepts/known-pitfalls)).

There is no exponentiation operator: `^` is the String kind sigil and nothing else. `@` is not a matrix product either — it only ever appears as the prefix of an annotation (`@pub`, `@export`) or of the Interface kind sigil, and in the `@{ ... }@` vectorized block.

---

## Arithmetic on types

```typr noplayground
type Combined <- A + B;      # Type::Operator on indices/dimensions
T if T1 in T2                 # conditional type (experimental refinement)
```

---

## Where these tables come from

The inventory above is derived from
[`components/syntax/mod.rs`](https://github.com/we-data-ch/typr/blob/main/crates/typr-core/src/components/syntax/mod.rs)
in the compiler, via `typr syntax --json`. Hand-maintained operator tables are
exactly how six copies of TypR's syntax drifted apart before the manifest
existed — this one listed `@` as a matrix product long after the tokenizer had
dropped it.

Two things are *not* in the manifest and stay on this side: the one-line meaning
of each operator, and the precedence ranks, which live in
`Op::get_binding_power`. Both are kept in `scripts/syntax-glossary.mjs`, keyed by
lexeme, and CI fails when a key has no matching lexeme in the manifest — so an
operator TypR no longer has cannot keep a row in the table above.
