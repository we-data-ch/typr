- **Status:** draft
- **RFC PR:** we-data-ch/typr#0000
- **Tracking issue:** —
- **Implemented in:** Steps 1 to 6, refinement obligations and integer overflow on branch `rust-target` (not released)
- **Start date:** 2026-10-12

# Compilation targets: a portable core, Rust as a second target, and target availability checking

## Summary

TypR gains a notion of **compilation target**. `r` is the target that exists today and does not
change. `rust` is a new target: it compiles a TypR project to a Cargo project, which can then be
built as a native binary, a library, a `wasm32` module, or an R extension through extendr. Every
top-level definition gets an **availability set**, meaning the targets it can be compiled to. The
compiler computes that set from what the definition uses, the way Gleam does for Erlang and
JavaScript. Calling an R-only function from code built for `rust` is a compile-time error that
names the call chain. It is never a runtime surprise.

To make the second target possible, this RFC first states what a TypR program **means** without
reference to R: a portable core with its own semantics, implemented by each target. The rest
(data frames, raw `R { ... }`, untyped R functions, `Foreign<T>`, R classes) stays available on
the `r` target and is marked unavailable elsewhere.

This RFC settles the language and project contract: the semantics of the portable core, the
syntax for externals and target annotations, the availability rules and their error messages, and
the `typr.toml`/CLI surface. It sketches the Rust code generation enough to show that the core is
implementable. The emitted Rust itself is a build artifact, neither stable nor meant to be read:
the contract is the behaviour of the binary (§9).

## Motivation

A TypR project today produces R and nothing else. That is the right default for the audience,
since R users read the generated R and that is TypR's main argument. But it closes off three
things that keep coming up:

1. **Shipping the same logic outside R.** A validation library, a parser, or a scoring model
   written in TypR could also be a CLI, a WebAssembly module in a web page, or a standalone
   tool, without being rewritten in another language.
2. **Fast paths inside an R package.** The usual answer when R is too slow is to rewrite the hot
   function in C++ (Rcpp) or Rust (extendr). With a Rust target, the hot function stays in TypR
   and is compiled to native code, while the rest of the package stays plain R.
3. **A cross-platform standard library.** Today `std_R.ty` is a list of signatures backed by a
   hand-written `std.R`. Nothing records which of those functions *could* exist elsewhere.

None of these can be done by "also generating some Rust". The obstacle is that TypR's meaning is
currently defined by the R it produces. Two concrete examples, both verified on the current
`develop`:

```typr
let x <- [1, 2, 3] + [1, 2];   # accepted: R recycles the shorter vector (with a warning)
```

```typr
type Shape <- .Circle(num) | .Square(num);
let area <- fn(s: Shape): num { match s { .Circle(r) => 3.14 * r * r, .Square(c) => c * c } };
```

The first one means whatever R's recycling rule says. The second one compiles to an S3 method
`area.Shape` that is dispatched at runtime on the class attribute. Neither behaviour is written
down as TypR's own. A second target has to implement *something*, and without a spec it would
implement "whatever R does, approximately", which is the worst outcome: two targets that agree on
the easy cases and quietly diverge on the others.

The compiler already has some groundwork. `Config::target_language` exists with variants `R` and
`JS` (and, oddly, `JS` as the `#[default]`), together with a 12-line `std_JS.ty` and a
`JS { ... }` escape block. That attempt was never carried through, and it is the reason this RFC
puts semantics and availability first and code generation second.

## Guide-level explanation

### Targets

A TypR project declares the targets it builds for:

```toml
# typr.toml
[project]
targets = ["r", "rust"]      # default: ["r"], i.e. every existing project is unchanged
```

```bash
typr check                    # checks every declared target
typr check --target rust      # only one
typr build --target rust      # writes a Cargo project under target/rust/ and runs `cargo build`
typr build --target rust --platform wasm   # same, for wasm32-unknown-unknown
```

### Most code runs everywhere

Code written with TypR's own constructs (scalars, vectors, records, tags and `match`,
functions, generics, interfaces, modules) is available on every target without any annotation:

```typr
type Shape <- .Circle(num) | .Square(num);

let area <- fn(s: Shape): num {
    match s {
        .Circle(r) => 3.14159 * r * r,
        .Square(c) => c * c,
    }
};
```

This compiles to the same R as today on `r`, and to an `enum` plus a `fn` with a `match` on
`rust`.

### Some code is tied to a target, and the compiler knows it

```typr
let summarise <- fn(path: char): Foreign<Any> {
    R { readr::read_csv(path) |> dplyr::summarise(n = dplyr::n()) }
};

let main <- fn(): Empty {
    summarise("data.csv");
    print("done")
};
```

```
$ typr check --target rust
error[T052]: `main` is not available on target `rust`
  ┌─ main.ty:6:5
  │
6 │     summarise("data.csv");
  │     ^^^^^^^^^ `summarise` is only available on `r`
  │
  = note: `summarise` uses an `R { ... }` block (main.ty:2:5)
  = help: give `summarise` an implementation for `rust` with `@extern(rust = "...")`,
          or restrict `main` to R with `@target(r)`
```

You never write the availability of an ordinary function. The compiler infers it, and you only
see it when it gets in your way. Errors are reported **where a definition is used for a target**,
not where it is defined. A library can mix R-only helpers and portable functions, and a project
that only uses the portable ones builds for `rust` without complaint.

### One name, several implementations

An external function can have one implementation per target:

```typr
@extern(r = "stats::median", rust = "typr_std::stats::median")
median: (x: [#N, num]) -> num;
```

It can also have externals for some targets and a TypR body for the rest:

```typr
@extern(r = "base::rev")
let rev <- fn(xs: [#N, T]): [#N, T] {
    # portable fallback, used on every target without an extern
    ...
};
```

A function with a body is available everywhere its body is. An `@extern` entry adds a target
(or overrides the body for that target). An `@extern` without a body is available exactly on the
targets it names.

### Restricting a definition on purpose

`@target(...)` states an upper bound. It is a declaration of intent that the compiler checks:

```typr
@target(r)
let plot_results <- fn(df: dataframe[#N]{ x: num, y: num }): Empty { ... };
```

On a definition, `@target(r, rust)` means "this must stay portable". If someone later adds an
`R { ... }` block to the body, the error is reported on that definition rather than three call
sites away.

### `NA` is in the type, not in the value

The portable core takes the rule TypR already enforces and makes it binding on every target. A
plain `int`, `num`, `bool` or `char` **never** holds `NA`. A value that can be missing has `na`
in its type:

```typr
let safe_div <- fn(a: int, b: int): int | na {
    if (b == 0) { NA } else { a / b }
};

let r <- safe_div(6, 0);
let n: int <- r;          # error: Expected int (today already)
```

On `r`, nothing changes. On `rust`, `int` is an `i32` with no sentinel and `int | na` is an
`Option<i32>`. The type checker already guarantees that an `int` is never `NA`, so the Rust side
doesn't need to check for it.

### How to explain this to an R user

"Your TypR code is checked once and can then be compiled to R, which is what you know, or to
Rust, which gives you a binary, WebAssembly, or a fast path inside your package. Most code works
on both. When something only makes sense in R (a data frame, a raw `R { }` block, an untyped R
function), TypR tracks it. If you try to build a Rust binary that needs it, you get a compile
error pointing at the exact call, not a crash."

## Reference-level explanation

### 1. The target model

`Target ::= r | rust` (plus `js`, which keeps its current dormant status and is out of scope).

A **target** is a language with its own semantics implementation and its own standard-library
implementation. A **platform** is a way of packaging the output of one target:

| Target | Platforms | Output |
|---|---|---|
| `r` | `script` (default), `package` | `.R` files, as today |
| `rust` | `bin` (default), `lib`, `wasm`, `extendr` | a Cargo project, then `cargo build` |

Platforms do not influence availability, with one exception: `extendr` adds the **boundary
check** (§6). WebAssembly is a platform of `rust`, not a separate target, because the code is the
same Rust. A future `wasi`/`wasm` difference (no filesystem) is handled by capability externals
in the standard library, not by a new target. See *Unresolved questions*.

`TargetLanguage` in `components/context/config.rs` becomes this `Target`, and its default becomes
`R`.

### 2. The portable core: semantics independent of R

Each item below is the meaning a target must implement. The "R today" column shows that the
existing R output already conforms, so **no emitted R changes**. The "Rust" column is the planned
mapping, given as evidence that the item can be implemented, not as a contract.

| Construct | Portable meaning | R today | Rust (sketch) |
|---|---|---|---|
| `int` | 32-bit signed integer, never `NA` | `integer` with class `Integer` | `i32` |
| `num` | IEEE-754 double, never `NA`; `NaN`/`Inf` are ordinary values | `double` | `f64` |
| `bool` | `true`/`false`, never `NA` | `logical` | `bool` |
| `char` | UTF-8 string, never `NA` | `character` | `typr_rt::Str` (`Rc<str>`) |
| `T \| na` | `T` or the missing value | `NA` of the right mode | `Option<T>` |
| `null` | the absent value, distinct from `na` | `NULL` | `()` / unit, or `None` in `T \| null` |
| `[#N, T]`, `Vec[#N, T]` | immutable-by-value sequence, **1-based** indexing, copy on modification | typed vector / list | `typr_rt::Vector<T>` (`Rc<Vec<T>>` + `Rc::make_mut`, which is R's copy-on-modify) |
| vector arithmetic | element-wise; a scalar broadcasts; unequal lengths follow R's recycling rule | R | implemented in `typr_rt` with the same rule and warning |
| records | product of named fields; equality is structural | `list` + class + validator | a generated `struct` per record shape |
| tags / unions | tagged sum; `match` is exhaustive | constructor functions + class | a generated `enum` |
| `match` | first matching arm, top to bottom | `if`/`else` chain | `match` |
| functions, lambdas | first-class, closures capture **by value** | closures | `fn` items, `Rc<dyn Fn>` for values |
| argument evaluation | **strict, left to right** | lazy (see below) | strict |
| overloading | resolved by the type of the first argument | S3 dispatch at runtime | resolved statically, or a `match` on the union |
| generics | parametric | erased | monomorphised |
| interfaces | structural capability; satisfied by free functions | S3 | monomorphised; no trait needed in the general case |
| modules | namespaces with `@pub` visibility | environments | Rust `mod` |
| `State<T>` | shared mutable cell | environment | `Rc<RefCell<T>>` |
| `as!` / refinements | runtime validation, error on failure | `validate_T` / checks | generated checks, `panic!` on failure |
| errors | an uncaught error stops the program with a message | `stop()` | `panic!` with the same message |
| `Any` | an opaque value: it can be passed, returned, stored, printed, and narrowed with `as!` | the value itself | a monomorphised type parameter, or `typr_rt::Dyn` (§2, *`Any`*) |

Five points need more than a table cell:

**Integer overflow.** In R, `.Machine$integer.max + 1L` is `NA` with a warning. That breaks the
`int` invariant (`int` would hold `NA`), so it is a soundness bug in the R target today, not a
semantics to copy. The portable meaning is that **integer overflow is a runtime error**, with
the message `integer overflow`. On `rust` this is `checked_add` and friends, which `panic!`. On
`r`, the integer methods of `+`, `-`, `*` and `/` in `std.R` are fixed so they detect the
overflow and call `stop()` with that message. The check is the one R itself makes cheaply: an
`NA` in the result that was not in either operand. This fix is part of this RFC (see the
checklist), so overflow is not a documented divergence.

**Lazy evaluation.** R evaluates arguments lazily. TypR does not expose promises, so the only
observable difference is an argument that is never used and has a side effect or fails. The
portable semantics are strict. A program whose result depends on laziness is outside the portable
core. The compiler cannot detect this in general, so it is documented as a divergence and covered
by the differential test suite (§8), not checked statically.

**Recycling.** `[1, 2, 3] + [1, 2]` is accepted today. The portable meaning is R's rule, so that
no existing program changes meaning: the result has the longer length, and a warning is emitted
when the lengths are not multiples. Tightening it statically (lengths must be equal or one of
them 1, when the `#N` indices are known) is a separate type-checking RFC that would apply to both
targets.

**Overloading.** In the R output, a user function is an S3 method dispatched at runtime
(`area.Shape`, `f.Union0`). The type checker already knows which overload a call resolves to.
`typing()` returns the desugared `Lang` and `type_recorder` can record the type of every node. The
Rust target uses that static resolution. When the argument is a union and different members
select different overloads, the generated code `match`es on the value. The R target keeps S3,
which gives the same result.

**`Any`.** The type checker treats `Any` as an opaque top type: a value of any type can go in,
and the only ways out are passing it on and `as!`. This RFC does not change that typing. On
`rust`, `Any` is **lowered as a generic**. Each occurrence of `Any` in a signature or a field is
an implicit, independent type parameter, the same way `Interface@_` gets a fresh id at each
occurrence. This matches what the checker already accepts: `fn(a: Any, b: Any)` takes
`same(1, "a")`, so the occurrences really are independent. Monomorphisation then instantiates each
parameter at each call site:

```typr
let show <- fn(x: Any): char { "ok" };
show(1);       # instance show::<i32>
show("a");     # instance show::<Str>
```

An `Any` that occurs only in a return position is instantiated to the type the body produces in
that instance (`fn(x: Any): Any { x }` called on `3` returns an `i32`). An `as!` applied to a value
whose concrete type is known in the instance becomes a static check. If the type matches, it is
the identity. If it doesn't, the generated code `panic!`s with the same message as `validate_T`
on `r` (the failure stays a runtime error, as on `r`), and the compiler also emits a warning.

The generic reading fails when one `Any` holds **values of different types in the same
instance**. All three of these type-check today:

```typr
let x: Any <- if (c) { 1 } else { "a" };                          # one binding, two types
let pick <- fn(c: bool, a: Any, b: Any): Any { if (c) { a } else { b } };  # return mixes a and b
let bs <- [Box:{ v = 1 }, Box:{ v = "a" }];                        # `type Box <- list { v: Any }`
```

For those occurrences, and only those, the Rust target falls back to a boxed dynamic value,
`typr_rt::Dyn` (an `Rc<dyn std::any::Any>` that also carries a print function), and `as!` becomes
a downcast that `panic!`s on mismatch. The compiler chooses per occurrence and per instance, and
the user never writes either form. Because `Any`'s only operations are pass-through, printing
and `as!`, `Dyn` needs nothing more than that.

`Any` is therefore portable. What remains `r`-only is the **origin** of some `Any` values:
the result of an untyped R function (RFC 0028) or of an `R { ... }` block. That is already
covered by §3.

### 3. What is outside the portable core

These constructs make a definition `r`-only:

- `R { ... }`, `function(...)` (untyped R function), `extern ... r#"..."#` (raw R body);
- `Class(...)` (`RClass`), `Foreign<T>`;
- `dataframe[...]{...}` and `Tibble`-like `typeconstructor`s, until a portable table type exists
  (*Future possibilities*);
- `library(...)`, `use("pkg", ...)`, `@importFrom`;
- calls to an `UnknownFunction` (untyped preloaded R builtins, RFC 0028). `Any` itself is
  portable (§2).

Symmetrically, `rust`-only constructs are introduced:

- `Rust { ... }`: a raw Rust block, the mirror of `R { ... }`;
- `extern (x: int) -> int rust#"..."#`: the mirror of the raw-R extern.

### 4. Availability

**Definition.** For every top-level definition `d`, `avail(d) ⊆ Target`.

1. A primitive construct listed in §3 has `{r}` (or `{rust}`). Every other `Lang` node is
   available everywhere and contributes nothing.
2. A reference to another definition `e` (call, value, type alias, constructor) contributes
   `avail(e)`. This covers types as well as functions: a signature that mentions `Foreign<T>` or
   a data frame is `r`-only.
3. For a definition with a body, `avail_body(d)` is the intersection of the contributions of
   every node in the body and in the signature.
4. `avail(d) = (avail_body(d) ∪ externs(d)) ∩ declared(d)`, where `externs(d)` is the set of
   targets named in `@extern(...)` and `declared(d)` is the `@target(...)` set (all targets if
   absent). A body-less `@extern` has `avail_body = ∅`.
5. Mutually recursive definitions are solved as a greatest fixed point: start from "all targets"
   and remove targets until nothing changes. A cycle of definitions that only use each other is
   portable.

`declared(d)` is checked as well as applied. If `@target(r, rust)` is written but
`avail_body(d) ∪ externs(d)` lacks `rust`, the error (T053) is reported **on `d`**.

**Standard library.** `.ty` entries carry their availability in the metadata TypR already
parses:

```typr
#! pkg: base
#! tier: T1
#! targets: r, rust
@`+`: (int, int) -> int;
```

An entry without `#! targets:` is `{r}`. That is the honest default: every current entry is backed
by `std.R` only. Making the standard library portable then means adding `rust` to the entries
`typr_rt`/`typr_std` implement, one at a time. Each addition is checked by the differential suite.

**When it is checked.** For each target `t` in the project's targets, every **root** must have
`t ∈ avail(root)`. Roots are the entry point (`main.ty`'s top level), `@pub`/`@export` members of
the project's own modules, and, on `extendr`, every exported function. Definitions that are not
reachable from a root are never reported. A library may contain R-only helpers without becoming
R-only.

**Where it lives.** A new pass after type checking, over the call/use graph. `processes/spg`
already builds a graph of definitions with `Uses` edges, so the pass is a fixed point over that
graph plus a scan of each body for the §3 constructs. It needs no change to type checking.

**Error messages.**

```
error[T052]: `main` is not available on target `rust`
  ┌─ main.ty:6:5
6 │     summarise("data.csv");
  │     ^^^^^^^^^ `summarise` is only available on `r`
  = note: `summarise` → `load_raw` → `R { ... }` block (io.ty:12:3)
  = help: give one of these an implementation for `rust` with `@extern(rust = "...")`,
          or restrict `main` with `@target(r)`
```

```
error[T053]: `score` is declared `@target(r, rust)` but is only available on `r`
  ┌─ model.ty:3:1
3 │ @target(r, rust)
  │ ^^^^^^^^^^^^^^^^ declared here
  = note: `Foreign<Any>` in its signature is only available on `r` (model.ty:4:22)
```

The "note" line follows the shortest chain to a leaf that lacks the target, so the user always
sees *why* and not only *that*.

### 5. Syntax

| Form | Status | Meaning |
|---|---|---|
| `@extern pkg::fn: T;` | unchanged | sugar for `@extern(r = "pkg::fn") fn: T;` |
| `@extern(r = "...", rust = "...") name: T;` | **new** | body-less multi-target external |
| `@extern(rust = "...") let name <- fn(...) {...};` | **new** | external for some targets, TypR body for the others |
| `@target(r)`, `@target(r, rust)` | **new** | declared upper bound on availability |
| `Rust { ... }` | **new** | raw Rust block, `{rust}` only |
| `extern (...) -> T rust#"..."#` | **new** | raw-Rust-bodied extern, `{rust}` only |
| `#! targets: r, rust` | **new** `.ty` metadata key | availability of a stdlib / external-definition entry |

Lexemes added to the syntax manifest (`components/syntax/mod.rs`): `@target` and the `Rust`
block head. `@extern(` with a parenthesised argument list is a new form of an existing lexeme.
Both forms parse, and `signature_extern` tries the parenthesised one first. Target names are plain
identifiers, and an unknown one is a syntax error (S021, `UnknownTarget`) with the list of valid
names, never silently ignored.

`Lang::Signature { is_extern, extern_r_name }` becomes `Lang::Signature { externs:
BTreeMap<Target, String>, .. }`. `is_extern` is `!externs.is_empty()`. The binary serialisation
of `Lang` changes, which affects the cached context and must be bumped.

### 6. The `extendr` platform and the boundary check

`--platform extendr` builds a Rust crate plus the R wrapper package expected by `rextendr`. Each
`@export` function becomes an `#[extendr]` function, and its signature must only contain
**boundary types**, the types that cross the R ↔ Rust FFI without a TypR-specific encoding:

- `int`, `num`, `bool`, `char` and their `| na` variants;
- `[#N, T]` and `Vec[#N, T]` of those;
- records whose fields are boundary types (passed as named lists);
- `Empty`.

Tags, closures, `State<T>` and interfaces are not boundary types. Using one in an exported
signature is error T054, reported on the export:

```
error[T054]: `fit` cannot be exported through extendr
  ┌─ fast.ty:2:23
2 │ @export let fit <- fn(m: Model, xs: [#N, num]): num { ... };
  │                       ^^^^^^^^ `Model` is a union; only boundary types cross into R
  = help: export a wrapper taking the payload as a record, and build the union inside
```

The **hybrid package** case (an R package whose hot module is compiled through extendr while the
rest stays R) is the combination of the two targets in one project. It is described in *Future
possibilities* and not specified here. It needs §4 and §6 first, and it is where the boundary
check earns its keep.

### 7. Project and CLI contract

```toml
[project]
targets = ["r", "rust"]

[rust]
crate_name = "my_tool"         # default: project name in snake_case
platform   = "bin"             # bin | lib | wasm | extendr
edition    = "2024"

[rust.dependencies]            # verbatim Cargo dependency table, for @extern(rust = "...")
clap = { version = "4", features = ["derive"] }
```

- `typr check [--target t]` runs type checking once, then the availability pass per target.
- `typr build [--target t] [--platform p]` writes `target/rust/` (a generated Cargo project that
  depends on `typr_rt` at the compiler's version) and runs `cargo build`. It is never written
  under `R/`.
- `typr run --target rust` runs `cargo run`.
- Without `--target`, `build` and `run` use the first entry of `targets`. Existing projects have
  `["r"]` and see no change.

The generated Cargo project is an **artifact, not source**: regenerated on every build, never
edited, and listed in the `.gitignore` that `typr new` writes.

### 8. Conformance: differential testing

The portable core is defined by the table in §2, and **enforced** by running the same programs on
both targets:

- a `cases/` entry may carry `targets = ["r", "rust"]` in `expect.toml`. `typr case run` builds
  and runs it on each target and compares standard output;
- output is compared through TypR's own printing (`print` on portable values formats them the
  same way on both targets: `typr_rt` implements the format, R uses the existing `print` methods).
  A format change is therefore a change to both runtimes at once;
- a divergence is a `REGRESS`, unless the case is marked `divergence = "<reason>"`, which is how
  the documented divergences in §2 (overflow, laziness) are recorded rather than forgotten.

### 9. Implementation notes (non-normative)

- The transpiler today goes straight from `Lang` to an R string (`RTranslatable`, about 4,700
  lines with global state for includes and generated files). The Rust target is **not** built on
  top of that. It reads a typed, desugared intermediate representation built from what `typing()`
  returns, with every node's type resolved (the `type_recorder` work is the starting point). The R
  backend is not migrated to the IR by this RFC, and `cases/` protects it in the meantime.
- **The emitted Rust is an intermediate artifact, not a product.** This is the model Nim uses
  with C: it is not meant to be read, edited or diffed, and its shape can change in any release.
  It is optimised for `rustc` (and for compile times), not for a reader. Mangled names, generated
  helper types and duplicated monomorphised code are all acceptable. The contract stops at the
  behaviour of the binary (§2, §8) and at the Cargo/CLI surface (§7). This is the opposite of the
  R target, where readability is the product.
- Monomorphisation is the planned strategy for generics, interfaces and `Any` on `rust`.
  Whole-program compilation knows every instantiation, which avoids trait coherence problems
  (orphan rules) when an interface is satisfied by a type from another module.
- Anonymous records and spread (`%R`, `...source`) are the hardest mapping to nominal Rust
  structs. The plan is one generated struct per distinct field set, with spreads expanded at the
  instantiation site after monomorphisation.

## Gradual typing

Unannotated code is unaffected on `r`. On `rust`, the gradual end of the dial (`function(...)`,
`R { }`, calls to untyped R builtins, see RFC 0028) is `r`-only. `Any` written by hand stays
portable (§2): it is the *source* of the untyped value that ties a program to R, not the type. So the more a program leans on
untyped R, the fewer targets it has. This is the intended trade: portability is earned by typing,
the same way safety is. A fully unannotated program that only uses inference (`let x <- 3;`) is
not affected, because inference produces ordinary types and stays portable.

## Drawbacks

- **Two runtimes to keep in sync.** Every portable stdlib function is implemented twice. The
  differential suite makes divergence visible, but it doesn't make the work go away.
- **`typr_rt` contradicts "no runtime" for the Rust target.** `rfcs/README.md` asks for output
  with no companion library. That constraint remains true of the `r` target, and this RFC changes
  nothing there. The `rust` target needs a runtime crate (vectors, recycling, printing), and the
  constraint is explicitly scoped to `r`.
- **The language gets frozen.** Writing down that arguments are evaluated strictly, that overflow
  is an error, and that `NaN` is a `num` turns today's accidents into commitments.
- **Checked integer arithmetic on `r` costs something.** The overflow check adds one scan for
  `NA` per integer operation in `std.R`. On long vectors that is measurable, and it is paid by
  every R user, including those who never build for `rust`.
- **A large, long project.** The availability pass is small. The Rust backend, the runtime and a
  portable stdlib are each larger than any previous TypR feature.
- **`@extern` syntax grows a second form.** Two spellings for the same thing until the old one is
  deprecated, if it ever is.

## Rationale and alternatives

**Replace R by Rust as the only target.** Rejected. TypR's audience is R users, and readable R is
the argument for adopting it. A Rust-only TypR is a different language with a different audience.

**Generate Rust that embeds an R-like dynamic value (`enum RValue`) everywhere.** This is the
quickest way to "compile everything", because untyped code would also work. But it produces slow
binaries that keep R's semantics with none of the benefits. Readability is not the argument here,
since the emitted Rust is not meant to be read (§9). The point of the Rust target is speed and
native types, so it compiles the *typed* core, and uses a dynamic value (`Dyn`) only where an
`Any` cannot be monomorphised.

**Conditional compilation (`#[cfg(target)]`-style blocks) instead of availability inference.**
Gleam initially offered target-conditional definitions and moved towards externals with a
fallback body plus inferred availability (to verify against the current Gleam documentation
before acceptance). Conditional blocks scatter target logic across the code base and give no
error until someone builds the other target. Inference gives errors at the first `typr check`.

**Annotate every function with its targets.** Exact, but nobody would do it, and it rots. Here
`@target` exists only to state intent where it is useful.

**Do nothing.** The cost is the three missing use cases in *Motivation*, and the dormant
`TargetLanguage::JS` remains as a half-built precedent.

## Prior art

- **Gleam** compiles to Erlang and JavaScript. `@external(erlang, ...)` / `@external(javascript,
  ...)`, a function body as fallback, and an error when a function is used on a target it doesn't
  support. This is the direct model for §4 and §5.
- **Nim** compiles to C (and C++/JavaScript) and treats the generated C as a build artifact that
  nobody reads. This is the model for the status of the emitted Rust (§9).
- **Kotlin Multiplatform**: `expect`/`actual` declarations and a common source set. It is heavier
  than Gleam's model, but its lesson matters: the common standard library is what makes the
  common code useful.
- **Haxe** compiles to many targets with conditional compilation everywhere. Its experience is the
  argument against the `#if` approach.
- **extendr / Rcpp**: the R community's existing path to native code. This RFC does not compete
  with them. It produces extendr code instead of asking the user to write it.
- **R itself**: `NA_integer_` is a sentinel inside the `integer` type. TypR's choice to make `NA`
  a separate type `na` (`int | na`) is what makes the Rust mapping cheap (`i32` vs
  `Option<i32>`), and it is already enforced by the type checker.

## Resolved during review

- **`Any` on `rust`: lowered as a generic, with a dynamic fallback.** First options considered:
  make it `r`-only, or make it a dynamic value everywhere. The adopted reading is that each
  occurrence of `Any` is an implicit independent type parameter, monomorphised like any other
  generic, with `typr_rt::Dyn` only for the occurrences that hold several types in one instance
  (§2, *`Any`*). This keeps `Any` portable without making the common case dynamic, and it leaves
  the typing of `Any` unchanged.
- **Runtime errors on `rust`: `panic!`.** The portable meaning of an error is the R meaning of
  `stop()`: the program stops with a message. `as!`, refinement checks, overflow and explicit
  errors all `panic!` with the message the R target gives. Recoverable failures are expressed
  in the standard library with tags (`.Ok`/`.Err`), not with a second error mechanism.
- **Integer overflow on `r`: fixed, not documented.** The integer operators in `std.R` detect
  overflow and `stop()` (§2). This is a soundness fix for the `r` target in its own right, and
  it lands with step 1.
- **The emitted Rust is not a contract, and not meant to be readable.** It is a build artifact,
  as C is for Nim (§9). Only the binary's behaviour and the Cargo/CLI surface are specified.

## Unresolved questions

To settle during implementation:

1. The exact `typr_rt` vector representation (one generic `Vector<T>` vs specialised atomic
   vectors), driven by benchmarks.
2. Whether `wasm` needs a capability mechanism (no filesystem, no process) or whether it is
   enough for the relevant stdlib entries to be `rust`-only externals that fail to link on
   `wasm32`.
3. The rule that decides, for one `Any` occurrence, between monomorphisation and `Dyn`. The
   simplest one: after monomorphisation, an occurrence that receives two different concrete
   types in one instance becomes `Dyn`. Whether this is decided per instance or per definition
   is left to the implementation.
4. Name mangling into Rust. Since the output is not read, any injective scheme works. The only
   requirement is that panic messages and printed output use the TypR names.
5. Before acceptance, check the claim about Gleam's `@target` in *Rationale and alternatives*
   against Gleam's current documentation.

## Future possibilities

- **Hybrid R packages.** In an `r` project, a module declared `@native` is compiled through the
  `rust`/`extendr` pipeline and called from the R side through generated wrappers. The boundary
  check (§6) applies to the module's `@pub` surface. This is the "write the hot path in TypR"
  story.
- **A portable table type** to replace `dataframe` on both targets, backed by `data.frame` on `r`
  and by Arrow/Polars on `rust`.
- **A cross-platform standard library** (`typr_std`) with filesystem, processes, HTTP, time and
  terminal I/O. Each entry has an R and a Rust implementation, checked by the differential suite.
- **CLI, TUI and GUI** through `@extern(rust = ...)` bindings to `clap`, `ratatui` and `egui`,
  first as hand-written bindings, then as `.ty` external definitions distributed like RFC 0031's.
- **The `js` target**, re-specified on the same model, or replaced by `rust` + `wasm`.
- **Static recycling checks** using `#N` (§2).

## Implementation checklist

Proposed order. Each step is shippable on its own.

- [x] **Step 1. Availability with one real target.** `Target` replaces `TargetLanguage`
      (default `R`). `@extern(...)`, `@target(...)` and `#! targets:` are parsed, plus the
      availability pass and T052/T053. `rust` is a declarable target with an empty stdlib.
      Useful immediately: `typr check --target rust` tells you how far a project is from
      portable.
      Done 2026-10-13 (`processes/availability.rs`, cases 0107-0109). Differences from the
      text above, to settle before acceptance:
      * `Lang::Signature` keeps no `is_extern`; it carries `externs: BTreeMap<Target,
        Option<String>>` (`None` = same name, the bare `@extern name` form). `@target(...)` and
        `@extern(...) let` are a separate statement, `Lang::Attribute`, that applies to the next
        definition and is invisible to type checking and transpilation.
      * Resolution is by name within one source file; no cross-file or cross-module
        resolution, and type aliases are not definitions (a `dataframe`/`Foreign<T>` hidden
        behind an alias isn't seen). A local binding shadows a definition for the whole body.
      * A *call* to a stdlib/preloaded R function is `r`-only unless `#! targets:` says otherwise,
        so with the stdlib still empty even `print("x")` makes a root `r`-only. Operators
        (`+`, `*`, …) are portable by construction (§2). `#! targets:` is parsed
        (`FunctionMeta::targets`); nothing feeds it to the pass yet (`StdlibAvailability::with_targets`).
      * Only the first offending chain per root is reported (the shortest).
      * The `Rust { ... }` block and the raw `extern ... rust#"..."#` form are not parsed yet
        (no backend to give them a meaning); `@target` is in the syntax manifest, `Rust` is not.
- [x] **Integer overflow on `r`**: checked integer operators in `std.R`, `stop("integer
      overflow")`, plus a `cases/` entry. This is independent of the rest and can land first.
      Done 2026-10-13: an `Ops.Integer` group method in `std.R` (also `typr-mcp/data/std.R`)
      for `+`, `-`, `*`; an `NA` in the result absent from both operands means overflow. Other
      warnings (recycling) are re-signalled. Case 0110. Not covered: `/` on `int` is typed
      `int` but R's `/` returns a double (`7L / 2L` is `3.5`), a separate soundness issue.
- [x] **Step 2. Typed IR** built from `typing()`'s output, with no change to the R backend.
      Done 2026-10-13 (`processes/ir/`: `mod.rs` the IR, `lower.rs`, `print.rs`; `typr ir FILE`
      prints it). Differences from the text above, to settle before acceptance:
      * `typing()` does **not** return a rewritten program: for `Lines` it returns the desugared
        `Lang` of the *last* statement only, and the R backend transpiles the parsed AST and
        re-types locally. So §9's "reads what `typing()` returns" holds statement by statement:
        `check_and_lower` folds the top level the way `typing()` folds `Lines` and lowers each
        statement's own result. Nested blocks are lowered from that statement's `Lang`.
      * Types come from the `type_recorder` table, keyed by span. Two gaps filled by the lowering:
        a callee's name has no entry (`function_application` never types it), so it gets the
        instantiated signature `(arg types) -> call type`; and `Lang::Operator` names its operands
        backwards (`rhs` is the syntactic left one), swapped once in `lower.rs`.
      * Lowering is total. A construct without a portable meaning (`R { }`, untyped R function,
        data frame, package import, extern signature, `as!`, record spread, `PartialApp` if one
        survives) is `ExprKind::Unsupported { what }`; `Program::unsupported()` lists them.
      * Not covered yet, left as `Unsupported`: spreads, `as!`, interfaces, `Use`, `Lang::Test`.
        Generics are `IrType::Param`; nothing is monomorphised (Step 5).
      * `IrType` drops lengths (`#N`) and refinements, and turns `T | na` into `Optional(T)`.
- [x] **Step 3. Minimal Rust backend**: scalars, `int | na`, functions, `if`/`match`,
      records, tags, modules. `typr build --target rust` produces a binary.
      Done 2026-10-13 (`processes/rust_backend/`: `mod.rs` the emitter, `runtime.rs` the inlined
      runtime; `typr build|run --target rust`, tests in `crates/typr-cli/tests/rust_target.rs`).
      The pipeline is parse → type check → availability on `rust` → IR → one `main.rs` under
      `target/rust/` → `cargo build|run`. Differences from the text above, to settle before
      acceptance:
      * `--target` is also taken by `build` and `run`; without it, the first of
        `[project] targets` (a bare file is `r`). Only the `bin` platform exists. `[rust]
        crate_name` is read, `edition`/`dependencies` are not (the manifest has no dependencies).
      * Top-level statements are the entry point, as on `r`: they run in `fn main()` in order.
        A user function called `main` is emitted as `main_` and is *not* called automatically.
      * `print` is the only call the backend implements (`INTRINSICS`; seeded as `r, rust` in
        `StdlibAvailability::from_context` until Step 7). Any other stdlib call is refused with
        "not available on `rust`". Its format follows R for scalars (`[1] 3`, `TRUE`, `NA`, 7
        significant digits); records and tags print as Rust `Debug` for now (Step 4).
      * Types: `int`→`i32`, `num`→`f64`, `char`→`String`, `T | na`→`Option<T>`, a record shape →
        one `struct` (the declared one when the shape matches), a union of tags → an `enum`
        (tags must belong to a declared `type`; every type shares one namespace, hoisted to the
        crate root, so two modules cannot declare the same type name). `int` arithmetic
        (`+ - *`) is checked and panics with `integer overflow`; `/` on `int` is Rust's integer
        division (the R/`int` soundness issue above applies); `%` on negatives differs from R's
        `%%`.
      * Refused with the Step that brings them: vectors, indexing, `for`, `in` (4); generics,
        `Any`, interfaces, union of non-tag types (5). Not done at all: matching on a type
        (`x as int`), nested modules, top-level `let` other than functions and literals inside a
        module.
      * Limits that surface as `rustc` errors rather than wrong output: a function cannot see a
        top-level `let` that is not a literal (it lives in `main()`); a nested `let f <- fn` is a
        Rust `fn` item and cannot capture; a lambda is a `move` closure in an `Rc`.
      * `IrType` of a `Var` can be wrong (the type table is keyed by span, and a variable shares
        its span with the field access built on it: `q$x` records `int` for `q`), so the emitter
        keeps its own scopes. Also fixed in `lower.rs`: `m$f(args)` was lowered to an empty
        field access; it is now a call on `Field { m, f }`.
      * Pre-existing, found on the way and not touched: `while`/`loop` do not parse in the
        current parser (`syntaxe.md` documents them); `.Dot` (a tag with no payload) in a union
        makes `.Circle(2.0)` stop matching its parameter.
- [x] **Step 4. `typr_rt`**: vectors, recycling, printing, `State<T>`, validation. Differential
      `cases/` (§8). Done 2026-10-13, **except validation** (`as!`, refinements), which is
      still `Unsupported` (see below). Runtime in `processes/rust_backend/runtime.rs`, unit
      tests in `rust_backend/mod.rs::runtime_tests` (the file is compiled there too), cases
      0111-0114. Differences from the text above, to settle before acceptance:
      * **`typr_rt` is still inlined**, not a crate: it has no dependency, so a generated
        project builds offline, and nothing in it depends on being inlined. Becoming a crate is
        mechanical once a second consumer (`wasm`, `extendr`) needs it. `[rust.dependencies]`
        stays unread.
      * `[#N, T]` / `Vec[#N, T]` is `typr_rt::Vector<T>`, a generic `Rc<Vec<T>>` (unresolved
        question 1 is answered for now: one generic vector, no benchmark yet). `Vector::at` is
        1-based; `v[is]` is `select`; `for` iterates a clone of the elements.
      * Operators on vectors are `zip_with` with R's recycling rule (longer length, shorter
        operand repeated, a warning on stderr when the lengths do not divide, empty operand
        gives an empty result). A scalar is a vector of length one, `int` widens to `num` per
        element, `int` `+ - *` stay checked. `Lang::VecFunctionApp` (a scalar user function
        lifted by the checker) is lowered as a plain call and lifted again by the backend for one
        or two arguments (`map` / `zip_with`).
      * **Printing** follows R's `print` of an unboxed atomic vector: `[1] ...`, one field width,
        `num` vectors with a common number of decimals, `char` quoted and left-aligned, lines
        wrapped at 80 columns with `[26]`-style labels, `NA`, `integer(0)`. Not done: scientific
        notation (`1e+05`), `NaN` in the decimals count, nested lists. Records and tags still
        print as Rust `Debug`.
      * **A divergence found on the way (case 0112).** On `r`, `print` of a *boxed* scalar (class
        `Integer`/`Character`/`Boolean`/`Number`) goes through `print.Integer` & co. in `std.R`,
        which use `cat`: `2`, `hi`. The same value out of a base function (`length(v)`, `v[2]`)
        prints `[1] 2`. So `r` itself is inconsistent, and `rust` prints `[1] 2` / `[1] "hi"`
        (the unboxed format). Which one is canonical has to be decided before the differential
        suite can cover scalars; until then the cases print vectors or wrap a scalar in `c(...)`.
      * Out-of-range `v[7]` is an error on both targets (R's `[[` stops), so `int` never holds
        `NA` there; case 0113 pins it (`subscript out of bounds`).
      * `State<T>` is `typr_rt::State<T>` (`Rc<RefCell<(T, version)>>`); `state`, `get`, `set`,
        `update`, `version` are intrinsics. Copying the handle shares the cell. `State.map` and
        the like are not intrinsics yet (Step 7).
      * Intrinsics added: `length`, `sum` (on `[int]` / `[num]`), `seq` (`a:b`), `state`, `get`,
        `set`, `update`, `version` (with `print`). Any other stdlib call is still refused.
      * `typr case`: `expect.toml` takes `targets = ["r", "rust"]` (replays with
        `typr run --target t`, compares stdout) and `divergence = "<reason>"` (the case passes
        only while the outputs differ). The CLI's progress goes to stderr, so stdout is the
        program's own output.
      * **Validation: obligations done (2026-10-13, case 0124).** The boundaries where the
        checker could not prove a refinement (`let a: [3, int] <- v`, an argument, a returned
        value; `refinement_check.rs`) are recorded in `Context::refinement_obligations`, keyed by
        span. `lower.rs` wraps the expression at that span in `ExprKind::Refine { value, checks,
        loc }` (`RefineCheck`: a length or value interval, so the IR keeps no checker type), and
        the emitter calls `typr_rt::refine_length` / `refine_value`, which `panic!` with the exact
        message of `typr_refine_*` in `std.R` (`Type refinement violation at file:line: ...`).
        `IrType` still drops the bounds: the check lives on the expression, not on the type.
        Checked on `int`/`num` scalars and vectors of them; a refinement on any other type is
        refused. `int & (> 0)` before normalisation (`Operator(Intersection, _, Property)`) now
        lowers to its base type.
      * **`as!` to a refined alias is still not done, on either target.** `x as! Age` with
        `type Age <- int & (>= 0)` fails on `r` already (`validate_Age` is never generated, so R
        stops with "impossible de trouver la fonction"): the R side has to be fixed first, then
        `Cast` can carry the alias's `RefineCheck`s. On `rust` the cast is currently the identity.
      * Not covered: `x in v` (the checker rejects `in` as a function today), vector assignment
        (`v[2] <- x` does not parse), `Array[...]` of records printing, vectors inside generic
        types.
- [x] **Step 5. Generics, interfaces, closures, `Any`** (monomorphisation, `Dyn` fallback).
      Done 2026-10-13 (all in `processes/rust_backend/mod.rs`, runtime `Dyn` in `runtime.rs`,
      cases 0115-0118, tests in `tests/rust_target.rs`). Differences from the text above, to
      settle before acceptance:
      * **Generic functions** are never emitted as such. A call reads the type parameters off
        the call site (the arguments' own types first, then the checker's instantiated callee,
        then the call's type) and goes to an instance `name__N`, emitted once per distinct
        substitution; instances may request others (worklist). The body is a clone with every
        type substituted, so the emitter itself did not change. The body's types name the
        checker's rigid variables (`__rigid_0`), not the signature's `T`: they are read off the
        parameters' uses (`derive_rigids`).
      * **`Any` and interfaces in a parameter** are implicit type parameters, one per occurrence
        (`anyfy`). An interface-typed parameter is monomorphised like `Any`: the body's method
        calls then resolve to the free function for the concrete type.
      * **Overloads** (several `let f <- fn(...)` of one name, in one scope) are resolved
        statically: exact parameter types first, then a loose compatibility pass, else the first
        definition. They are emitted as `f__o0`, `f__o1`, ... Generic overloads work the same way.
      * **Generic types** (`type Box<T> <- list {...}`, `type Opt<T> <- .Some(T) | ...`) become
        one struct or enum per distinct argument list (`Box__3`). A generic tag needs its type
        known from the context (expected type, or the scrutinee's); `let a: Opt<int> <- .Some(4)`
        works, a bare `.Some(4)` passed where the checker records no `Opt<int>` does not.
      * **`Any` as data** (a binding, a field, an unresolved return) is `typr_rt::Dyn`
        (`Rc<dyn Any>` + the printed form). Unresolved question 3 is answered for now by the
        simplest rule: an `Any` that is an *argument* position of a generic function is
        monomorphised; an `Any` in a binding, a field or a return the call site cannot pin
        down is `Dyn`. A `Dyn` prints as the value did when it was wrapped. The per-instance
        refinement (return `Any` becoming `i32` when the body returns one type) is not done.
      * **`as!`** is `ExprKind::Cast`. Done: from `Any` (a downcast that panics with
        `Validation failed for type T: expected T`), identity, and between two different
        scalars (always fails). **Refinements are still not validated**: `IrType` drops the
        bounds, so `x as! Age` where `Age` is `int{>= 0}` is refused with a message.
      * Closures needed nothing new beyond Step 3 (`Rc<dyn Fn>`, `move`); a function literal's
        recorded type can be another function's (same span), so the emitter types it from its
        own parameters.
      * Also fixed on the way: a literal the checker recorded no type for (an argument of a call
        through a module field) is typed from its own kind; calls between functions of one
        module (`pick` calling a sibling) resolve.
      * **Found, on the `r` target, not touched**: a generic tag union
        (`let a: Opt<int> <- .Some(4)`) never gets an `as.Opt`; `first(xs: [#N, T])` on a bare
        vector fails in `UseMethod` ("pas de méthode ... classe c('integer','numeric')"); a
        value that crossed an `Any` parameter prints `attr(,"class")` (case 0118, recorded as a
        divergence).
      * Still refused: a union of non-tag types (`int | char`) as a type, recursive generic
        types, a generic function used as a value, matching on a type (`x as int`), an interface
        in a data position (field, vector element), `Dyn` of a closure.
- [x] **Step 6. `wasm` and `extendr` platforms**, plus the boundary check T054.
      Done 2026-10-13 (`processes/rust_backend/platform.rs`, `emit_with`; `--platform` on
      `typr build|run`; case 0119; tests in `rust_backend::platform_tests` and
      `tests/rust_target.rs`). Differences from the text above, to settle before acceptance:
      * **Selecting a platform**: `--platform bin|lib|wasm|extendr`, else `[rust] platform` in
        `typr.toml`, else `bin`. An unknown name is an error listing the valid ones. `typr run`
        needs `bin`.
      * **`lib`**: `src/lib.rs`, no `fn main`; top-level statements that are not definitions are
        dropped (nothing runs them).
      * **`wasm`**: a `cdylib` built with `--target wasm32-unknown-unknown`, with **no
        dependency** (no wasm-bindgen): each `@pub` top-level function whose parameters and
        result are `int`/`num`/`bool` (result may be `Empty`) gets a C-ABI wrapper exported under
        its TypR name. Other `@pub` functions stay Rust-only; there is no boundary check on `wasm`
        and no error for them. Verified by loading the `.wasm` in node (`area(2)` = 12.56636). A
        richer ABI (`char`, vectors, records) needs wasm-bindgen or a hand-written one.
      * **`extendr`**: a `staticlib` depending on `extendr-api`, one `#[extendr]` wrapper per
        `@pub` top-level function (`typr_export_<name>`), an `extendr_module!`, and
        `target/rust/R/extendr-wrappers.R` with the R functions that `.Call` them (with
        `@useDynLib <crate>, .registration = TRUE`). The generated crate compiles against
        `extendr-api` 0.9. **Not run from R**: assembling the R package around it (`rextendr`'s
        layout, `src/entrypoint.c`, `DESCRIPTION`/`NAMESPACE`) is left to the user for now.
        Supported across the boundary: `int`/`num`/`bool`/`char`, `T | na` for a scalar, and
        `[#N, int|num|char]` (as `Vec`). Vectors of `bool` and **records pass T054 but are refused
        by the emitter** ("not supported through extendr yet"), as is an overloaded export.
      * **T054** runs before anything is generated, on `@pub` top-level functions only (not
        module members), for `extendr` only. It points at the definition's first line, not at the
        offending parameter (the IR keeps one span per definition), and names the parameter in
        the label. Generic and `Any` signatures are rejected too ("not a concrete type").
      * `HelpData::at(file, offset, end)` was added so a diagnostic can be raised from the IR.
- [~] **Step 7. Portable stdlib**: `#! targets: r, rust` added entry by entry. First slice done
      2026-10-13 (case 0120, `standard_library::declared_targets`); the stdlib is not fully
      portable, and it is meant to grow one entry at a time. Differences from the text above, to
      settle before acceptance:
      * **The `.ty` sources are now the source of truth.** `typr-cli` reads `#! targets:` from the
        T1 sources (`R_T1_SOURCES`) and feeds the availability pass (`with_targets`); the hard-coded
        seeding from `INTRINSICS` is gone. `print`, `sum`, `seq`, `state`, `get`, `set`, `update`,
        `version` carry the tag, plus the new `map`, `filter`, `reduce`, `fold`, `extend`,
        `startsWith`, `endsWith`, `stop` (a `panic!`; case 0121), `join`, `contains` (case 0122; neither had an
        implementation in `std.R`, so `.default` methods were added there: `paste(a, collapse = s)` and
        `grepl(a, b, fixed = TRUE)`) (and `State.map`), then `set_at` and `as_vec` (case 0123). A test (`declared_rust_targets_match_the_backend_intrinsics`)
        keeps the tags and the backend's `INTRINSICS` equal in both directions.
      * **`length` cannot be tagged**: it is an R builtin that the stdlib must not declare (a
        signature would shadow it), so it is the one `UNDECLARABLE_INTRINSICS` entry, seeded in
        `from_context`.
      * **Granularity is the name**, not the signature: `map` is portable as soon as one overload
        declares it, and the backend refuses the argument types it does not implement (e.g. `get`
        on `(Any, char)` in `default.ty` is untagged, `get` on a `State` is tagged).
      * `set_at` out of range is an error on both targets (`subscript out of bounds`): R's `replace`
        grew the vector and padded it with `NA`, so `std.R` (both copies) now checks the bound.
      * Runtime additions in `runtime.rs`: `set_at`, `vmap`, `filter`, `reduce` (empty vector panics with the
        message `r` gives), `fold`, `extend`, `starts_with`, `ends_with`, `state_map`.
      * Not done: `sort`/`unique` (need `Eq`/`Ord` interfaces on `rust`), `cat`, anything on `base.ty` (doc-only, `UnknownFunction`).
      * **Found, on the `r` target, not touched**: a user function passed as a value inside a
        vector literal (`[reduce(xs, add2)]`) is emitted as `add2.default`, which does not exist.
        Case 0120 uses lambdas to stay clear of it.
- [ ] `cases/` entries for every error code and every documented divergence
      (T052: 0107, T053: 0108, portable: 0109, T054: 0119, stdlib: 0120; S021 and the divergences still missing)
- [ ] `typr syntax --write` (`@target` done; `Rust { }` with Step 3)
- [x] `syntaxe.md` updated in both copies (Step 1 forms only)
- [ ] Documentation PR on `we-data-ch/typr.github.io`
