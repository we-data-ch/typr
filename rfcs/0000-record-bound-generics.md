- **Status:** draft
- **RFC PR:** we-data-ch/typr#0000
- **Tracking issue:** —
- **Implemented in:** not yet
- **Start date:** 2026-10-24

# Records and data frames that keep the caller's type

## Summary

A function written `fn(x: list{a: int}): list{a: int}` accepts any record that has at least a
field `a: int`, and returns **the caller's own record type**: `f(list(a = 1, b = true))` is a
`list{a: int, b: bool}`, not a `list{a: int}`. The record type written in the signature becomes
the *bound* of a hidden type variable, exactly as an interface does since
`0000-interface-bound-generics.md`. The body may read and update the bound's fields, and may copy
the parameter (`list(...x, a = 2)`, `Self:{ a = 2, ...x }`), but it may not return a freshly built
record in its place. The same holds for data frames (`df[#N]{a: int}`), whose columns are kept. A
new type form `exact list{...}` names a closed record: exactly these fields, with exactly these
field types, never widened. The emitted R does not change.

## Motivation

Record parameters use width subtyping today, and the extra fields are lost on the way out
(measured on `develop`, 2026-10-24):

```typr
let f <- fn(x: list{a: int}): list{a: int} { x };
let z: list{a: int, b: bool} <- f(list(a = 1, b = true));
# Type error: type list{a: int, b: bool} doesn't match type list{a: int}
#   Received list{a: int}
```

```typr
let y <- f(list(a = 1, b = true));
y.b;   # Type error: list{a: int} doesn't have a field 'b'
```

A function that works on "anything with an `a`" cannot be reused without losing everything else
the caller knew. That defeats the main reason to write it generically. The alternatives today
are all worse:

- `fn(x: %R): %R` keeps the type, but the body knows nothing about the fields (`x.a` is rejected).
- A named alias per caller duplicates the function.
- `Any` gives up typing.

Data frames hit this hardest. `crates/typr-cli/configs/std/dataframe.ty` explicitly leaves `head`,
`subset`, `merge`, `rbind`… out of the typed catalog, or types them `-> Any`, because "functions that
return a data frame whose columns depend on their arguments" cannot be expressed. A row filter
keeps its columns, and nothing in the type language says so.

Two related defects were found while preparing this RFC. They are bugs, not design, and are
listed under *Prerequisites*.

## Guide-level explanation

### A record parameter keeps the caller's type

```typr
let bump <- fn(x: list{a: int}): list{a: int} {
  x$a <- x$a + 1;
  x
};

let p <- bump(list(a = 1, b = true, name = "p"));
p$name;   # char: `bump` returned the caller's record, not a `list{a: int}`
```

Read the signature as "give me any list with at least `a: int`, and you get back **the same kind
of list**". The record written in the signature is a *minimum*, not the exact type.

### What the body may do

The body only knows the fields written in the signature. It works on *the caller's* record, so it
must give that record back, possibly updated:

```typr
let ok1 <- fn(x: list{a: int}): list{a: int} { x };                          # as is
let ok2 <- fn(x: list{a: int}): list{a: int} { x$a <- 0; x };                # update a known field
let ok3 <- fn(x: list{a: int}): list{a: int} { list(...x, a = x$a * 2) };    # copy with an override
```

```typr compile_fail
let bad <- fn(x: list{a: int}): list{a: int} { list(a = 2) };
```

```
× Type error: this function must return the caller's record, received a new list{a: int}
  ╭─[main.ty:1:48]
1 │ let bad <- fn(x: list{a: int}): list{a: int} { list(a = 2) };
  ·                  ──────┬─────                  ─────┬─────
  ·                        │                            ╰── a new list{a: int}: the caller's other fields are lost
  ·                        ╰── the return type is tied to `x`: it is whatever record the caller passed
  help: copy the parameter and override its fields: `list(...x, a = 2)`
        or, to return a plain new record, declare `: exact list{a: int}`
```

Teaching line for an R user: *"the record type in your signature is a minimum. Whatever the
caller passes comes back out with all its fields, so build your result from the parameter."*

### Returning a new record: `exact`

When a function really builds a new record, it says so with `exact`:

```typr
let reset <- fn(x: list{a: int}): exact list{a: int} { list(a = 0) };
reset(list(a = 1, b = true));   # exact list{a: int}
```

`exact list{a: 3, b: bool}` is the type of lists with **exactly** the two fields `a` and `b`,
where `a` can only be `3`. Nothing is widened, and nothing extra is allowed. An `exact` type in a
signature is never tied to anything: it means exactly what it says.

### Field types are generalized, unless `exact`

The caller's literal and refined field types do not leak into the bound fields:

```typr
let p <- bump(list(a = 1, b = true));
# p : list{a: int, b: bool}
#   a: int   because the signature says int; the body may have written any int there
#   b: bool  because the literal `true` is generalized
```

Only an `exact` argument keeps its precise types on the fields the signature does not mention.

### Several parameters, explicit tags

When the same record type appears in several parameters **and** in the return type, they all
stand for one type, as for interfaces. To make them independent, tag them, using the `@A` you
already know from interfaces:

```typr
let pick  <- fn(x: list{a: int}, y: list{a: int}): list{a: int} { x };   # x and y: same type
let pick2 <- fn(x: list{a: int}@A, y: list{a: int}@B): list{a: int}@A { x };
let pick3 <- fn(x: list{a: int}, y: list{a: int}): bool { x$a == y$a };  # no return tie: independent
```

### Data frames keep their columns

```typr
let positives <- fn(d: df[#N]{a: int}): df[#M]{a: int} { ... };
let d2 <- positives(data_frame(a = [1, -2], label = ["x", "y"]));
# d2 : df[#M]{a: int, label: char}
```

For data frames, only the **columns** are tied. The number of rows comes from each annotation, so
a filter (`#N` in, `#M` out) keeps its columns and changes its height.

### Named aliases are unchanged

```typr
type Point <- list{x: int, y: int};
let shift <- fn(p: Point): Point { Point:{ x = p$x + 1, y = p$y } };   # still fine
```

A named record alias reads as "a `Point`", and building a fresh `Point:{...}` from one is the most
common thing done with it. The rule above applies only to record types **written out** in the
signature. An alias opts in with a tag: `fn(p: Point@A): Point@A { Self:{ x = p$x + 1, ...p } }`.

## Reference-level explanation

### Syntax

1. **Bound suffix on record and data-frame types.** `@Id` (one uppercase letter, or `_`) may
   immediately follow the closing `}` of `list{...}`, `dataframe[...]{...}` and `df[...]{...}`,
   with no space, exactly as after an interface alias (`bound_suffix` in
   `processes/parsing/types.rs`). It parses to `Type::Bounded(id, Box<record type>, h)`. Today
   `list{a: int}@A` is the fatal syntax error S023 (`StructuralBoundSuffix`, cases/0129, raised
   by `reject_structural_bound_suffix`); this RFC replaces that error, for records and data
   frames only (`[N, T]@A` stays S023), by the parse above.
2. **`exact`.** A new type-level keyword, prefix of a record or data-frame type:
   `exact list{...}`, `exact df[N]{...}`. It parses to a flag on the record type (see
   *Representation*). `exact` is only valid before a record or data-frame type. Anywhere else
   (`exact int`, `exact Point`) it is a syntax error. **Lexeme added to the manifest**
   (`components/syntax/mod.rs`), so `typr syntax --write`, and a gloss in
   `typr.github.io/scripts/syntax-glossary.mjs`.

   `exact Point` is rejected rather than accepted: an alias can be declared exact at its
   definition (`type Point <- exact list{x: int, y: int}`). Accepting both spellings would give
   two ways to say one thing.

### Representation

- `Type::Bounded(id, bound, h)` is reused unchanged. `bound` may now be a `Record` or a
  `DataFrame`, not only an interface.
- `exact` is a boolean on the record and data-frame variants. Adding a field to an existing variant
  changes the binary serialization layout of `Type` (the stdlib cache, `.typr_cache`); bump the
  cache format version. If a new variant is preferred (`Type::Exact(Box<Type>)`), it must be
  appended **last** in `Type` for the same reason. Decide at implementation; the semantics below do
  not depend on it.

### Signature normalization (`type_checking/signature_normalization.rs`)

A new pass, `desugar_bare_records`, runs next to `desugar_bare_interfaces`:

1. Collect every **bare** record/data-frame occurrence in the return type: written out (not an
   alias), not `exact`, not already `Bounded`, at any depth (`Vec`, `Tuple`, `Function`, union
   members).
2. For each such return occurrence `R`, find the parameter occurrences **structurally equal** to
   `R`. For data frames, the comparison ignores the row index (`df[#N]{a: int}` and
   `df[#M]{a: int}` match). Field order does not matter.
3. If at least one parameter occurrence matches, rewrite `R` and every matching parameter
   occurrence to `Bounded(id, R)` with one fresh id per distinct `R`, named after the structure
   (e.g. `{a:int}`), so that error messages can print it. If none matches, `R` stays a plain
   record: a new record, as today. This is not an error. A bare record only in the return type was
   already a plain value type, unlike an interface (T017).
4. Bare record occurrences in parameters that no return occurrence matches are left alone. They
   keep today's width subtyping. This is what keeps `fn(x: list{a: int}): int` and every existing
   predicate unchanged.

Explicit tags (`list{a: int}@A`) go through the existing machinery: T047 (one id, two different
bounds) and T048 (id equal to a free generic) apply as is. One more check: an explicit id that
appears **only** in the return type is T017, as for interfaces.

Named aliases are **not** desugared. `Point` stays `Point`; `Point@A` is handled by the existing
`Bounded` path, with a record bound.

### Body (`type_checking/function.rs`)

Each tied id gets one rigid variable, as for interfaces. For a rigid `A` bounded by record `B`:

| Operation on `x: A` | Rule |
|---|---|
| `x$f` / `x.f`, `f` in `B` | type of `f` in `B` |
| `x$g`, `g` not in `B` | `FieldNotFound`, with the help "only the fields of `list{...}` are known here" |
| `x$f <- v`, `f` in `B` | `v` must be a subtype of `B.f`; `x` stays `A` |
| `x$g <- v`, `g` not in `B` (adding a field), `x$f <- NULL` | rejected, by the existing rules for records |
| `list(...x, f = v, …)`, `Self:{ f = v, ...x }` | of type `A` if every explicit field is in `B` and each `v` is a subtype of `B.f`; otherwise a plain record (see *Future possibilities*) |
| `x` passed where `B` (or a supertype) is expected | accepted: `A <: B` |
| `x` passed to a tied parameter of another function | binds that function's id to `A` |
| `B`-typed value returned where `A` is expected | rejected: `B` is not a subtype of `A` (new error, below) |

`is_subtype_raw` already compares rigid variables by name (interface RFC, step 2). It needs one
more rule: `Rigid(A) <: B'` when `B <: B'`, where `B` is `A`'s bound. This rule may already hold
through the interface path; the implementation verifies it.

Spread forms: `list(...x, a = 2)` already parses and type-checks (measured), and
`Self:{ …, ...x }` exists (`generic_constructor.md`, tests `test_self_constructor_*` in
`type_checking/mod.rs`). The new work is typing their result as `A` when `x: A`.

### Call site (`instantiate_at_call`)

For each tied id with bound `B` and argument type `T`:

1. `T` must be a subtype of `B` (width and depth, as today). Otherwise `NoMatchingSignature`, as
   today.
2. **Generalization.** The id is bound to `T'`, built from `T`:
   - every field of `B` takes **`B`'s field type**, whatever `T` had (`a: 1` or `a: int{>0}`
     becomes `a: int`). This is what makes `x$a <- -1` in the body sound: the caller's value is
     never claimed to still satisfy a narrower type the body did not know about;
   - every other field: if `T` is `exact`, kept exactly, and `T'` stays `exact`. Otherwise a literal
     field type is widened to its base type (`true` → `bool`, `1` → `int`, `"x"` → `char`) and a
     refined or user-written type is kept;
   - for a data frame, the row index of `T'` is taken from the **return annotation**, resolved as
     today (`#N` in param and return: same height; `#M` in the return: a fresh height).
3. A repeated id (several parameters) binds to the first argument's `T'`, and every other argument
   must be a subtype of it **and** vice versa (as for interfaces). Otherwise `NoMatchingSignature`,
   with the help "`x` and `y` share one record type; tag them `@A`/`@B` to make them independent".
4. `Bounded(id)` is substituted by `T'` in the parameters and the return type, through `Vec`,
   `Tuple`, `Function` (the existing substitution).

### `exact`

- `exact R <: R'` iff `R <: R'` (width and depth). An exact record is still a record.
- `T <: exact R` iff `T` is `exact` (or a record literal expression, whose inferred type is closed
  by construction), has the same field names as `R`, and each field type is a subtype of `R`'s.
  A non-exact, non-literal type is never a subtype of an exact one.
- An `exact` type in a signature is never desugared, tied or generalized.
- `exact` has no runtime counterpart. `as! exact R` (validating cast) additionally checks
  `setdiff(names(x), <fields>)` is empty in the generated assertion. Without `as!`, nothing is
  emitted.

### Emitted R

Unchanged for everything except `as! exact`. `bump` above compiles to the same R it does today:

```r
bump <- function(x) {
  x$a <- x$a + 1
  x
}
```

`list(...x, a = 2)` and `Self:{ a = 2, ...x }` keep their current transpilation (spread helper /
`.spread =` constructor parameter, `spread_operator3.md`).

### Error messages

New type errors (the next free codes on `develop` when this lands; `T054` is reserved by the
`rust-target` branch):

- **`RecordReturnNotCaller`** (T055, step 4): the message shown in the guide section. Primary label on the
  returned expression ("a new list{a: int}: the caller's other fields are lost"), secondary on the
  tied parameter type. The help always gives both ways out (copy with `...x`, or `exact`).
- **`ExactMismatch`** (T056, step 4): `type list{a: int, b: bool} is not exact list{a: int}: field 'b' is not
  allowed`. Or `…: list{a: int} is open, it may carry other fields`, when an open variable flows
  into an exact slot.
- **`ExactOnNonRecord`** (syntax, S024, implemented in step 1): `exact` must be followed by a record or data-frame type.

The return-type error must print the **id's structure** (`list{a: int}` / `A`), never the
internal rigid name (`__RIGID_1`). That defect is still open in the interface RFC; fixing it is
part of this work.

## Gradual typing

Unannotated parameters stay `Any`, unaffected. A function whose parameters are typed but whose
return type is not annotated infers its return type from the body. If the body returns `x`, the
inferred return is the rigid `A`, so the function is tied without any annotation. If it builds a
new record, it is a plain record, as today. The dial is preserved: the rule only adds information
where the author already wrote a record type on both sides.

## Drawbacks

- **Behavior change.** A function whose return type is written as the same record as a parameter,
  and which builds a fresh record, is now rejected (fix: `exact`, or copy the parameter). Measured
  2026-10-24 over `crates/`, `cases/`, `typr.github.io/docs` and `blog`: **zero** such signatures
  with written-out records. The 166 same-type-in/same-type-out signatures found all use named
  aliases or interfaces, which this RFC deliberately leaves alone. User code may still have some.
- **Two readings of `list{...}`.** In a parameter whose type also appears in the return type, it
  is a bound; elsewhere, a type. The rule is local (look at the return type) and the error
  message names it, but it is a rule to learn.
- **Aliases and literal records differ.** `fn(p: Point): Point` and
  `fn(p: list{x: int, y: int}): list{x: int, y: int}` no longer mean the same thing, though `Point`
  is that record. This is the price of not breaking the constructor idiom (`Point:{...}`), which
  the measurement shows is everywhere.
- **Generalization loses precision** on the bound's fields: a caller passing `a: int{>0}` gets
  back `a: int`. That is the price of a sound body. A caller who needs the precision writes `@A`
  on a function that does not write the field. *Unresolved*: whether a read-only body should keep
  it.
- **A new keyword**, `exact`, with its gloss and grammar regeneration.

## Rationale and alternatives

- **Row polymorphism** (`{a: int | ρ}`). It has the same power for this use case, and is sound for
  field writes by construction, since `a` is fixed to `int`. That is exactly what rule 2
  (generalization) reproduces. It is more powerful for adding/removing fields
  (`{a | ρ} -> {a, c: bool | ρ}`). It was rejected as the *surface*: it exposes a row variable, a
  new concept for R users, and does not share anything with interfaces. This RFC gets
  row-polymorphic semantics with bounded-quantification syntax. Adding fields comes back through
  `{..A, c: bool}` (*Future possibilities*), still without row variables.
- **Pure bounded quantification, no generalization** (TypeScript's `T extends {a: number}`).
  It is unsound: `x.a = -1` in a function bounded by `{a: number}` returns a `T` whose `a` the
  caller believes is `1`. TypeScript accepts this hole; TypR has literal and refined field types,
  so it would show up immediately.
- **Opt-in only (`@A`), bare records unchanged.** No breakage at all, but the default stays the
  information-losing one, and the reuse the feature is for needs an annotation nobody will think
  to write. The measurement (zero affected signatures) removed the main argument for it.
- **One shared id per bare record type in all parameters** (the interface rule, even without a
  return tie). It would make `distance(p: list{x: num}, q: list{x: num}): num` reject two
  differently shaped points, for no benefit. Interfaces need that sharing for `Self`-typed methods
  (`Eq`); records have no such methods.
- **Tying named aliases too.** Rejected after measurement: `fn(p: Point): Point { Point:{...} }` is
  the dominant idiom in tests, snapshots and docs.
- **`list!{...}` instead of `exact`.** `!` is a reserved kind sigil (`sigils.md`) and already means
  "validated" in `as!`. A word reads better to an R user and costs one manifest entry.
- **Doing nothing.** The cost is the motivation: every record-generic helper loses the caller's
  fields, and data-frame transformations stay `-> Any`.

## Prior art

- **R**: lists and data frames are open by nature. `modifyList`, `within`, `transform`,
  `dplyr::filter`/`mutate` all return "the input, plus or minus". R users already expect a
  function on a data frame to give back their data frame, not a stripped one. This RFC types what
  they already do.
- **TypeScript**: `<T extends {a: number}>(x: T) => T`. It has the same surface idea and the
  field-write unsoundness discussed above. `exact` types were requested for years (issue #12936)
  and never added; Flow has them (`{| a: number |}`), and they are widely used there.
- **OCaml / PureScript / Elm**: row polymorphism, extensible records. PureScript's
  `{ a :: Int | r }` is the semantic model reproduced here.
- **Rust**: `impl Trait` vs. named generics: the `@A`/`@B` split mirrors the interface RFC.

## Unresolved questions

To settle before merge:

- Repeated parameters (`pick(x, y)` with a return tie): shared (this text) or an error that
  requires explicit tags?
- Should a body that never writes a bound field keep the caller's precise field types (no
  generalization on read-only functions)? It is more precise, but the signature would then depend
  on the body.

To settle during implementation:

- `exact` as a flag or a wrapper variant (*Representation*).
- Whether `Rigid <: bound's supertypes` already holds through the interface path.
- Interaction with unions of records in a return type (`list{a: int} | null`): the occurrence
  inside the union is tied. Check `Option`-like returns.

## Prerequisites (bug fixes, issues + `cases/`, not part of this RFC)

Found while preparing this RFC (2026-10-24, `develop` @ `a7b8f5f`). **All three fixed on
2026-10-25**, `cases/0128`–`0130`:

- 0128: `is_subtype_raw` no longer lets any type through to a rigid (only the rigid itself and
  `Any`). This exposed `typing_container`, which printed and re-parsed the element type, so
  `[a]` with `a: T` lost its rigid. It now keeps the element type as is.
- 0129: S023 `StructuralBoundSuffix`, fatal.
- 0130: `x$a <- v` on a variable of anonymous record type retypes the field with the widened
  type of `v` (`field_write_retype`), the reassignment rule of cases/0065 applied to a field.
  A named alias keeps its declared type. Nested writes (`x$a$b <- v`) are not retyped yet.

The original reports:

1. **Interface return not checked against the rigid.** Both are accepted:
   `fn(a: Lovable): Lovable { list(name = "Tom") }` and, with `tom: Cat`,
   `fn(a: Lovable@A): Lovable@A { tom }`. The body should be rejected. The record work relies on
   this exact check, so fix it first.
2. **`list{a: int}@A` silently drops `@A`.** Fixed by Syntax §1 above. Until then it should at
   least be S018.
3. **Field write on a literal-typed record.**
   `let x <- list(a = 1, b = true); x$a <- 5; let z: list{a: 1, b: bool} <- x;` is accepted. The
   write is not checked against the field's (literal) type. Either the write is checked, or an
   unannotated `let` of a record literal generalizes its literal fields, as this RFC's call-site
   rule does. That second option is a typing-rule change and would need its own short RFC.

## Future possibilities

- **Extension in the return type**: `fn(d: df[#N]{a: int}): df[#N]{..A, a_sq: num}`, written
  `list{..A, c: bool}`, where `A` is a tied id: "the caller's type plus a field". This is `mutate`.
  Spread exists today only at the value level (`TypeName:{ ...x }`, RFC-TR-033; `list(...x)`);
  a type-level `..A` would be new syntax. Removal
  (`select`) would be `{..A, -c}` or similar.
- **A typed `head`/`subset`/`rbind`/`merge`** in `std/dataframe.ty`, now expressible:
  `@head: (x: df[#N]{}, n?: int) -> df[#M]{}` keeps the caller's columns. That needs the bare `{}`
  bound to tie, which this RFC already allows.
- The same machinery for named aliases by default, if a later measurement shows the constructor
  idiom has moved to `Self:{ ..., ...p }`.

## Implementation plan (for the session that implements this)

Read first: `typR/ai_context/CLAUDE.md`, `rfcs/0000-interface-bound-generics.md` (same machinery),
`typR/ai_context/generic_constructor.md` (`Self:{...}`), `typR/ai_context/spread_operator3.md`.
Pitfalls from the interface work: use `parse_from_string` in tests, not `parse2`. Do not build
tests on records where one extends the other. `npm run check:examples` needs
`TYPR_BIN=target/debug/typr`.

0. ~~Prerequisites~~: done 2026-10-25 (cases/0128-0130). Step 1 removed S023 for records and data frames; `cases/0129` now covers the remaining S023 (`tuple{..}@A`, `[N, T]@A`).
1. ~~Parser~~: done 2026-10-25 (not committed). `structural_bound_suffix` in
   `processes/parsing/types.rs` turns `list{..}@A` / `df[..]{..}@A` into `Bounded(id, record)`
   (tuples and vectors keep S023; `list{..} @A` and `list{..}@Self` are S018). `exact_type`
   parses `exact list{..}` / `exact df[..]{..}` to `Type::Exact(Box<record>, h)`; `exact` on
   anything else is S024 `ExactOnNonRecord` (fatal in `project.rs`, the inner type is kept so
   parsing goes on). `exact` is in the manifest (rule `types.modifier`, scope
   `storage.modifier.typr`, Vim link `StorageClass`) and the grammars are regenerated.
   Parser tests: `test_record_bound_suffix_*`, `test_exact_*`.
   **Interim state:** a signature `fn(x: list{a: int}@A)` parses, but the body does not know the
   bound's fields yet (`x.a` is "Undefined variable"): that is step 4. `Type::Exact` has only
   the plumbing (hash, equality, printer `exact ...`, `to_category`, `get_help_data`); no
   subtyping rule yet.
2. ~~Subtyping for `exact`~~: done 2026-10-25 (not committed), in `Type::is_subtype_raw`
   (`components/type/mod.rs`); `reduce_type_helper` now reduces inside `Exact`. Rules:
   `exact R <: R'` iff `R <: R'` (also towards unions, `Any`, generics); `exact R <: exact R'`
   iff same field names (`closed_field_names`, which sees through a data frame's element
   record) and `R <: R'`; a non-exact type is never `<: exact R` (a union on the left needs
   every member to be; an alias on the left is reduced first). **Representation decided:** a
   wrapper variant `Type::Exact(Box<Type>, HelpData)`, appended last in `Type` (hash tag 46),
   so the `.bin` layout of existing variants does not move and no cache bump is needed.
   Tests: `test_exact_*` / `test_plain_record_is_not_exact` in `components/type/mod.rs`.
   **Not done here, moved to step 4/6:** the "record literal expression is exact" clause. A
   literal's *type* is a plain `Record`, so the type-level rule cannot see it; the check that
   `fn(..): exact list{a: int} { list(a = 0) }` is accepted must look at the returned
   expression (and at an annotated `let`), not at the type. **Still to do for code that
   matches `Type::Record`:** see through `Exact` in field access, `$` and spread on an exact
   value (steps 4-5), since `Type::Exact` is not a `Record` pattern.
3. ~~`desugar_bare_records`~~: done 2026-10-25 (not committed), in `signature_normalization.rs`,
   called at the end of `normalize_signature` after the T047/T048/T017 checks (so those still
   see only what the author wrote). Matching is `same_bare_record`: same field names, each field
   `same_type` (mutual subtyping), order ignored; for data frames the elements are compared and
   the row index is ignored. Ids are `record_id`: the printed record (`list{a: int}`), or
   `df` + the element record for a data frame, so they cannot collide with a user's `@A`.
   A bare record is matched as a unit (its fields are not searched), but an untied one is
   descended into, so a tied record nested in a bigger parameter is still found. Aliases,
   `Exact`, and explicit `@A` are untouched (the rewrite never meets them as `Record`).
   Tests: `record_in_and_out_is_tied`, `…only_in_return_or_only_in_param_is_untied`,
   `repeated_record_shares_one_id…`, `tied_record_is_found_at_depth`,
   `data_frame_rows_are_ignored_when_tying`, `aliases_and_exact_are_left_alone`,
   `explicit_record_tags_are_untouched`.
   **Interim state:** nothing consumes the tied signature yet. `function()` reads only its
   errors, and `instantiate_at_call` exits early on a signature with no alias or `Bounded` in its
   parameters, so a plain `fn(x: list{a: int}): list{a: int}` still behaves as before (width
   subtyping, return type `list{a: int}`). A signature that mixes a tied record with an alias or
   an interface would already bind the record id to the argument's type *without* generalization
   and without the data-frame row rule: that is step 5. Not covered yet: a bare record nested in
   another bare record of the return type (only outermost records are tied).
4. ~~Body~~: done 2026-10-25 (not committed). Most of it already worked through the interface
   machinery: a tied record parameter becomes a rigid whose `interface_constraint` is the record,
   and `record_facet` sees through it, so `x$a`, `x$a <- v` and passing `x` where the bound is
   expected needed no change (`x$g` for `g` outside the bound is `FieldNotFound`, now printing
   the bound, `list{a: int}`, instead of `__RIGID_0`). New:
   - `list(...x, f = v)` is typed `A` (`Lang::List` in `type_checking/mod.rs`) when `x: A`, there
     is exactly one spread, and every explicit field is in the bound with a subtype value;
     otherwise a plain record built from the bound's fields (it used to be `SpreadNonRecord`).
   - `check_return` (`function.rs`) is the single return check, used for the trailing expression
     and for `return`. It accepts a record literal (no spread) for an `exact` return, by closing
     its type, which settles the "literal is exact" clause left open by step 2 for returns.
   - Errors: **T055 `RecordReturnNotCaller`** (a plain record returned where the caller's is
     expected; help gives `...x` and `exact`) and **T056 `ExactMismatch`** (an open record
     returned for an `exact` slot). `T054` is taken on this branch by `NotExportable`.
   Tests: `record_bound_*`, `exact_return_*` in `type_checking/mod.rs`.
   **Not done, moved on:** `Self:{ a = 2, ...x }` is accepted but not typed `A`: it gets the
   constructor's usual alias type (check in step 6 what a caller sees). The "literal is exact"
   clause for an annotated `let` and for call arguments (`f(list(a = 1))` into an `exact`
   parameter) is still to do (step 5/6). The found type in T055 prints the literal's type
   (`list{a: 2}`), not its widening (`list{a: int}`) as in the guide. The call site still does
   plain width subtyping: `f(list(a = 1, b = true))` returned `list{a: int}` until step 5.
5. ~~Call site~~: done 2026-10-25 (not committed), in `signature_normalization.rs`.
   `instantiate_at_call` no longer exits early on a signature with a record or data frame in
   its parameters, and runs when the normalized signature has a `Bounded` with a record bound
   (`tied_records`). `bind_ids` binds such an id through `generalize_to_bound`: the argument is
   reduced, every field the bound names takes the bound's type, every other field has its
   literal type widened (`true` → `bool`), unless the argument is `exact` (then it is kept, and
   the result stays `Exact`). A named alias argument is kept as is when generalizing changes
   nothing (`Cat` stays `Cat`); otherwise the result is the plain record. **Row index
   decision:** instead of reading the return annotation, the substitution gives each
   occurrence of a tied data-frame id the row index of *its own* bound (`with_rows_of`), so
   `df[#N]{a} -> df[#M]{a}` is `df[#3]{a, label}` on the way in and `df[#M]{a, label}` on the
   way out, and two arguments that differ only by height share one id. Repeated ids use the
   existing T049 clash (mutual subtyping after generalization); no dedicated "tag them
   `@A`/`@B`" help yet.
   Tests: `call_returns_the_callers_record`, `call_generalizes_bound_fields_and_literals`,
   `exact_argument_keeps_its_extra_fields_precisely`, `shared_record_id_rejects_different_shapes…`,
   `data_frame_filter_keeps_columns_and_changes_height`, `named_alias_argument_is_kept`.
   **Not done:** an argument that is a bare record *literal* is not treated as `exact` when it
   goes into an `exact` parameter, nor for an annotated `let` (the clause left open in
   steps 2 and 4); `Self:{ a = 2, ...x }` is still not typed `A`; the T049 message for a
   shared record id does not yet suggest explicit tags.
6. ~~Cases~~: done 2026-10-25 (not committed), `cases/0131`-`0138`, all `fixed`, `cmd = check`,
   oracle on the checker's output: 0131 `bump` keeps extra fields; 0132 `bad` (T055 message);
   0133 `exact` return (closed: `r$a` ok, `r$b` refused); 0134 shared id clash (`pick`);
   0135 explicit `@A`/`@B` and untied `pick3`; 0136 data-frame filter keeps columns; 0137
   literal/bound-field generalization; 0138 named alias untied, `Point@A` opt-in. Writing them
   exposed three gaps, fixed here:
   - `$` did not see through `Type::Exact` (`r$a` on an `exact list{a: int}` was "not a record"):
     `dollar_access` and `record_facet` now unwrap it.
   - A record **literal argument** into an `exact` parameter: `function_application` has a third
     attempt that closes spreadless record-literal arguments (the loop also used to `break` when
     the refined-types attempt had nothing to relax, which skipped any later attempt).
   - A record **literal initialising an annotated `let`** (`let z: exact list{a: int} <- list(a = 1)`):
     closed in `let_expression`.
   Tests: `exact_value_fields_are_readable`, `record_literal_flows_into_an_exact_slot`.
   **Still not done:** `Self:{ a = 2, ...x }` is accepted but not typed `A` (no case covers it;
   0138 only checks it is accepted); T049's message for a shared record id does not suggest
   `@A`/`@B`; T055 prints the literal's type (`list{a: 2}`), not its widening.
7. ~~Docs~~: done 2026-10-25 (not committed in `typr.github.io`). `syntaxe.md` (both copies: new
   subsection "Records qui gardent le type de l'appelant" after Records), `docs/reference/types.md`
   (section *Records that keep the caller's type*, with the `compile_fail` block for `bad`) and
   `docs/reference/records.md` (short section pointing to it). `npm run check:examples`
   (`TYPR_BIN=<abs path>/target/debug/typr`; a relative path fails with ENOENT): 202 blocks and 20
   counter-examples behave as announced. **Not done:** the gloss for `exact` (see checklist), a
   dedicated concept page (the two reference pages carry it).
8. ~~Verification~~: done 2026-10-25. `RUST_MIN_STACK=8388608 cargo test --workspace` all green
   (without it, `standard_library::tests::all_hash_bang_examples_are_typecheckable` overflows the
   stack), `typr case run` no REGRESS (PASS=111), `typr syntax --check` up to date.

**Left open after the plan:** `Self:{ a = 2, ...x }` typed `A`; T049 help suggesting `@A`/`@B`;
T055 printing the widened type; the two *Unresolved questions* above; `Status:` and
`Implemented in:` are filled at release.

## Implementation checklist

- [x] `cases/` covering the new behaviour (0131-0138, step 6)
- [x] `typr syntax --write` (`exact` is a new lexeme), done with step 1
- [x] `syntaxe.md` updated in both copies (step 7)
- [ ] `scripts/syntax-glossary.mjs` gloss for `exact` (`typr.github.io`). Add it **with the release** that ships the refreshed `typr.syntax.json`: the generator fails on a gloss with no lexeme, so adding it earlier breaks the site build
- [ ] Documentation PR on `we-data-ch/typr.github.io`, landing in the same release
- [ ] `Implemented in:` filled in above
