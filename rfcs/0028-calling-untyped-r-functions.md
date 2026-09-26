- **Status:** accepted
- **RFC PR:** we-data-ch/typr#28
- **Tracking issue:** we-data-ch/typr#29
- **Implemented in:** not yet
- **Start date:** 2026-09-09

# Calling untyped R functions

## Summary

An untyped R function — `let f <- function(a, b) { a + b };` — can be defined in
TypR today but not called: any call with at least one argument is a type error.
This RFC asks whether that is the intended semantics, and proposes that such a
function become callable with its arguments unchecked and its result typed
`Any`, with arity checked when it is known.

## Motivation

The documentation presents the untyped form as the low-safety end of TypR's
gradual dial. `docs/philosophy/intro.md` shows this block, commented *"Also a
valid TypR code: weak on safety, strong on freedom"*:

```typr
let num1 <- 3;
let num2 <- 7;

let my_addition <- function(a, b) {
	a + b
};

my_addition(num1, num2)
```

It does not compile. Verified against `typr` 0.5.10:

```
Type error: No signature of function 'my_addition' matches this call.
  Called with 2 argument(s): (int, int)
help: 'my_addition' exists but none of its signature(s) accepts these arguments:
          () -> UnknownFunction
```

The definition is accepted; the call is not. Three further facts, all verified
on 0.5.10:

- **The placeholder has arity zero.** `let f <- function() { 1 }; f();` passes.
  Any call with arguments fails, because `Type::UnknownFunction` carries no
  parameter list — even though `Lang::RFunction` parsed one.
- **The same hole affects preloaded base-R builtins.** The untyped names from
  `functions_R.txt` (`Position`, `t`, `Reduce`, …) are preloaded as
  `(Any, UnknownFunction)` so that referring to them is not an "undefined
  variable" error. `Position()` type-checks; `Position(1, 2)` fails with the
  same message. So this is not only about user-written `function(...)`: it is
  every untyped R name the standard preload knows about.
- **The emitted R is already correct.** `let f <- function(a) { a + 1 };`
  transpiles verbatim to `` `f` <- function (a) { a + 1 } ``. Nothing about the
  output blocks the call — only the type checker does.

> ⚠ A trap when reproducing this: test with an invented name. A first check
> using `add` appeared to pass, because `add` exists in the standard library
> with real signatures and the local definition merely shadowed it.

So the situation is not a design that was argued and written down; it is a
placeholder type that never grew a parameter list. Which means the question is
open, and it has to be answered before the documentation can be corrected — the
page cannot say "valid TypR" while the compiler says otherwise, and it cannot
be quietly demoted either, since it carries the gradual-typing argument that is
the page's whole thesis.

## Guide-level explanation

Under this proposal, `function(...)` stays what `syntaxe.md` §12 says it is — an
escape hatch, alongside `R { ... }` and `extern` — but it becomes a *callable*
one:

```typr
let my_addition <- function(a, b) {
	a + b
};

let total <- my_addition(3, 7);   # accepted; total: Any
```

The rule to teach is one sentence: **TypR checks that you called it with the
right number of arguments, and nothing else.** The result is `Any`, so to use it
as a number you say so:

```typr
let total: int <- my_addition(3, 7) as! int;
```

That cast is the honest bookkeeping of the trade: nothing verified the body, so
the type at the boundary is an assertion by the author, marked as such. It is
the same shape as the rest of TypR's opaque values (`Foreign<Any>`, `as!`), not
a new concept.

For the reader of the philosophy page, this is the sentence that becomes true:
you can write plain R inside TypR, use it, and pay for it only at the point
where you want the type back.

## Reference-level explanation

**Syntax.** None. `function(...)` already parses to `Lang::RFunction`, which
already carries `parameters`.

**Typing rules.**

1. `Lang::RFunction` with parameters `p₁…pₙ` is typed as a function
   `(Any, …, Any) -> Any` with *n* parameters, instead of the current
   `Type::UnknownFunction` with none.
2. Applying it checks arity only, as the literal count of parsed parameters
   (`parameters.len()`) — `Lang::RFunction` carries no default values and no
   `...` today (see "Resolved during review" below), so there is no case to
   special-case yet. Argument types are not checked and not propagated.
3. The result type is `Any`.
4. Preloaded untyped builtins keep `Type::UnknownFunction`, since
   `functions_R.txt` gives no arity, but `UnknownFunction` becomes **variadic**:
   it accepts any number of arguments and returns `Any`. (Today it is
   effectively 0-ary, which is why `Position(1, 2)` fails.)

Point 4 is the load-bearing half. Point 1 without point 4 leaves the same error
message reachable through every untyped base-R name, which is the confusing
version of the bug rather than a fix.

**Interaction.** `Any` is already opaque: `let y: int <- x;` on an `Any` fails
with `type int doesn't match type any`, `x + 1` finds no `+` signature, and
`x as! int` is the way through. Nothing here changes that; the proposal only
produces more `Any` values, at a place where the user asked for exactly that.

Dispatch is unaffected: TypR dispatches same-named functions on the first
parameter's type, and an untyped R function offers no type to dispatch on. It
shadows, it does not overload.

**Emitted R.** Unchanged — a call to an R function is already just a call.

**Error messages.** The remaining error is arity, and it should say what it is
rather than reusing the signature-mismatch wording:

```
Type error: 'my_addition' is an untyped R function taking 2 argument(s), called with 3.
  help: its body is not type-checked; only the number of arguments is.
```

The current message — *"none of its signature(s) accepts these arguments:
`() -> UnknownFunction`"* — should disappear entirely: it exposes an internal
placeholder to someone who wrote perfectly ordinary R.

## Gradual typing

This is the proposal's whole subject. Today the dial has two notches, not three:
full annotation, and inference (`let x <- 3;` works and infers `int`). The third
— borrow R wholesale — is defined but unusable, so the untyped end of the dial
is a cliff. This restores it, and makes the cost visible in one place: the `as!`
you write when you want the value back inside the type system.

## Drawbacks

- **`Any` propagates.** Every call to an untyped function yields a value that
  can only be cast, not used, and a codebase that leans on this ends up
  scattering `as!`. That is arguably the correct pressure — the escape hatch
  should be mildly uncomfortable — but it is a real cost.
- **It weakens a guarantee that is currently, if accidentally, strong.** Today
  no untyped call passes the type checker at all. This deliberately opens a hole
  and calls it a feature.
- **Arity is checked, which may surprise R users.** R's own arity rules are
  looser (partial matching, `...`, missing arguments with defaults). See
  "Defaults and `...`" below for why this drawback does not currently bite: the
  syntax that would trigger it isn't parseable yet.

## Rationale and alternatives

**Option 1 — the documentation aligns instead.** Keep `function(...)`
non-callable and rewrite the philosophy page: the untyped form is an escape
hatch for *defining* R that other R consumes, and the gradual-typing argument is
rebuilt on inference, which genuinely works. Cheapest, honest, and loses
something real — "you can use plain R here" stops being true at the moment it
matters most, the call.

**Option 2 — fully unchecked calls.** Accept any call on an `UnknownFunction`,
any arity, result `Any`. Simpler than this proposal by exactly one rule, and it
throws away arity information the parser already has, for nothing.

**Option 3 — infer the body.** Type-check the body of `function(...)` and give
it a real signature. That is not an escape hatch any more; it is a second
function syntax with inference, and it collides with `fn` being the typed form.
If body inference is wanted, it should be an RFC about `fn`, not about the
escape hatch.

**Doing nothing** leaves `docs/philosophy/intro.md` in its current state: a
central page whose main example is marked as excluded from the documentation
site's example checks precisely because it does not compile.

## Prior art

- **TypeScript** — `any` is exactly this bargain: calls through `any` are
  unchecked, and the type system's job is to make the boundary visible rather
  than to forbid it. The lesson usually reported is that the escape hatch must
  be *named*, so tooling can find it; `as!` and `Any` already are.
- **R itself** — no arity guarantee at all before the call, which is one of the
  fragilities TypR exists to reduce. Checking arity while ignoring types is
  strictly more than R offers, and less than TypR offers elsewhere. That
  in-between position is the thing to argue about.
- **Julia** — untyped arguments are the default and dispatch happens at runtime;
  not transferable, since TypR must decide statically and emit R.
- **`extern`** — TypR's own precedent, and the counter-argument to this whole
  RFC: it already gives a way to call untyped R with a *declared* signature. The
  question is whether the undeclared case deserves an answer too.

## Resolved during review

These three questions were open when this RFC was drafted. Checked against the
compiler (0.5.12):

- **Defaults and `...`.** Neither exists in `Lang::RFunction` today. Its
  `parameters: Vec<Lang>` is built by the parser's generic `variable` combinator
  (`processes/parsing/elements.rs`), which accepts a bare name plus an optional
  `: Type` annotation — no `= expr` default syntax, no special-casing of `...`.
  `function(a, b = 2)` and `function(...)` are not parseable as an `RFunction`
  today; there is nothing for arity checking to get wrong, because the grammar
  gives it nothing but a plain count of bare names. So point 2 of the
  Reference-level explanation is exactly `parameters.len()`, no fallback needed.
  Adding default-parameter or dots syntax to the escape hatch is a separate,
  larger change (it touches the syntax manifest) and its own RFC if ever
  proposed; this one does not depend on it.
- **`Any`, not `Foreign<Any>`.** `Foreign<T>` is for a value TypR acknowledges
  is opaque *and* gives an accessor story for. An untyped call's result could be
  anything a plain R function returns — vector, list, S3 object — and forcing it
  through `Foreign` would imply an accessor contract this RFC does not define.
  `Any` is the honest choice, and it composes with the rest of this proposal:
  a future package-registry design that degrades low-trust or generated
  signatures to "callable, unchecked" (see `Type::UnknownFunction` above) needs
  exactly this same fallback shape, so settling on `Any` here keeps that door
  open instead of introducing a second opaque-value convention to reconcile
  later.
- **`R { ... }` blocks: out of scope, and for a concrete reason.** They are
  *not* already `Any` — the type checker assigns them `Type::Empty`
  (`processes/type_checking/mod.rs`), a distinct placeholder from
  `Type::UnknownFunction`/`Type::Any` with its own, more permissive unification
  rule (`Type::Empty` unifies with anything; `Type::Any` only satisfies a
  target that is itself `Any`). Folding `R { ... }` into this RFC's `Any`/`as!`
  story would be a behavior change to existing code, not a documentation
  footnote, since it would make `R { ... }` results stop unifying freely with
  concrete types. Left for a separate RFC if ever proposed.

## Future possibilities

A lint — "this call is unchecked" — reported at a level the user can turn on,
which would make the escape hatches auditable in a package before it ships. Out
of scope here.

## Implementation checklist

- [x] `cases/` entry: untyped `function(a, b)` defined, called, and cast
      (`cases/0062-untyped-r-function-callable`)
- [x] `cases/` entry: arity error message (`cases/0063-untyped-r-function-arity-error`)
- [x] `cases/` entry: preloaded builtin called with arguments (`Position(1, 2)`)
      (`cases/0064-untyped-preloaded-builtin-variadic`)
- [x] `syntaxe.md` §12 updated (`typr.github.io/syntaxe.md`; no second copy
      exists under `typr/` yet)
- [x] `docs/philosophy/intro.md` and `docs/reference/escape-hatches.md` updated
      on `typr.github.io`; the `noplayground` marker on the philosophy block
      removed — both blocks verified with `typr check` (0.5.12+d85f8d9) and
      `npm run check:examples`
- [ ] `Implemented in:` filled in above — pending a release past 0.5.12; the
      implementing commit (`d85f8d9`) is on `develop`, not yet tagged
