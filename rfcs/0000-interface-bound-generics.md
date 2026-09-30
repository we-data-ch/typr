- **Status:** implemented (branch `develop`, not yet merged)
- **RFC PR:** we-data-ch/typr#0000
- **Tracking issue:** —
- **Implemented in:** not yet released
- **Start date:** 2026-09-30

# Implicit generics on interfaces: `Interface@Id`

## Summary

An interface name in a parameter type can carry a suffix `@Id` (`Lovable@A`) that names the hidden
type variable standing for "some type satisfying `Lovable`". Two parameters written `Lovable@A` and
`Lovable@B` are independent; two written `Lovable@A` share one type; the return type can name the
variable it comes from. A bare `Lovable` is sugar for `Lovable@Lovable`: **one variable per
interface**. No `T`/`U` is needed. The emitted R does not change.

## Motivation

Two parameters of the same interface used to behave differently in the body and at the call site:

```typr
let second <- fn(a: Lovable, b: Lovable): Lovable { b };
let r: Cat <- second(cat, dog);   # accepted; `let r: Dog <- ...` was rejected ("Received Cat")
```

- In the body, `a` and `b` were two distinct rigid variables.
- At the call site, one variable was shared, and the first argument fixed it. The return type was
  bound to the first argument, and `b: Dog` was never checked against `a: Cat`. This is a soundness hole.
- `fn(a: Lovable@A, b: Lovable@B)` was accepted and the `@A` silently dropped.
- `type Same <- interface { same: (Self, Self) -> bool }; fn(a: Same, b: Same) { a.same(b) }` was
  accepted with two distinct rigid variables, because `PartialEq for Type` treats any two
  `Generic` as equal.

## Guide-level explanation

```typr
let cmp    <- fn(a: Lovable@A, b: Lovable@B): bool { a.love() == b.love() };  # cmp(cat, dog): ok
let second <- fn(a: Lovable@A, b: Lovable@B): Lovable@B { b };               # second(cat, dog): Dog
let same   <- fn(a: Lovable, b: Lovable): bool { ... };                      # same(cat, dog): error
```

Teaching line for an R user: "if you want two parameters to be independent, give each a small tag,
`@A`, `@B`". Same tag, same type. No tag, one shared tag per interface.

## Reference-level explanation

**Syntax.** `@Id` is written immediately after an interface alias, no space. `Id` is one uppercase
letter, or `_` for a fresh variable at each occurrence. It can be nested (`[#N, Eq@A]`,
`(Lovable@A) -> ...`). A malformed suffix (`Lovable @A`, `Lovable@Self`, `Lovable@Abc`) is a syntax
error S018 (`DetachedBoundSuffix`), never silently ignored. The prefix `@A` (a generic of kind
interface) is unchanged; `@` is already in the syntax manifest, so no lexeme is added.

**Representation.** `Type::Bounded(id, bound, help_data)`, last variant of `Type` (binary
serialization). It exists only in signatures.

**Typing.**
1. *Normalization* (`type_checking/signature_normalization.rs`): `I@_` gets a fresh id; a bare
   interface alias becomes `Bounded(I, I)`. Errors: T047 two different bounds for one id; T048 an id
   equal to a free generic of the signature; T017 an id only in the return type.
2. *Body* (`function.rs`): one rigid variable **per id**. A return `Lovable@B` accepts only the
   rigid of `B`. Two distinct rigid variables are no longer subtypes of each other
   (`is_subtype_raw` compares rigid names; `PartialEq for Type` is untouched, unification depends on it).
3. *Call site* (`instantiate_at_call`): each id is bound to its argument's type (the bound must be
   satisfied; a repeated id requires mutual subtyping), then `Bounded(id)` is substituted in the
   parameters and the return type, under `Vec`, `Tuple` and `Function`.

**Emitted R.** Unchanged (same S3 dispatch on the interface name, same `--checked` assertions). Tested.

**Error messages.** A call whose ids cannot be bound reports `NoMatchingSignature`, listing the
declared signatures. A dedicated "`A` bound to `Cat` then `Dog`" message is not written yet.

## Gradual typing

Unaffected: the feature only applies to interface-annotated parameters. Unannotated parameters stay `Any`.

## Drawbacks

- **Behavior change.** A function with two bare parameters of the same interface, called with two
  different concrete types, used to compile and is now rejected. The measured corpus (tests, cases,
  docs, blog, standard library) has none, but user code may.
- One more piece of syntax next to `T`, `#N`, `@I`.

## Rationale and alternatives

- **Bare `I` = fresh variable per occurrence** (rejected). It makes the return type ambiguous with
  two parameters, and is unsound for `(Self, Self)` interfaces (`Eq`, `Ord`).
- **`where A: I`** (rejected): needs explicit `T`, which is what the design avoids.
- **`Lovable#Id`** (rejected): `#` already prefixes index generics.
- **Extend `KindedGen`** (rejected): its key is a `Kind`, not a type; the bound must travel with the
  type through substitution.
- **A unification arm for `Bounded`** (planned, then dropped): substituting per signature before the
  existing filters is simpler and reuses them unchanged.

## Prior art

Rust `impl Trait` in argument position (independent anonymous types) versus `<T: Trait>` (shared);
Haskell class constraints; TypeScript `T extends I`. R itself has no static counterpart (S3/S4 dispatch
is dynamic), so R users have no existing expectation to break.

## Unresolved questions

- `swap` with a `tuple{...}` return, `[#N, Eq@A]` elements and lambda parameters have no dedicated test.
- `try_interface_subtype_match` is kept for a single bare interface parameter (`unique`, `sort`); it could be removed.
- The rigid name shows up in a return-type error (`__RIGID_1`) instead of the id.
- Parametrized interfaces (`interface<T>`, `Box@A`) are not parsed today.

## Future possibilities

Parametrized interfaces: `Bounded` already accepts a bound like `Box<T>` without a change of representation.

## Implementation checklist

- [x] `cases/0086`, `0087`, `0088`
- [x] `typr syntax --check` (no lexeme changed)
- [x] `syntaxe.md` updated (only the `typr.github.io` copy exists)
- [ ] Documentation PR on `we-data-ch/typr.github.io`, landing in the same release
- [ ] `Implemented in:` filled in above
