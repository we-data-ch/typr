- **Status:** draft
- **RFC PR:** we-data-ch/typr#0000
- **Tracking issue:** —
- **Implemented in:** not yet
- **Start date:** YYYY-MM-DD

# Title

<!--
  Copy this file to `rfcs/0000-my-feature.md` and open a PR against `develop`.
  Keep the `0000` — the number is the PR's own number, assigned at merge.
  Delete the guidance comments as you fill each section; delete a whole section
  only if it genuinely does not apply, and say why in one line.
  Read `rfcs/README.md` first.
-->

## Summary

<!-- One paragraph. What changes, in a sentence a TypR user would understand. -->

## Motivation

<!--
  What can't be done today, or what goes wrong today. Prefer a real snippet over
  an abstraction: the code someone tried to write, and what the compiler said.
  If this comes out of a discussion or an issue, link it.
-->

## Guide-level explanation

<!--
  Explain it as if it were already shipped and you were writing the
  documentation page. Vocabulary a user would meet, examples they would copy.

  ```typr
  # what the feature looks like in use
  ```

  Say explicitly how you would teach it to someone who knows R but not TypR.
-->

## Reference-level explanation

<!--
  The part a maintainer implements from. As applicable:

  - **Syntax.** The exact forms accepted, and the lexemes added or changed in
    the syntax manifest (`crates/typr-core/src/components/syntax/mod.rs`). Any
    ambiguity with existing forms, and how it resolves.
  - **Typing rules.** Inference, unification, subtyping, dispatch. What is
    rejected, and at which stage — parsing or type checking.
  - **Interaction with what already exists.** Generics and kinds, unions and
    tags, records, interfaces, modules, the escape hatches, the standard library
    preload (`processes/spg`).
  - **Emitted R.** The generated code for each example above. This is a
    superset of R that compiles to readable R — if the output stops being
    readable, that is a cost to state here.
  - **Error messages.** What the compiler prints when the feature is misused.
    Write the message, not a description of it.
-->

## Gradual typing

<!--
  What does unannotated code do under this proposal? A feature that only makes
  sense when everything is annotated forces the dial to one end, which is a
  design decision and needs to be argued as one.
-->

## Drawbacks

<!-- Why we might not want to do this. Write the strongest version. -->

## Rationale and alternatives

<!--
  Why this design and not the neighbouring ones. What is the cost of doing
  nothing — sometimes the honest answer is "small", and that is worth writing
  down.
-->

## Prior art

<!--
  R itself first (S3/S4/R6/S7, existing packages, what R users already expect),
  then other languages: Rust, TypeScript, Julia, Python. Both what worked and
  what those communities regretted.
-->

## Unresolved questions

<!--
  What this RFC deliberately leaves open, and what should be settled before
  merge versus during implementation.
-->

## Future possibilities

<!-- What this makes possible later. Explicitly out of scope for this RFC. -->

## Implementation checklist

<!-- Filled in as the work lands, after acceptance. -->

- [ ] `cases/NNNN-slug/` covering the new behaviour
- [ ] `typr syntax --write` if a lexeme changed (grammars are generated)
- [ ] `syntaxe.md` updated in both copies (this repo and `typr.github.io`)
- [ ] Documentation PR on `we-data-ch/typr.github.io`, landing in the same release
- [ ] `Implemented in:` filled in above
