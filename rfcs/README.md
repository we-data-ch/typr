# TypR RFCs

Most changes reach TypR through an issue and a pull request. Some should not.
A change to the **language** — new syntax, a new typing rule, a different shape
of generated R — is hard to take back once people have written code against it.
Those go through an RFC first: a written proposal, reviewed in the open, merged
into this directory once it is accepted.

The point is not ceremony. It is that the *reasoning* survives: six months later
the question "why does TypR do it this way?" has a written answer instead of a
recollection.

## Do I need one?

| What you have | Where it goes |
|---|---|
| A snippet the compiler mishandles | an [issue](https://github.com/we-data-ch/typr/issues), then a `cases/` entry with the fix |
| A vague "it would be nice if…" | [Discussions → Ideas](https://github.com/we-data-ch/typr/discussions/categories/ideas) |
| A question about how to write something | [Discussions → Q&A](https://github.com/we-data-ch/typr/discussions/categories/q-a) |
| A doc page that is wrong or missing | a PR on [`typr.github.io`](https://github.com/we-data-ch/typr.github.io) |
| **A change to what the language means** | **an RFC — this directory** |

An RFC is needed when the change:

- adds or alters syntax the parser recognises — i.e. it touches the syntax
  manifest (`crates/typr-core/src/components/syntax/mod.rs`);
- adds or alters a typing rule: inference, unification, subtyping, dispatch,
  what is an error and what is not;
- changes the R emitted for code that already compiles;
- changes a published contract: the CLI, the project layout, the `.ty` ↔ R
  package interface;
- removes anything at all.

No RFC is needed for a bug fix (the compiler failing to do what the
documentation already says), performance work, internal refactoring, wording of
an error message, new `cases/`, editor integrations, or documentation.

The dividing line: **if the answer to "what does TypR do here?" changes, it is an
RFC. If the compiler is catching up with an answer that was already given, it is
an issue.**

When in doubt, open a discussion in **Ideas**. Half of those get answered there;
the other half become better RFCs than they would have been.

## The process

1. **Float the idea first**, in Ideas. A cold RFC is almost always the slower
   path — it collects on the second read the objections a thread would have
   surfaced in a day.
2. Copy `0000-template.md` to `rfcs/0000-my-feature.md`. Keep the `0000`: the
   number is assigned at the end, not at the start.
3. Open a pull request against `develop`. **The PR is where the discussion
   happens** — comment threads land on the actual sentences, which is exactly
   what a mailing list cannot do.
4. A maintainer labels it `rfc-draft`. Revise in place; force-pushing over
   review comments loses them, so prefer new commits.
5. It ends one of three ways:

| Outcome | Label | What happens to the text |
|---|---|---|
| Accepted | `rfc-accepted` | renamed `NNNN-my-feature.md`, header filled in, merged |
| Declined | `rfc-rejected` | PR closed. The text and the reasoning stay readable in the closed PR — that record is the point, do not delete it |
| Not now | `rfc-draft` | the PR stays open, with a comment saying what it is waiting for |

**`NNNN` is the pull request's own number**, zero-padded to four digits. Nothing
to reserve, nothing to renumber, and two proposals cannot race for the same
slot.

**This directory is the accepted set.** A file in `rfcs/` is a decision that was
taken; an open PR is a decision being taken; a closed one is a decision that was
declined. That invariant is what lets the documentation site list accepted RFCs
without maintaining a second list by hand.

## Accepted is not implemented

Merging an RFC settles the design, not the code. Nobody is assigned by the merge
— including the author, who is welcome to implement it and equally welcome not
to.

After a merge:

- a tracking issue is opened and its number goes into the RFC header;
- the implementation follows the usual rules — a `cases/` entry for the new
  behaviour (a change with no case behind it has nothing stopping it from
  regressing), `typr syntax --write` if a lexeme moved, and `syntaxe.md` updated
  in **both** copies (this repo and `typr.github.io`);
- the documentation PR on `typr.github.io` should land in the same release, not
  the one after. Its example blocks are compiled against the real binary in CI,
  so a feature that is documented before it ships fails that repository's build
  — which is the intended order, not an obstacle;
- when it ships, `Implemented in:` in the header gets the version.

An accepted RFC that has sat unimplemented for a year is a design nobody wanted
enough. Say so in a follow-up PR that marks it superseded, rather than leaving
it to look like a promise.

## What makes a TypR RFC convincing

This is the part that is not a copy of Rust's process, because these are TypR's
constraints:

- **The output stays plain, readable R.** No runtime, no companion library
  shipped alongside the generated code, nothing an R user reading `R/` cannot
  follow. If the feature needs a helper, show the helper.
- **R that already works keeps working.** TypR is a superset; a superset that
  keeps breaking its base is a dialect.
- **Say what happens without annotations.** Typing is a dial, so every proposal
  has to answer what the unannotated version of the code does — not just the
  fully typed one.
- **Error messages are part of the design.** Show what the compiler prints when
  someone uses the feature wrong. A rule whose violation cannot be explained in
  three lines is usually the wrong rule.
- **Show the emitted R.** Two proposals that type-check identically can generate
  very different R, and that difference is often the real decision.

## Where the existing design notes fit

A large body of design reasoning already exists in the maintainers' workspace
(`spécifications/`, `ai_context/*.md`) — constructors, generics, sigils, the
spread operator, soundness. Those notes are working material, often in French,
and they are not the public record. When one of those questions is reopened, the
RFC is what it becomes: the same reasoning, in English, with the decision at the
top.

## Header of an accepted RFC

Every merged RFC starts with the block from `0000-template.md`:

```markdown
- **Status:** accepted
- **RFC PR:** we-data-ch/typr#42
- **Tracking issue:** we-data-ch/typr#57
- **Implemented in:** 0.6.0   <!-- or: not yet -->
- **Start date:** 2026-09-09
```

`Status` is one of `draft`, `accepted`, `superseded`. `rejected` never appears
here — a rejected RFC is not in this directory.
