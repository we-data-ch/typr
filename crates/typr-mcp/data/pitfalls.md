---
description: "Common pitfalls and ambiguities in the TypR parser."
---

# Known Pitfalls

This page documents common pitfalls and ambiguities in the TypR parser.

<!-- truncate -->

:::note
This page is under construction. See the full details in the
[syntaxe.md](https://github.com/we-data-ch/typr/blob/main/syntaxe.md#14--ambiguïtés--pièges-connus-du-parseur) reference.
:::

## Semicolon swallowing

Missing a `;` between two instructions could cause the second instruction to be silently absorbed into the first. This has been fixed. The parser now emits a `ForgottenSemicolon` warning.

## `//` is not a comment

TypR only recognizes `#` for comments. A `//` is now caught by a dedicated parser and treated as a comment with a `WrongCommentSyntax` warning.

## Single `=` is never a binary operator

Use `==` for equality testing. A single `=` is only valid in named fields, top-level assignment, and default parameter values.

## `record` vs `tuple`, the shape decides

`list{...}` and `:{...}` produce either a record or a tuple based on the shape of the elements, not the keyword.

## Type aliases need 2+ characters

A single-letter PascalCase alias (e.g., `type A <- int;`) is not parseable. Use at least 2 characters (e.g., `type Ab <- int;`).
