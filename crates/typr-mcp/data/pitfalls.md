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

## Alias a type as soon as it's used in a function's first parameter

An inline structural type (`list{...}` or `:{...}`) that appears as a function's **first**
parameter should be given a `type` alias, even if it's only used once. It reads better at the
call site, names the generated R constructor/validator (`as.T`/`validate_T`) instead of leaving
it anonymous, and lets UFCS calls (`point.add(other)`) read like a method on a real type instead
of on a shape:

```typr
# Avoid
let add <- fn(point: list{val: int, name: char}, other: int): int { point$val + other };

# Prefer
type Point <- list{val: int, name: char};
let add <- fn(point: Point, other: int): int { point$val + other };
```

Once a shape has an alias, reuse it for every other function whose first parameter has that
same shape rather than repeating the inline structural type.
