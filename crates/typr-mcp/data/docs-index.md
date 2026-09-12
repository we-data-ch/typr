# TypR Documentation Index

Every page on `typr.github.io/docs/`, one row each: title, Diátaxis category, one-line description, and canonical URL. Use this to find which page answers a question that `check`/`build`/`explain` and the other resources here can't — then point the user at the URL (or fetch it, if the client has a web tool) instead of guessing.

| Title | Category | Description | URL |
|---|---|---|---|
| Getting started | Tutorial | Learn TypR in about ten minutes by writing and running small typed programs, step by step. | https://we-data-ch.github.io/typr.github.io/docs/intro |
| FAQ | Reference | Frequently asked questions about TypR for R users. | https://we-data-ch.github.io/typr.github.io/docs/faq |
| Types for Beginners | Tutorial | Why types are worth it, explained without jargon: what a type system remembers for you, and when it is overkill. | https://we-data-ch.github.io/typr.github.io/docs/tutorials/typr-for-dummies |
| Create your first TypR package | Tutorial | Build a complete R package with TypR, from an empty folder to an installable package with types, tests, and documentation. | https://we-data-ch.github.io/typr.github.io/docs/tutorials/first-package |
| Migrate an existing R package | Tutorial | Add TypR to an R package you already have, one file at a time, without rewriting it. | https://we-data-ch.github.io/typr.github.io/docs/tutorials/migrate-r-package |
| Model data with TypR types | Tutorial | Model real data with records, unions, interfaces, and generics by building a small contact-list library. | https://we-data-ch.github.io/typr.github.io/docs/tutorials/typed-data-modeling |
| Type existing R functions | How-To | Add type safety to R functions you already have — base R, your own helpers, or third-party packages — with @ signatures. | https://we-data-ch.github.io/typr.github.io/docs/howto/type-r-functions |
| Interop with R6/S4/RC | How-To | Use R6, S4, and Reference Class objects from typed TypR code. | https://we-data-ch.github.io/typr.github.io/docs/howto/interop-r6-s4 |
| Use dplyr/tidyr from TypR | How-To | Call dplyr, tidyr, and the rest of the tidyverse from typed TypR code. | https://we-data-ch.github.io/typr.github.io/docs/howto/use-dplyr-tidyr |
| Build, test & document | How-To | The full development workflow of a TypR package: building, testing, and documenting it. | https://we-data-ch.github.io/typr.github.io/docs/howto/build-and-test |
| Use raw R blocks | How-To | Write plain R from TypR with raw R blocks and the other escape hatches, and know when to reach for them. | https://we-data-ch.github.io/typr.github.io/docs/howto/r-raw-blocks |
| Declare S3/S4 generics | How-To | Declare the types of R's S3 and S4 generic functions so TypR can check calls into R's object systems. | https://we-data-ch.github.io/typr.github.io/docs/howto/generics-signatures |
| Connect an AI assistant via MCP | How-To | Give Claude Code, Claude Desktop, Cursor or any MCP client direct access to the TypR compiler, so it checks its own generated code instead of guessing. | https://we-data-ch.github.io/typr.github.io/docs/howto/mcp-server |
| Type Shiny apps | How-To | Type reactive values and modules in a Shiny app built with TypR. | https://we-data-ch.github.io/typr.github.io/docs/howto/shiny-integration |
| Reference | Reference | How the reference is organised, and what to read to understand the type system and the main language constructs. | https://we-data-ch.github.io/typr.github.io/docs/reference/intro |
| Installation | Reference | Install the TypR compiler as a complement to an existing R installation, and verify it works. | https://we-data-ch.github.io/typr.github.io/docs/reference/installation |
| Editor Setup | Reference | Set up TypR in VS Code, RStudio, Positron, or Vim/Neovim, all driven by the typr CLI and the typr lsp language server. | https://we-data-ch.github.io/typr.github.io/docs/reference/editor-setup |
| Lexicon & Literals | Reference | The fundamental tokens and literal types of TypR. | https://we-data-ch.github.io/typr.github.io/docs/reference/lexicon |
| Bindings & Mutation | Reference | How variables are declared, destructured, and reassigned in TypR. | https://we-data-ch.github.io/typr.github.io/docs/reference/bindings-mutation |
| Types | Reference | A comprehensive overview of the TypR type system. | https://we-data-ch.github.io/typr.github.io/docs/reference/types |
| Functions | Reference | Function definitions, calling conventions, signatures, and advanced patterns. | https://we-data-ch.github.io/typr.github.io/docs/reference/functions |
| Records & Constructors | Reference | Record types, constructors, the spread operator, and named type embedding. | https://we-data-ch.github.io/typr.github.io/docs/reference/records |
| Unions, Tags & Pattern Matching | Reference | Tagged union types and the match expression. | https://we-data-ch.github.io/typr.github.io/docs/reference/unions-patterns |
| Operators & Precedence | Reference | Every operator in TypR and its precedence rules. | https://we-data-ch.github.io/typr.github.io/docs/reference/operators |
| Define an Option type with generics | Reference | if/else, for/while, and match expressions, and the type safety they add over plain R. | https://we-data-ch.github.io/typr.github.io/docs/reference/control-flow |
| Modules & Imports | Reference | Organise code into modules and import symbols across files. | https://we-data-ch.github.io/typr.github.io/docs/reference/modules |
| Interfaces & Structural Validation | Reference | Interfaces: describing structural capabilities without modifying the original types. | https://we-data-ch.github.io/typr.github.io/docs/reference/interfaces |
| Signatures, @extern & Foreign | Reference | Type aliases, opaque types, typeconstructors, and the signature system for declaring types without bodies. | https://we-data-ch.github.io/typr.github.io/docs/reference/signatures |
| Escape Hatches | Reference | Dropping out of TypR's type system to write raw R or JavaScript. | https://we-data-ch.github.io/typr.github.io/docs/reference/escape-hatches |
| TypR vs R — What Really Changes | Reference | A side-by-side comparison of R and TypR. | https://we-data-ch.github.io/typr.github.io/docs/reference/cheatsheet |
| Compatibility with R | Reference | Why TypR is a companion to R rather than a replacement, and how a TypR package stays an ordinary R package. | https://we-data-ch.github.io/typr.github.io/docs/reference/r-typr |
| Philosophy | Philosophy | Freedom versus safety, and where TypR's gradual typing sits between R and a strict language. | https://we-data-ch.github.io/typr.github.io/docs/philosophy/intro |
| Why this type system | Philosophy | Why the type system is shaped the way it is: keeping R's flexibility while taming its fragility. | https://we-data-ch.github.io/typr.github.io/docs/philosophy/type-system-design |
| Vectorization by design | Philosophy | Why R's vectorization stops at custom types, and how TypR's lifting-based vectorization goes further. | https://we-data-ch.github.io/typr.github.io/docs/philosophy/vectorization_by_design |
| The beauty of syntax | Philosophy | The syntax choices that set TypR apart from R, and the reasoning behind each of them. | https://we-data-ch.github.io/typr.github.io/docs/philosophy/beautiful_syntax |
| Design Proposals | Philosophy | How language changes are decided in TypR: the RFC process, where proposals live, and what makes one convincing. | https://we-data-ch.github.io/typr.github.io/docs/philosophy/design-proposals |
| Generics & Kind Sigils | Deep Dive | Generic type parameters and the kind sigils that power TypR's type system. | https://we-data-ch.github.io/typr.github.io/docs/concepts/generics-kind |
| Type Constructors & Aliases | Deep Dive | Defining new types with type, opaque, and typeconstructor. | https://we-data-ch.github.io/typr.github.io/docs/concepts/type-constructors |
| Known Pitfalls | Deep Dive | Common pitfalls and ambiguities in the TypR parser. | https://we-data-ch.github.io/typr.github.io/docs/concepts/known-pitfalls |
