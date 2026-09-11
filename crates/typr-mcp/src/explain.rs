//! Long-form explanations for TypR's diagnostic codes.
//!
//! `check`/`build` already return a stable `code` (`T0xx`/`S0xx`) plus a
//! one-line `simple_message()` — enough to locate the problem, not always
//! enough to fix it without guessing. This table adds, for every code that
//! can actually be produced by the compiler (see `TypRError::code`/
//! `SyntaxError::code` in typr-core for the enums this is derived from), a
//! short explanation of *why* the compiler rejects the code plus a minimal
//! before/after example.
//!
//! Every entry's `bad` example is asserted (in `lib.rs`'s test suite,
//! `explain_entries_are_verified_against_the_compiler`) to reproduce exactly
//! that code, and `good` to type-check cleanly, so every entry here is
//! verified against the real compiler rather than hand-typed prose that can
//! drift. Extend this table code by code, each time re-verified the same
//! way — don't paraphrase a `TypeError`/`SyntaxError` variant's doc comment
//! into an entry without running it through `check_source`.
//!
//! As of 2026-09-10 this covers 54 of the 60 declared codes (17 `S0xx` + 43
//! `T0xx`) — every one with at least one live call site in `typr-core`. The
//! 6 missing codes are dead code, not gaps in this table: each variant is
//! declared in `SyntaxError`/`TypeError` but has **zero** remaining
//! construction sites anywhere in `typr-core` (verified by
//! `grep -rn "TypeError::NAME(" crates/typr-core/src/` /
//! `SyntaxError::NAME(`, excluding the enum's own definition file), so no
//! input can make `check`/`build` ever return them:
//! - `S001` (`FunctionWithoutType`) — its one call site's guard condition
//!   (`*id.fragment() == "fn"` after matching only `"function"`/`"\\"`) can
//!   never be true; unreachable by construction, not just untested.
//! - `S004` (`MissingListPrefix`) — no construction site at all.
//! - `T003` (`UndefinedFunction`), `T007` (`PrivateVariable`), `T008`
//!   (`GenericPatternMatch`), `T030` (`CircularModuleDependency`) — likewise
//!   no construction site left in the type checker.
//!
//! `explain` reports `found: false` for these six rather than guessing at an
//! example that wouldn't actually reproduce. If one of them grows a real
//! call site again, add its entry the same verified way as every other row.

pub struct ExplainEntry {
    pub code: &'static str,
    pub title: &'static str,
    pub explanation: &'static str,
    /// TypR source that reproduces this diagnostic.
    pub bad: &'static str,
    /// The same source, fixed — type-checks cleanly.
    pub good: &'static str,
}

pub const ENTRIES: &[ExplainEntry] = &[
    ExplainEntry {
        code: "S002",
        title: "Missing return type after ':'",
        explanation: "A function signature has a ':' but nothing after it. TypR requires an \
            explicit return type on every named function — write it right after the ':', \
            before the body's opening '{'.",
        bad: "let f <- fn(a: int): { a };",
        good: "let f <- fn(a: int): int { a };",
    },
    ExplainEntry {
        code: "S003",
        title: "Missing semicolon",
        explanation: "Every statement in TypR ends with ';', unlike R. This is what lets the \
            transpiler reason about statement boundaries without guessing from newlines, and \
            it applies even to the last statement in a file or block.",
        bad: "let a: int <- 5",
        good: "let a: int <- 5;",
    },
    ExplainEntry {
        code: "S005",
        title: "Empty function body",
        explanation: "A function body '{ }' with nothing inside is rejected outright rather than \
            silently accepted — an empty block is almost always a placeholder left behind while \
            writing the signature first. Use '...' as an explicit stand-in for a body that isn't \
            written yet.",
        bad: "let f <- fn(a: int): int { };",
        good: "let f <- fn(a: int): int { ... };",
    },
    ExplainEntry {
        code: "S006",
        title: "'fn(...)' used in type position",
        explanation: "'fn(...)' is expression syntax for defining a function value — it never \
            appears in a type annotation. A function *type* is written with bare parentheses and \
            an arrow: '(ArgType, ...) -> ReturnType'.",
        bad: "let f: fn(int) -> int <- fn(a: int): int { a };",
        good: "let f: (int) -> int <- fn(a: int): int { a };",
    },
    ExplainEntry {
        code: "S007",
        title: "Record constructor type with more than one index",
        explanation: "A generic record type built with 'typeconstructor NAME[N] record;' takes \
            exactly one integer-generic index before the field block — 'Name[N]{ ... }'. Adding \
            a second, comma-separated index or type there isn't supported.",
        bad: "typeconstructor Tibble[N] record;\nlet f <- fn(t: Tibble[3, int]{ id: int }): int { 1 };",
        good: "typeconstructor Tibble[N] record;\nlet f <- fn(t: Tibble[3]{ id: int }): int { 1 };",
    },
    ExplainEntry {
        code: "S008",
        title: "Record block inside a recursive array/vector type",
        explanation: "'Array[N, ElementType]' (and 'Vec[N, ElementType]') take a plain type as \
            their element, not an inline record block — writing 'Array[N, { field: Type }]' isn't \
            supported. Declare a named record type first and reference it by name instead.",
        bad: "let f <- fn(x: Array[5, { a: int }]): int { 1 };",
        good: "let f <- fn(x: Array[5, int]): int { 1 };",
    },
    ExplainEntry {
        code: "S009",
        title: "Unrecognized token",
        explanation: "The parser hit a sequence of characters that doesn't start any known TypR \
            construct at that position — not an identifier, keyword, or operator it recognizes. \
            Usually a stray symbol left over from editing, or punctuation copied from another \
            language.",
        bad: "let a <- 5; ~~~;",
        good: "let a <- 5;",
    },
    ExplainEntry {
        code: "S010",
        title: "'let' used where 'type' was meant",
        explanation: "The right-hand side is a type expression (e.g. 'list { ... }'), but the \
            binding starts with 'let', which declares a value. Declaring a reusable type needs \
            'type NAME <- ...;' instead.",
        bad: "let Person <- list { name: char };",
        good: "type Person <- list { name: char };",
    },
    ExplainEntry {
        code: "S011",
        title: "'type' used where 'let' was meant",
        explanation: "The right-hand side is an ordinary value (a literal, a call, ...), but the \
            binding starts with 'type', which declares a type alias. Binding a value needs \
            'let NAME <- ...;' instead.",
        bad: "type message <- \"hi\";",
        good: "let message <- \"hi\";",
    },
    ExplainEntry {
        code: "S012",
        title: "Single uppercase letter used as a type alias name",
        explanation: "A single uppercase letter (T, U, A, ...) is reserved for generic type \
            variables in TypR — writing 'type T <- ...;' collides with that convention. Give the \
            alias a longer, descriptive PascalCase name instead.",
        bad: "type T <- int;",
        good: "type Ty <- int;",
    },
    ExplainEntry {
        code: "S013",
        title: "'list{...}'/'record{...}'/'object{...}' with positional elements",
        explanation: "The 'list'/'record'/'object' keywords in front of '{ ... }' are reserved \
            for named-field record literals ('list{ x = 1, y = 2 }'). Giving them positional, \
            unnamed elements instead ('list{1, 2, 3}') is rejected — use the neutral ':{...}' \
            tuple syntax for a positional tuple.",
        bad: "let a <- list{1, 2, 3};",
        good: "let a <- :{1, 2, 3};",
    },
    ExplainEntry {
        code: "S014",
        title: "'!;' mutation on a non-assignable target",
        explanation: "The implicit-mutation sugar 'expr!;' (short for 'x <- expr;') requires the \
            head of the '.'/'|>' chain to be a plain variable, so the compiler knows what to \
            reassign. Using it on a literal or any expression that doesn't start from a variable \
            has nothing to assign back to.",
        bad: "3!;",
        good: "let x <- 3;\nx!;",
    },
    ExplainEntry {
        code: "S015",
        title: "'//' used for a comment",
        explanation: "TypR comments start with '#', the R convention — '//' (C/JS-style) has no \
            special meaning to the parser and is rejected rather than silently misparsed.",
        bad: "// comment\nlet a <- 5;",
        good: "# comment\nlet a <- 5;",
    },
    ExplainEntry {
        code: "S016",
        title: "'=' used as a comparison",
        explanation: "'=' is not TypR's equality operator — it has no comparison meaning at all \
            in expression position. Use '==' to compare two values; '=' is reserved for named \
            arguments in calls and for default parameter values.",
        bad: "let ok: bool <- 5 = 5;",
        good: "let ok: bool <- 5 == 5;",
    },
    ExplainEntry {
        code: "S017",
        title: "Tuple destructuring arity mismatch",
        explanation: "'let :{a, b, ...} <- source;' destructures a tuple positionally, so the \
            number of bindings on the left must exactly match the number of elements in the \
            source tuple — TypR neither drops extra elements nor leaves bindings unset.",
        bad: "let :{a, b} <- :{1, 2, 3};",
        good: "let :{a, b, c} <- :{1, 2, 3};",
    },
    ExplainEntry {
        code: "T001",
        title: "Type mismatch on a 'let' binding",
        explanation: "The value on the right-hand side doesn't match the type annotation on the \
            left. Either change the annotation to match the value's real type, or fix the \
            value — there's no implicit coercion between TypR's primitive types.",
        bad: "let x: int <- \"oops\";",
        good: "let x: int <- 42;",
    },
    ExplainEntry {
        code: "T002",
        title: "Field value doesn't match its declared type",
        explanation: "A record constructor ('TypeName:{ field: value, ... }') was given a value \
            for one of its fields whose type doesn't match what the type declares for that \
            field. Structural types are still checked field by field at construction time.",
        bad: "type Person <- list { name: char }; let p <- Person:{ name: 5 };",
        good: "type Person <- list { name: char }; let p <- Person:{ name: \"Alice\" };",
    },
    ExplainEntry {
        code: "T004",
        title: "Undefined variable",
        explanation: "A name is used that was never bound with 'let' (or a function parameter, \
            or an import) anywhere visible from this point. This is usually a typo, a binding \
            that comes later in the file, or a missing 'use module::name;' for something \
            defined in another module.",
        bad: "let y <- does_not_exist;",
        good: "let x <- 42;\nlet y <- x;",
    },
    ExplainEntry {
        code: "T005",
        title: "Function body doesn't match its declared return type",
        explanation: "The value the function body actually evaluates to doesn't match the \
            return type written after ':'. Unlike a 'let' mismatch (T001), this is about the \
            last expression of the function body versus the signature, not a variable's \
            annotation.",
        bad: "let f <- fn(a: int): int { \"oops\" };",
        good: "let f <- fn(a: int): int { 42 };",
    },
    ExplainEntry {
        code: "T006",
        title: "Name already bound, can't be imported again",
        explanation: "A 'use module::{...}' directive names the same local identifier twice \
            (directly, or because it collides with something already declared/imported in this \
            scope). Each name can only be bound once per scope — drop the duplicate, or alias \
            one of them with 'as'.",
        bad: "module geo { @pub let circle_ratio <- 3.14; };\nuse geo::{circle_ratio, circle_ratio};",
        good: "module geo { @pub let circle_ratio <- 3.14; };\nuse geo::circle_ratio;",
    },
    ExplainEntry {
        code: "T009",
        title: "Field not found on a record type",
        explanation: "A field is accessed with 'value$field' (or 'value.field') but the field \
            isn't part of the value's structural type. Either the field name is misspelled, or \
            the type declaration needs that field added.",
        bad: "type Person <- list { name: char }; let p <- Person:{ name: \"a\" }; p$age;",
        good: "type Person <- list { name: char, age: int };\nlet p <- Person:{ name: \"a\", age: 1 };\np$age;",
    },
    ExplainEntry {
        code: "T010",
        title: "Type error in an expression",
        explanation: "A generic fallback raised across many expression forms (conditions, \
            operators, casts, ...) when the expression's shape doesn't type-check for a reason \
            not specific enough to get its own code — e.g. an 'if' condition that isn't 'bool'. \
            Check the diagnostic's source position; the short 'message' from `check`/`build` \
            usually pins down which sub-expression is at fault.",
        bad: "if (5) { 1 } else { 0 };",
        good: "if (5 > 0) { 1 } else { 0 };",
    },
    ExplainEntry {
        code: "T011",
        title: "Indexing out of range or with the wrong type",
        explanation: "Either a tuple was indexed with a literal position outside its length \
            ('t[N]' where N isn't between 1 and the tuple's size), or an array/vector was \
            indexed with something that isn't an 'int'. TypR checks tuple indices statically \
            since a tuple's length is part of its type.",
        bad: "let t <- :{1, 2};\nt[5];",
        good: "let t <- :{1, 2};\nt[1];",
    },
    ExplainEntry {
        code: "T012",
        title: "Type alias not found",
        explanation: "A type name is used that isn't declared anywhere visible — not a builtin, \
            not a local 'type'/'opaque', and not resolvable through a module path either. Most \
            often a typo, or a module member that's private (see T013 for the 'exists but not \
            imported' case).",
        bad: "module geo { type Meters <- int; };\nlet x: geo::Meters <- true;",
        good: "module geo { @pub type Meters <- int; };\nlet x: geo::Meters <- 1;",
    },
    ExplainEntry {
        code: "T013",
        title: "Type alias exists in another module but isn't imported",
        explanation: "The bare alias name (without a module:: prefix) resolves to a real type \
            declared in a known module, but it was never brought into this scope with 'use'. \
            Distinguished from T012 (a genuinely unknown name) so the fix can point straight at \
            the missing 'use' line.",
        bad: "module Geo { @pub type Meters <- int; };\nlet x: Meters <- 1;",
        good: "module Geo { @pub type Meters <- int; };\nuse Geo::Meters;\nlet x: Meters <- 1;",
    },
    ExplainEntry {
        code: "T014",
        title: "Variable or function exists in another module but isn't imported",
        explanation: "The ordinary-name counterpart of T013: a value or function resolves to a \
            real member of a known module, but nothing brought it into this scope. If the member \
            is private in its module, the message says so — add '@pub'/'@export' there first, \
            then 'use module::name;' here.",
        bad: "module geo { @pub let circle_ratio <- 3.14; };\nlet y <- circle_ratio;",
        good: "module geo { @pub let circle_ratio <- 3.14; };\nuse geo::circle_ratio;\nlet y <- circle_ratio;",
    },
    ExplainEntry {
        code: "T015",
        title: "Function not defined for this type",
        explanation: "A call site refers to a function name that has no definition applicable \
            to the argument's type — most often because the function was never declared at \
            all, but it can also mean it exists only for a different type (TypR dispatches on \
            argument types the way R's S3 does).",
        bad: "does_not_exist(1);",
        good: "let does_not_exist <- fn(x: int): int { x };\ndoes_not_exist(1);",
    },
    ExplainEntry {
        code: "T016",
        title: "Generic used in an alias body without declaring it",
        explanation: "A type alias's right-hand side mentions a generic type variable (an \
            uppercase name like 'T') that never appears in the alias's own header. TypR needs \
            every generic the body uses to be declared up front: 'type Name<T> <- ...;' — or, if \
            it wasn't meant to be generic at all, replace the stray uppercase name with a \
            concrete type or 'Any'.",
        bad: "type Box <- list { value: T };",
        good: "type Box<T> <- list { value: T };",
    },
    ExplainEntry {
        code: "T017",
        title: "Interface type used only in return position",
        explanation: "An interface type appears as a function's return type but never as any \
            parameter's type — with nothing concrete anchoring 'Self', accepting this would \
            require an existential type TypR doesn't support. Either take a value of the \
            interface type as a parameter too (so a concrete type gets inferred from the call \
            site), or, if the function genuinely manufactures a value with a hidden concrete \
            type, declare an 'opaque' type for it instead of an interface.",
        bad: "type Incrementable <- interface { incr: (Self) -> Self };\nlet f <- fn(): Incrementable { ... };",
        good: "type Incrementable <- interface { incr: (Self) -> Self };\nlet f <- fn(i: Incrementable): Incrementable { i.incr() };",
    },
    ExplainEntry {
        code: "T018",
        title: "Undeclared generic record constructor",
        explanation: "'Name[N]{ ... }' builds a value of a generic record type, but 'Name' was \
            never registered as one via 'typeconstructor Name[N] record;'. Declare the \
            constructor before using its bracketed-index record syntax.",
        bad: "type MyTable <- Tibble[3]{ id: int };",
        good: "typeconstructor Tibble[N] record;\ntype MyTable <- Tibble[3]{ id: int };",
    },
    ExplainEntry {
        code: "T019",
        title: "Field provided more than once in a record constructor",
        explanation: "A 'TypeName:{ field = value, ... }' call sets the same field name twice. \
            Each field may be given at most once per constructor call (either as an explicit \
            'field = value' or, exclusively, via a spread).",
        bad: "type Person <- list { name: char, age: int };\nlet bob <- Person:{ name = \"Bob\", name = \"Bobby\", age = 12 };",
        good: "type Person <- list { name: char, age: int };\nlet bob <- Person:{ name = \"Bobby\", age = 12 };",
    },
    ExplainEntry {
        code: "T020",
        title: "Missing field in a record constructor",
        explanation: "A record type declares a field that the constructor call doesn't provide \
            — neither as an explicit 'field: value' nor via a '..source' spread. Every \
            non-optional field must be covered.",
        bad: "type Person <- list { name: char, age: int }; let p <- Person:{ name: \"a\" };",
        good: "type Person <- list { name: char, age: int };\nlet p <- Person:{ name: \"a\", age: 1 };",
    },
    ExplainEntry {
        code: "T021",
        title: "Spread source has the wrong alias",
        explanation: "'TypeName:{ ..source }' (the nominal spread) requires 'source' to be of \
            exactly the same declared alias as the type being constructed — even if the two \
            aliases happen to share the same structural fields, they're different types. Use a \
            value of the matching alias, or switch to the structural spread ('...source') if \
            mixing compatible shapes is really intended.",
        bad: "type Person <- list { name: char, age: int };\ntype Robot <- list { name: char, age: int };\nlet bob <- Robot:{ name = \"Bob\", age = 12 };\nlet alice <- Person:{ name = \"Alice\", ..bob };",
        good: "type Person <- list { name: char, age: int };\nlet bob <- Person:{ name = \"Bob\", age = 12 };\nlet alice <- Person:{ name = \"Alice\", ..bob };",
    },
    ExplainEntry {
        code: "T022",
        title: "Type-level operator applied outside its domain",
        explanation: "A type-level operator ('+ - * /' for arithmetic on index/dimension types, \
            or '&' for intersecting two types) was applied to operand(s) it isn't defined for — \
            e.g. '&' requires both sides to be structurally record-like (plain records or \
            interfaces), not primitives like 'int'/'char'.",
        bad: "type Combo <- int & char;",
        good: "type Movable <- interface { mv: (Self) -> Self };\ntype Drawable <- interface { draw: (Self) -> char };\ntype Combo <- Movable & Drawable;",
    },
    ExplainEntry {
        code: "T023",
        title: "'embed' on a non-record field",
        explanation: "'embed name: Type' (named type embedding) requires 'Type' to be a record — \
            it forwards that record's methods onto the enclosing type. Embedding a primitive or \
            any non-record type has nothing to forward.",
        bad: "type Bad <- list { embed n: int };",
        good: "type Ok <- list { embed n: list { v: int } };",
    },
    ExplainEntry {
        code: "T024",
        title: "Two embedded fields forward the same method name",
        explanation: "Named type embedding auto-forwards every function whose first parameter \
            matches the embedded record's type. When two embedded fields both have a function of \
            the same name defined for them, the forwarding is ambiguous — TypR won't guess which \
            one you meant. Rename one of the underlying functions, or embed only one of the two \
            fields.",
        bad: "type Position <- list { x: int, y: int };\nlet move <- fn(self: Position, dx: int, dy: int): Position { Position:{ x = self$x + dx, y = self$y + dy } };\ntype Speed <- list { x: int, y: int };\nlet move <- fn(self: Speed, dx: int, dy: int): Speed { Speed:{ x = self$x + dx, y = self$y + dy } };\ntype Player <- list { embed coords: Position, embed vel: Speed };",
        good: "type Position <- list { x: int, y: int };\nlet move <- fn(self: Position, dx: int, dy: int): Position { Position:{ x = self$x + dx, y = self$y + dy } };\ntype Speed <- list { x: int, y: int };\ntype Player <- list { embed coords: Position, embed vel: Speed };",
    },
    ExplainEntry {
        code: "T025",
        title: "Explicit function collides with an embedded one",
        explanation: "A type defines its own function under a name that's already provided by \
            one of its embedded fields' forwarding. The explicit definition and the auto-forwarded \
            one can't coexist under the same name — rename the explicit function, or drop the \
            embedding for that method.",
        bad: "type Position <- list { x: int, y: int };\nlet move <- fn(self: Position, dx: int, dy: int): Position { Position:{ x = self$x + dx, y = self$y + dy } };\ntype Player <- list { embed coords: Position, name: char };\nlet move <- fn(self: Player, dx: int, dy: int): Player { Player:{ coords = move(self$coords, dx, dy), ...self } };",
        good: "type Position <- list { x: int, y: int };\nlet move <- fn(self: Position, dx: int, dy: int): Position { Position:{ x = self$x + dx, y = self$y + dy } };\ntype Player <- list { embed coords: Position, name: char };",
    },
    ExplainEntry {
        code: "T026",
        title: "Generic used with two different kind sigils",
        explanation: "The same generic name (e.g. 'T') is used more than once in a function \
            signature with conflicting kind sigils (bare vs '#T' vs '^T' vs '%T' vs '@T' vs \
            '?T') — the first occurrence fixes the generic's kind for the rest of that \
            signature. Give the two uses the same sigil, or use distinct generic names if they \
            really are unrelated.",
        bad: "let f <- fn(x: A): [#A, int] { [1] };",
        good: "let f <- fn(x: #A): [#A, int] { [1] };",
    },
    ExplainEntry {
        code: "T027",
        title: "'Self:{...}' used with no anchor",
        explanation: "'Self:{ field = value, ...base }' builds a value of the enclosing \
            function's own parameter type, so it only makes sense inside a function body — and \
            only with a '...base' spread anchoring which parameter 'Self' refers to. Used at the \
            top level, or without the spread, there's nothing for 'Self' to resolve to.",
        bad: "let x <- Self:{ x = 1 };",
        good: "type HasTruc <- list { truc: int };\nlet incrTruc <- fn(a: HasTruc): HasTruc { Self:{ truc = a.truc + 1, ...a } };",
    },
    ExplainEntry {
        code: "T028",
        title: "Non-default parameter follows a default one",
        explanation: "Once a function parameter has a default value, every later parameter must \
            also have one — TypR has no way to skip a middle argument at the call site. Reorder \
            the parameters so defaulted ones are trailing, or give the later parameter a default \
            too.",
        bad: "let f <- fn(a: int = 1, b: int): int { a + b };",
        good: "let f <- fn(a: int, b: int = 1): int { a + b };",
    },
    ExplainEntry {
        code: "T029",
        title: "Imported member is private",
        explanation: "A 'use module::name;' directive names a real member of that module, but \
            it was declared without '@pub'/'@export' — so it isn't visible outside the module. \
            Add '@pub' (or '@export' for one that should also get a roxygen2 export) before its \
            declaration.",
        bad: "module geo { let secret <- 1; };\nuse geo::secret;",
        good: "module geo { @pub let secret <- 1; };\nuse geo::secret;",
    },
    ExplainEntry {
        code: "T031",
        title: "Type doesn't satisfy an interface",
        explanation: "'Interface(value)' (a compile-time structural check, never a real call) \
            failed because 'value''s type has no function matching one or more of the interface's \
            required methods by name. The diagnostic lists which method(s) are missing — define a \
            free function with that name whose first parameter is the concrete type.",
        bad: "type Point <- list { x: int, y: int };\ntype Movable <- interface { mv: (Self, int, int) -> Self };\nlet p <- Point:{ x = 1, y = 2 };\nMovable(p);",
        good: "type Point <- list { x: int, y: int };\nlet mv <- fn(p: Point, dx: int, dy: int): Point { p };\ntype Movable <- interface { mv: (Self, int, int) -> Self };\nlet p <- Point:{ x = 1, y = 2 };\nMovable(p);",
    },
    ExplainEntry {
        code: "T032",
        title: "Interface method exists but has the wrong signature",
        explanation: "A concrete type has a function matching a required interface method by \
            name, but its parameter/return types aren't compatible with what the interface \
            declares (contravariant parameters, covariant return). Fix the function's signature \
            to match what the interface requires for 'Self'.",
        bad: "type Point <- list { x: int, y: int };\nlet mv <- fn(p: Point, dx: int): Point { p };\ntype Movable <- interface { mv: (Self, int, int) -> Self };\nlet p <- Point:{ x = 1, y = 2 };\nMovable(p);",
        good: "type Point <- list { x: int, y: int };\nlet mv <- fn(p: Point, dx: int, dy: int): Point { p };\ntype Movable <- interface { mv: (Self, int, int) -> Self };\nlet p <- Point:{ x = 1, y = 2 };\nMovable(p);",
    },
    ExplainEntry {
        code: "T033",
        title: "Non-exhaustive 'match'",
        explanation: "A 'match' over a tag-union scrutinee doesn't cover every variant the union \
            declares, and has no catch-all branch ('_ => ...' or 'name => ...') to cover the \
            rest either. List every variant explicitly, or add a catch-all as the last branch.",
        bad: "type Color <- .Red | .Green | .Blue;\nlet f <- fn(c: Color): int { match c { .Red => 1, .Green => 2 } };",
        good: "type Color <- .Red | .Green | .Blue;\nlet f <- fn(c: Color): int { match c { .Red => 1, .Green => 2, .Blue => 3 } };",
    },
    ExplainEntry {
        code: "T034",
        title: "Match pattern shape can't match the scrutinee's type",
        explanation: "A 'match' branch uses a pattern shape (tuple ':{...}', record, or a tag) \
            that's structurally impossible for the scrutinee's actual type — e.g. a tuple \
            pattern against an 'int'. The pattern's shape must agree with what's being matched.",
        bad: "let f <- fn(c: int): int { match c { :{a, b} => a } };",
        good: "let f <- fn(c: tuple{int, int}): int { match c { :{a, b} => a } };",
    },
    ExplainEntry {
        code: "T035",
        title: "Unsupported nested match pattern",
        explanation: "A pattern nested inside another pattern (e.g. a tuple pattern inside a \
            tuple pattern, or inside a tag's payload) isn't supported — only a plain variable \
            name, '_', or an empty payload can appear in that nested position. Flatten the match \
            into an extra level, or bind the whole sub-value to a name and destructure it in a \
            second step.",
        bad: "let f <- fn(t: tuple{tuple{int, int}, int}): int { match t { :{:{a, b}, c} => c } };",
        good: "let f <- fn(t: tuple{tuple{int, int}, int}): int { match t { :{p, c} => c } };",
    },
    ExplainEntry {
        code: "T036",
        title: "Wrong number of type arguments for a generic alias",
        explanation: "A generic alias ('type Name<T, ...> <- ...;') is referenced with a \
            different number of concrete type arguments than it declares — either bare with none \
            at all, or with too many/too few between the angle brackets. Match the argument count \
            to the alias's own declared arity.",
        bad: "type Box<T> <- list { value: T };\nlet x: Box<int, char> <- Box:{ value = 1 };",
        good: "type Box<T> <- list { value: T };\nlet x: Box<int> <- Box:{ value = 1 };",
    },
    ExplainEntry {
        code: "T037",
        title: "'break'/'next' outside a loop",
        explanation: "'break;' and 'next;' are only meaningful inside an enclosing 'loop'/'for'/\
            'while' body — used anywhere else, there's no loop to break out of or continue. R \
            would only fail at runtime for the same mistake; TypR catches it at compile time.",
        bad: "break;",
        good: "loop { break; };",
    },
    ExplainEntry {
        code: "T038",
        title: "DataFrame column isn't vector-shaped",
        explanation: "'data.frame(col = value, ...)' requires every column to be a vector \
            ('[T]'/'Vec[T]'), even a one-element one — a bare scalar isn't a valid column. Wrap \
            the value in an array literal.",
        bad: "let d <- data.frame(x = 1);",
        good: "let d <- data.frame(x = [1]);",
    },
    ExplainEntry {
        code: "T039",
        title: "DataFrame columns have different lengths",
        explanation: "Two columns in the same 'data.frame(...)' literal have statically known \
            lengths that don't match — every column of a dataframe must have the same number of \
            rows. Pad or trim the shorter array literal to match.",
        bad: "let d <- data.frame(x = [1, 2, 3], y = [4, 5]);",
        good: "let d <- data.frame(x = [1, 2, 3], y = [4, 5, 6]);",
    },
    ExplainEntry {
        code: "T040",
        title: "Tag constructed with named-field syntax",
        explanation: "A real union tag ('.Variant(payload)') is being constructed with \
            'Union.Variant:{ field = value }' record-style syntax. The generated R constructor \
            for a tag always takes exactly one positional payload — construct it with \
            '.Variant(value)' instead (use '.Variant(:{ ... })' when the payload itself is \
            record-shaped). Qualified 'Union.Name:{...}' syntax only works for a union member \
            that's a plain record alias, not a tag.",
        bad: "type Shape <- .Circle(num) | .Square(num);\nShape.Circle:{ x = 1 };",
        good: "type Shape <- .Circle(num) | .Square(num);\nlet s <- .Circle(1.0);",
    },
    ExplainEntry {
        code: "T041",
        title: "Not a variant of this union",
        explanation: "'Union.Name' (or 'Union.Name:{...}') names something that isn't one of \
            'Union''s declared tag-union members at all — usually a typo in the variant name.",
        bad: "type Color <- .Red | .Blue;\nColor.Green:{ };",
        good: "type Color <- .Red | .Blue;\nColor.Blue;",
    },
    ExplainEntry {
        code: "T042",
        title: "No matching call signature",
        explanation: "A function is called with argument types that don't match any of its \
            known signatures — same underlying mismatch as T002/T001, but surfacing at the \
            call site instead of the declaration, typically because the function value carries \
            an explicit function-type annotation.",
        bad: "let f: (int) -> int <- fn(a: int): int { a }; f(\"oops\");",
        good: "let f: (int) -> int <- fn(a: int): int { a };\nf(1);",
    },
    ExplainEntry {
        code: "T043",
        title: "Forced dispatch (`name<Type>`) has no implementation for that type",
        explanation: "'name<Type>(...)' (turbofish-style forced S3 dispatch) names a type with \
            no registered overload of 'name' — the compile-time counterpart of a runtime \"could \
            not find function `name.Type`\" error in the generated R. Either implement 'name' for \
            that type, or force dispatch to one of the types listed as available.",
        bad: "type Personne <- list{ name: char };\ntype Voiture <- list{ marque: char };\nlet greet <- fn(x: Personne, y: Personne): Personne { x };\nlet p1 <- Personne:{ name = \"a\" };\nlet p2 <- Personne:{ name = \"b\" };\ngreet<Voiture>(p1, p2);",
        good: "type Personne <- list{ name: char };\ntype Voiture <- list{ marque: char };\nlet greet <- fn(x: Personne, y: Personne): Personne { x };\nlet p1 <- Personne:{ name = \"a\" };\nlet p2 <- Personne:{ name = \"b\" };\ngreet<Personne>(p1, p2);",
    },
];

pub fn find(code: &str) -> Option<&'static ExplainEntry> {
    ENTRIES.iter().find(|e| e.code.eq_ignore_ascii_case(code))
}
