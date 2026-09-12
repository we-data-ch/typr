# untyped-r-function-arity-error

Source: `rfcs/0028-calling-untyped-r-functions.md`, accepted 2026-09-12
(we-data-ch/typr#28, tracking we-data-ch/typr#29), Reference-level explanation, "Error messages".

## Ce qui devrait se passer

Before the RFC, any call to an untyped R function was rejected by the *arity* check that
`Type::UnknownFunction` (0-ary) implicitly enforced, and the message printed the internal
placeholder back at the user:

```
Type error: No signature of function 'my_addition' matches this call.
  help: 'my_addition' exists but none of its signature(s) accepts these arguments:
          () -> UnknownFunction
```

Now that the function is callable, the only thing left to check is arity, and the RFC specifies
a dedicated message for it instead of reusing the generic no-matching-signature wording:

```
Type error: 'my_addition' is an untyped R function taking 2 argument(s), called with 3.
  help: its body is not type-checked; only the number of arguments is.
```

## Vérification

Implemented alongside case `untyped-r-function-callable`: `TypeError::UntypedFunctionArity`
(`components/error_message/type_error.rs`, code `T044`), raised in
`processes/type_checking/function_application.rs` when a call's argument count doesn't match a
signature that is exactly one, non-variadic, all-`Any` parameters returning `Any` — the shape
`Lang::RFunction`'s typing rule now produces (`FunctionType::is_r_function`).

## Statut

Kept as a regression net: the internal `UnknownFunction` placeholder must never resurface in a
user-facing error message again.
