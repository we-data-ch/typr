# `@A` after a tuple or vector type was silently dropped

Seul un nom d'interface, un `list{...}` ou un `df[...]{...}` porte un suffixe `@Id`
(`rfcs/0000-record-bound-generics.md`, étape 1). Après `tuple{...}` ou `[N, T]`, le suffixe était
avalé sans un mot par le résolveur de priorités. C'est l'erreur de syntaxe fatale S023
(`StructuralBoundSuffix`, `processes/parsing/types.rs` `structural_bound_suffix`).

Le repro d'origine (`list{a: int}@A`) est maintenant une syntaxe valide : voir les tests
`test_record_bound_suffix_parses` et `test_detached_record_bound_suffix_is_an_error`.
