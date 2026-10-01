# two parameters sharing the identifier X must receive the same type

Avec `fn(a: Lovable@X, b: Lovable@X)`, `same_kind(cat, dog)` doit être rejeté (X lié à Cat puis Dog). Aujourd'hui `@X` est avalé silencieusement et l'appel passe.

Référence : `interface_generic_unification_plan.md` §0 (défaut 2), phases 3-4.
