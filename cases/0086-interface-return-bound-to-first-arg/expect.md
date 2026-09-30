# a return typed Lovable@B is bound to the first argument, not to B

Avec `fn(a: Lovable@A, b: Lovable@B): Lovable@B`, `let s: Cat <- second(cat, dog)` doit être rejeté (le retour est un `Dog`). Aujourd'hui `@A`/`@B` sont avalés et le retour est lié au premier argument : le programme passe.

Référence : `interface_generic_unification_plan.md` §0 (défaut 1), phases 3-4.
