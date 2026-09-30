# a (Self, Self) method on two distinct rigid ids is rejected

Avec `fn(a: Same@A, b: Same@B): bool { a.same(b) }`, `b : B` ne satisfait pas le paramètre `A` : erreur attendue dans le corps. Aujourd'hui accepté (`@A`/`@B` avalés, et rigides distincts jugés compatibles).

Référence : `interface_generic_unification_plan.md` §0 (défaut 3), phases 3-4.
