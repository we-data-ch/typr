# new-scene-object-arg-not-found

## Ce qui DEVRAIT se passer

Dans le projet `framr`, `typr check` devrait passer sans erreur sur `TypR/main.ty`:

```typr
let c1 <- new_circle("c1", 2);
print(c1);
let scene <- new_scene([c1]);
```

`new_scene` attend `objects: [Object]` où `Object <- Id & Animable`; `Circle` satisfait
structurellement `Object` (champ `id: char` + fonction libre `animate(self: Circle, animation:
Animation): Circle`). `[c1]` devrait donc type-check comme `[Object]`.

## Anomalies observées

Deux bugs indépendants, tous deux dans `typr-core`, pas dans le projet `framr` :

**1. `use circle::animate;` manquant dans `main.ty` (piège déjà documenté, pas un bug ici)** —
la satisfaction d'interface structurelle de TypR n'examine que les fonctions visibles dans le
contexte local du fichier appelant (case 0009/0010). `main.ty` n'appelle jamais `animate`
directement mais doit quand même l'importer pour que `new_scene([c1])` type-check. Le `repro/`
de ce cas inclut cet import.

**2. Bug parseur réel : un `let` sans `;` final suivi d'un autre `let` annoté `@pub` fait
disparaître silencieusement le second, et corrompt le premier.**

`TypR/scene.ty` a plusieurs `let` de suite dont un (`add_object`) sans `;` terminal avant le
`@pub` du suivant :

```typr
@pub
let add_object <- fn(self: Scene, object: Object): Scene {
	Scene:{ objects: self.objects.extend(object), ...self }
}          // <- pas de `;` ici

@pub
let print <- fn(self: Scene): Empty { ... };
```

`processes/parsing/elements.rs::elements()` tokenisait l'expression via
`many1(alt((as_excl_operator_token, single_element_token, element_operator_token)))` sans
imposer d'alternance stricte `Expression (Opérateur Expression)*`. `variable_exp` n'exclut
aucun mot-clé, donc après le `}` non terminé, le tokenizer continuait et absorbait le `@` de
`@pub` comme `Op::At` (opérateur binaire `@`, jamais géré nulle part côté type-checking/
transpiling — syntaxe totalement morte), puis `pub` comme variable. Le tableau de tokens
résultant `[Expr(fn_add_object), Op(@), Expr(Variable(pub))]` — bien formé selon
`VectorPriority::run_helper` (`operation_priority.rs`) — devenait donc le corps entier de
`add_object`, avalant le `@` et masquant silencieusement la déclaration suivante (`let print`)
qui n'apparaissait alors plus du tout dans l'AST, sans la moindre erreur de syntaxe.

Erreur observée : `Function `@`<Scene> not defined in this scope.` (au lieu du vrai nom de
fonction), localisée à `std.ty:1:1` (position par défaut faute de `HelpData` réel), plus
`Function new_scene<...> not defined in this scope.` tant que le piège #1 n'était pas corrigé.

## Fix

Deux changements dans `typr-core`, indépendants du fix pour le piège #1 (qui reste un
correctif côté projet `framr`) :

1. `processes/parsing/elements.rs::elements()` réécrit pour imposer l'alternance stricte
   `single_element_token (operator_like_token single_element_token)*` au lieu du
   `many1(alt(...))` sans contrainte. Un token non-opérateur après une expression complète
   arrête `many0` sans consommer d'entrée, laissant l'instruction suivante au parseur appelant
   au lieu de l'avaler.
2. `components/language/operators.rs::op()` : retrait de `"@"`/`"@@"` de la liste des
   opérateurs binaires génériques reconnus. `Op::At`/`Op::At2` n'ont aucun bras de
   type-checking ni de transpilation (désucrage vers un appel de fonction `` `@` `` que rien ne
   définit) — syntaxe morte comme opérateur d'expression. `@` reste pleinement fonctionnel comme
   préfixe d'annotation (`@pub`/`@export`/`@testable`/`@extern`/`@name: ...`), géré par des
   parseurs dédiés en amont dans `base_parse`.

Avec le seul fix #1, le second `let` réapparaît dans l'AST mais son `@pub` reste absorbé par le
`@` résiduel du `let` précédent (perte de la visibilité publique + pollution du corps du premier
`let`). Le fix #2 est nécessaire pour que `@pub` soit correctement attribué à sa propre
déclaration.
