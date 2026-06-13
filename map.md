# État de `map` dans la stdlib TypR

## Résumé

`map` est **typé mais pas implémenté** : la signature existe côté compile-time, mais il
n'y a aucun corps R correspondant au runtime. Tout appel à `map(...)` compile sans erreur
de type, puis **échoue à l'exécution** R avec :

```
Error in UseMethod("map", x) :
  pas de méthode pour 'map' applicable pour un objet de classe "..."
```

C'est une lacune préexistante de la bibliothèque standard, indépendante des fonctions
variadiques (le problème apparaît avec n'importe quel tableau, pas seulement `...xs`).

## Les deux moitiés de la stdlib (rappel)

Une fonction de la stdlib n'est utilisable que si **ses deux moitiés** existent :

| Moitié                | Fichier                                | `map` ?            |
|-----------------------|----------------------------------------|--------------------|
| Signature typée       | `crates/typr-cli/configs/std/default.ty` | ✅ présent (l. 41) |
| Corps R (runtime)     | `crates/typr-cli/configs/src/std.R`      | ❌ **absent**      |

La signature présente :

```typr
@map: (a: [#N, T], f: (T) -> U) -> [#N, U];
```

Le stub `map <- function(x, ...) UseMethod('map', x)` est généré automatiquement dans
`R/generic_functions.R` (parce que `map` est un générique), mais **aucune méthode**
(`map.typed_vec`, `map.numeric`, `map.default`, …) n'est définie nulle part. `UseMethod`
ne trouve donc rien à dispatcher.

Méthodes effectivement disponibles sur `typed_vec` (pour comparaison) :
`length`, `get`, `apply`, `reduce`, `sum`, `extend`, `print`. `map` n'en fait pas partie.

## Reproduction

```typr
let xs <- [1.0, 2.0, 3.0];
let ys <- map(xs, fn(x: num): num { x * 2.0 });   // compile OK
print(ys)                                          // échec runtime : no method for 'map'
```

## Correction proposée

Ajouter une implémentation `map.typed_vec` dans `crates/typr-cli/configs/src/std.R`
(la valeur du `typed_vec` est stockée dans le champ `$data`, une liste R), par exemple :

```r
map.typed_vec <- function(x, f) {
  typed_vec(lapply(x$data, f), dim = attr(x, "typed_dim"))
}
```

> ⚠️ `map` reçoit ses arguments dans l'ordre `(a, f)` côté TypR. Le stub généré est
> `function(x, ...)`, donc la méthode doit accepter `f` en second paramètre.

Procédure complète (cf. `CLAUDE.md`, section « Regenerating after editing a signature ») :

1. Éditer `crates/typr-cli/configs/src/std.R` pour ajouter `map.typed_vec` (la signature
   `@map` dans `default.ty` est déjà correcte, ne pas la toucher).
2. Depuis la racine de l'app, lancer `typr std` pour régénérer les `.bin`
   (les signatures n'ont pas changé ici, mais c'est le réflexe à garder).
3. `cargo build` — les `.bin` sont embarqués via `include_bytes!`.
4. Vérifier avec un projet de test : `typr run` sur un `.ty` contenant un `map(...)`.

## Lien avec les fonctions variadiques

Depuis le correctif rendant le corps des fonctions variadiques utilisable, un paramètre
`...xs: T` est vu comme `[#N, T]` et matérialisé en `typed_vec` au runtime. Cela rend
`sum(xs)`, `length(xs)`, `reduce(xs, …)` opérationnels dans le corps — mais **pas**
`map(xs, …)`, tant que `map.typed_vec` n'est pas ajouté. C'est la seule restriction
connue côté runtime pour le variadique.
