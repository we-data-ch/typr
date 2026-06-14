# Bug — Fonction `@pub` typée dans un module : R généré cassé + `document()` échoue

## Symptôme

`typr build` sur un projet contenant une fonction `@pub` **typée** déclarée **à l'intérieur d'un
`module {}`** génère du R incorrect, et l'étape finale `document()` (roxygen2/devtools) avorte :

```
6: `g`.default
      ^
Appels : <Anonymous> ... update_namespace_imports -> namespace_imports -> lapply -> FUN
Exécution arrêtée
```

## Portée

**Spécifique aux modules.** La *même* déclaration au top-level (hors module) fonctionne :

| Contexte | R généré | `document()` |
|---|---|---|
| Top-level `@pub let h <- fn(): int { 7 };` | `#' @method h` + `` `h` <- … `` | ✅ réussit |
| Dans `module M { @pub let f <- fn(): int { 42 }; }` | `` `f`.default <- … `` (pas de `#' @method`) + `M$f <- f` | ❌ échoue |

## Reproduction minimale

```bash
typr new repro && cd repro
printf 'mod mid;\nprint("hi");\n'                         > TypR/main.ty
printf 'mod leaf;\n@pub let f <- fn(): int { 42 };\n'      > TypR/mid.ty
printf '@pub let g <- fn(): int { 1 };\n'                  > TypR/leaf.ty
typr build
```

`R/leaf.R` généré (problématique) :

```r
#' @include std.R
#' @include generic_functions.R
#' @include types.R
leaf <- new.env(parent = emptyenv())
local({
`g`.default <- (function() {            # (1) suffixe .default indu, (2) pas de #' @method
1L |> as.Integer()
} |> as.Integer()) |> as.Function0()

leaf$g <- g                            # (3) référence `g` non défini (seul `g`.default existe)
})
```

## Anomalies dans le R généré (3 distinctes)

1. **Mauvais suffixe** : le binding est nommé `` `g`.default `` au lieu de `` `g` ``. C'est la
   branche `Type::Any | Type::Generic` de `Lang::Let` qui est prise au lieu de la branche `_`
   (type concret).
2. **Tag `#' @method` manquant** : la branche `.default` n'émet pas le `#' @method …` que la
   branche concrète émet (visible au top-level).
3. **Export incohérent** : la logique d'export du module fait `leaf$g <- g`, mais seul
   `` `g`.default `` a été défini → `g` est introuvable à l'exécution.

## Localisation dans le code

`crates/typr-core/src/processes/transpiling/mod.rs`, branche `Lang::Let` (~ligne 869) :

```rust
let related_type = Var::try_from(expr)
    .ok()
    .map(|v| v.get_type())
    .filter(|t| !matches!(t, Type::Empty(_) | Type::UnknownFunction(_)))
    .unwrap_or_else(|| typing(cont, expr).value);   // <-- fallback
...
match related_type {
    Type::Empty(_)             => format!("{} <- {}", new_name, body_str),
    Type::Any(_) | Type::Generic(_, _) =>
        format!("{}.default <- {}", new_name, body_str),   // <-- (1)+(2) : branche prise à tort
    _ => format!("{}{} <- {}", method, new_name, body_str),// branche attendue (avec #' @method)
}
```

Logique d'export du module : même fichier, branche `Lang::Module` (~ligne 1420-1445), qui calcule
`typed_name` via `v.display_type(cont).get_name()` et émet `{name}${typed_name} <- {typed_name}`
→ anomalie (3).

## Hypothèse de cause racine (à confirmer)

À l'intérieur d'un `module`, le `related_type` de la `Let` se résout en `Any`/`Generic` au lieu du
type `Function` concret, alors qu'au top-level il se résout correctement. Probablement parce que,
dans le contexte du corps de module, soit `Var::try_from(expr).get_type()` renvoie un type vide/
inconnu (donc fallback sur `typing(cont, expr)`), soit `typing(cont, expr)` lui-même perd
l'information de type de la variable dans le scope du module. Le `display_type` côté export (3)
souffre vraisemblablement du même déficit d'information de type dans le contexte du module.

Pistes d'investigation :
- comparer `typing(cont, expr).value` pour la même `fn(): int {…}` au top-level vs dans un module
  (instrumenter ou test `FluentParser` avec `module M { … }` puis inspecter) ;
- vérifier comment le contexte de typage est peuplé pour les membres d'un module dans
  `processes/type_checking/mod.rs` (traitement de `Lang::Module`) — la déclaration typée du membre
  est-elle bien enregistrée avec son type `Function` avant la transpilation ?
- vérifier `Var::try_from(expr).get_type()` : la variable `f`/`g` porte-t-elle son type au moment
  de transpiler la `Let` dans le corps du module ?

## Indépendance vis-à-vis du hoisting `@include`

Ce bug est **sans rapport** avec l'option A (hoisting des `@include`) déjà livrée, et avec
l'option C (`option_C.md`). Il préexiste et concerne le codegen des fonctions `@pub` typées dans
les modules. Il **bloque** toutefois la validation end-to-end de A/C (le `document()` final
échoue), d'où l'intérêt de le corriger pour pouvoir tester un build de projet à modules complet.

## Critères d'acceptation du correctif

1. `module M { @pub let f <- fn(): int { 42 }; }` génère, dans `R/M.R`, le même schéma que le
   top-level : `#' @method f` + `` `f` <- … `` (type concret), **pas** `` `f`.default ``.
2. L'export du module référence un symbole réellement défini (cohérence entre la `Let` transpilée
   et `M$… <- …`).
3. `typr build` sur le projet de repro va jusqu'au bout : `document()` réussit.
4. Pas de régression sur `cargo test -p typr-core` (snapshots inclus).
