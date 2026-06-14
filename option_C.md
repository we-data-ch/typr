# Option C — Sortir la définition de module hors du `local()`

## Contexte

L'**option A** (déjà faite, cf. `transpiling/mod.rs` : pile `INCLUDE_STACK`) a résolu le
problème roxygen2 le plus urgent : les `#' @include foo.R` issus des `mod foo;` sont désormais
hoistés dans l'en-tête top-level de chaque fichier `R/*.R` au lieu d'être enterrés dans un
`local({…})`.

L'**option C** est la suite « propre ». Elle traite la cause structurelle : aujourd'hui un
`mod` est traité comme une **instruction du corps** d'un module alors que c'est sémantiquement
une **directive de fichier** (compile-time / ordonnancement). Tant que le corps est emballé dans
un `local({…})`, toute logique de dépendance qu'on y met est invisible pour les outils R qui
raisonnent au niveau top-level (roxygen2, et potentiellement d'autres).

## Objectif

Séparer, dans l'AST et à la transpilation, les **imports** (directives de fichier) du **corps
runtime** d'un module, de sorte que :

1. les déclarations `mod` ne traversent jamais le `local({…})` ;
2. à terme, on puisse sortir aussi la définition d'environnement (`name <- new.env(...)`) du
   `local()` si nécessaire, et exposer une structure top-level propre.

## État actuel (rappel)

`Lang::Module` (`components/language/mod.rs:73`) :

```rust
Module {
    name: String,
    body: Vec<Lang>,            // contient TOUT, y compris les ModuleImport (mod foo;)
    module_position: ModulePosition,   // Internal | External
    config: Config,
    help_data: HelpData,
}
```

Transpilation (`transpiling/mod.rs`, branche `Lang::Module`) :

```r
{generics_top_level}
name <- new.env(parent = emptyenv())
local({
    {body_content}        # <-- mod/imports + runtime mélangés
    {exports}
})
{generic_exports}
```

`mod foo;` est parsé en `Lang::ModuleImport`, résolu en métaprogrammation
(`typr-cli/metaprogramming.rs`) en un `Lang::Module { module_position: External }` inséré
**dans le `body`** à la place du `mod`.

## Conception proposée

### Variante C1 — partitionnement à la transpilation (le moins invasif)

Sans toucher à la structure de l'AST : dans la branche `Lang::Module` de `to_r`, **partitionner**
`body` en deux groupes avant de construire la sortie :

- `imports` = les `Lang::ModuleImport` + les `Lang::Module { External }` (les `mod` résolus) ;
- `runtime` = le reste.

Émettre les `imports` **avant** la ligne `name <- new.env(...)` (donc hors `local()`), et ne
mettre que `runtime` dans le `local({…})`.

- ✅ Réutilise directement la pile `INCLUDE_STACK` d'A (les External modules continuent d'écrire
  leur fichier + d'enregistrer leur include).
- ✅ Pas de changement de l'AST ni du type-checker.
- ⚠️ Le partitionnement par `matches!` est un peu fragile (dépend du fait que les `mod` aient
  bien été résolus en `External` à ce stade).

### Variante C2 — champ dédié dans l'AST (le plus propre, plus lourd)

Ajouter un champ `imports: Vec<Lang>` à `Lang::Module` :

```rust
Module {
    name: String,
    imports: Vec<Lang>,   // NEW : mod/ModuleImport, niveau fichier
    body: Vec<Lang>,      // uniquement le runtime
    module_position: ModulePosition,
    config: Config,
    help_data: HelpData,
}
```

- Le parseur (`parsing/mod.rs`, `module` / `mod_imp`) range les `mod` dans `imports`.
- La métaprogrammation (`metaprogramming.rs`) résout les imports en restant dans `imports`.
- La transpilation émet `imports` hors `local()`, `body` dedans.

Impacts à prévoir (chaque `match` sur `Lang::Module` doit gérer le nouveau champ) :
- `components/language/mod.rs` : variant, `PartialEq` (~494), `get_help_data` (~870, ~1423),
  `to_string`/printer (~961, ~1236), `to_module` (~713).
- `components/language/module_lang.rs` : `ModuleLang::try_from`.
- `processes/parsing/mod.rs` : `module` (~595).
- `processes/type_checking/mod.rs` : traitement de `Module`.
- `processes/transpiling/mod.rs` : branche `Lang::Module` (~1333).
- `typr-cli/metaprogramming.rs` : `import_file_modules_code`.

## Recommandation

Commencer par **C1** (partitionnement à la transpilation) : déverrouille la sémantique « imports
hors local() » sans toucher à l'AST, et reste un sous-ensemble strict de C2. Passer à **C2**
seulement si on a besoin de la séparation imports/corps ailleurs que dans la transpilation
(type-checking, métaprogrammation, outils).

## Critères d'acceptation

1. Pour un `module Foo { mod bar; … }`, le `R/Foo.R` généré place la dépendance de `bar`
   **hors** du `local({…})` (déjà partiellement vrai via A pour le `@include` ; C garantit que
   plus rien lié aux imports n'est dans `local()`).
2. Le `local({…})` ne contient que du runtime (let/fn/exports).
3. Aucun régression sur les tests existants (`cargo test -p typr-core`, snapshots inclus).
4. Un build de projet à `mod` imbriqués (main → mid → leaf) produit un ordre de `Collate`
   correct et charge sans erreur **une fois le bug `bug.md` corrigé**.

## Dépendances

- Réutilise l'infra `INCLUDE_STACK` de l'option A (`transpiling/mod.rs`).
- ⚠️ Le test de bout en bout (critère 4) est **bloqué** par le bug décrit dans `bug.md`
  (`document()` échoue sur les fns `@pub` typées dans un module). Corriger `bug.md` d'abord pour
  pouvoir valider C end-to-end.
