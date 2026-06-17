# : Was declared as a public function

## Ce qui devrait se passer

`typr build` doit réussir : `add(self: Scene, obj: Object): Scene` doit type-checker, et
`R/object.R` doit charger sans erreur.

## Anomalies trouvées (4 bugs distincts, empilés dans ce repro)

1. **(FIXED)** Commentaire `#...` dans un literal `TypeName:{ ... }` cassait le parsing.
   `constructor_call`/`record` (parsing/elements.rs) ne géraient que `multispace0` entre les
   champs, pas les lignes `# commentaire`. Un `#@case: ...` placé juste après `Scene:{` faisait
   échouer tout le literal → la fonction entière retombait en `Empty`/`NA`. Fix : nouveau
   combinateur `ws0` (whitespace + commentaires `#...`) utilisé autour de `constructor_field` /
   `record_field` et avant la fermeture `}`.

2. **(FIXED)** `self$objects` (type déclaré `[Object]`) se réduisait en `[any, any]` au lieu de
   `[any, Object]` dès que `Object` est un alias d'alias (`type Object <- Circle;`) **exporté
   depuis un autre module puis réimporté ailleurs** (`use object::Object;` dans scene.ty). En
   cause : à l'export d'un alias non-opaque (`type_checking/mod.rs`, construction de
   `pub_arg_types` pour `Lang::Module`), le type exporté était le `target_type` *brut tel
   qu'écrit* (`Type::Alias("Circle", ...)`), jamais résolu. Le fichier important (`scene.ty`)
   n'a jamais importé `Circle` lui-même, donc `reduce_type` ne pouvait pas le résoudre et
   retombait sur `Any`. Fix : résoudre l'alias via le contexte interne du module
   (`typing_context.context`, où `Circle` est enregistré) avant de l'exporter.

3. **(OPEN, hors scope $ )** `type Object <- Circle;` (alias transparent d'un autre alias de
   record) cause `objet 'Object' introuvable` à la génération R : comme `Object` n'est pas
   directement un `list { ... }`, aucun constructeur `Object <- function(...)` n'est généré
   dans `object.R`, mais l'export module (`object$Object <- Object`) suppose qu'il existe.

4. **(OPEN, hors scope $, pré-existant — indépendant des fixes 1/2)** Le nom `Position` collide
   avec la fonction de base R `Position()`, préchargée dans tout contexte comme
   `(Var("Position"), UnknownFunction)` à la fois dans `variables` et `std`
   (`crates/typr-cli/src/standard_library.rs::build_function_list_vartype`,
   `functions_R.txt:487`). La vérification anti-conflit de `use module::Name;`
   (`Lang::UseModule`, branche `Items`, type_checking/mod.rs) ne distingue pas ce placeholder
   stdlib d'une vraie collision utilisateur → `use position::Position;` échoue avec
   "The variable Position<Empty> is immutable" dès que le type s'appelle pareil qu'une fonction
   R de base (`Position`, `Reduce`, `Filter`, `t`, ...).

## Localisation (#@case)

- `TypR/circle.ty:10` — : Was declared as a public function
- `TypR/main.ty:5` — : `move` was imported in the scope
- `TypR/main.ty:8` — : `Circle` is a subtype of `Position`
- `TypR/main.ty:10` — : `move` should works on type `Circle`
- `TypR/position.ty:1` — : `Position` type publicly defined here
- `TypR/position.ty:7` — : `move` function publicly defined here
