# : Was declared as a public function

## Ce qui devrait se passer

`scene.ty` déclare `@pub let add <- fn(self: Scene, obj: Object): Scene { ... }`, une nouvelle
surcharge de `add` qui dispatch structurellement sur `Scene` (même mécanisme que les autres
fonctions "interface" : dispatch sur le type du premier paramètre). `main.ty` l'importe avec
`use scene::add;` puis appelle `sc.add(c1)`. `typr build` doit type-checker cet appel et le
transpiler en `add(sc, c1)` qui résout `add.Scene` côté R.

## Anomalie (→ règles expect.toml)

Le stdlib définit déjà deux surcharges de `add` :
```
@add: (a: int, b: int) -> int;
@add: (a: num, b: num) -> num;
```
`use scene::add;` échouait avec :
```
Type error: The variable add<Empty> is immutable
help: Try to replace the 'let' keyword by the 'mut' keyword
```
puis l'appel `sc.add(c1)` échouait avec :
```
Type error: Function add<list{...Scene...}> not defined in this scope.
```
et `R/main.R` transpilait l'appel en `print(NA)` au lieu de `print(add(sc, c1))`.

## Cause

`has_real_conflict` (`crates/typr-core/src/processes/type_checking/mod.rs`), utilisée par le
traitement de `use module::item;` (`Lang::UseModule`, branches `Wildcard` et `Items`), ne
regardait que si un nom était **déjà présent** dans le contexte — sans tenir compte du fait que
TypR autorise plusieurs surcharges d'un même nom de fonction dispatchant sur des premiers
paramètres différents (le mécanisme d'"interface" structurelle, voir `Context::get_functions_from_type`).
Elle excluait déjà le cas du placeholder stdlib non typé (`Any` + `UnknownFunction`, cf. case
0004 / bug "Position"), mais pas le cas d'une fonction stdlib réellement typée et surchargée
(`add`, `mul`, `div`, `minus`, ...). Du coup, importer une nouvelle surcharge d'un nom déjà
utilisé par le stdlib (même avec un premier paramètre différent) déclenchait un faux conflit
`TypeError::ImmutableVariable`, et l'import échouait silencieusement (pas d'ajout au contexte) →
l'appel suivant ne trouvait plus aucune surcharge pour `Scene`.

## Fix

`has_real_conflict` prend maintenant le type du membre importé et ne considère plus que c'est un
vrai conflit quand : la variable existante n'est pas une fonction, ou que c'est une fonction dont
le type du premier paramètre est **identique** à celui importé (vraie redéfinition). Une
surcharge sur un nouveau premier paramètre n'est plus rejetée.

## Localisation (#@case)

- `TypR/circle.ty:10` — : Was declared as a public function
- `TypR/main.ty:5` — : `move` was imported in the scope
- `TypR/main.ty:8` — : `Circle` is a subtype of `Position`
- `TypR/main.ty:10` — : `move` should works on type `Circle`
- `TypR/position.ty:1` — : `Position` type publicly defined here
- `TypR/position.ty:7` — : `move` function publicly defined here
- `crates/typr-core/src/processes/type_checking/mod.rs` — `has_real_conflict` (fix)
