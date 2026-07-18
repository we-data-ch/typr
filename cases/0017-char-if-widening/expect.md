# char-if-widening

## Ce qui DEVRAIT se passer

`let x0: char <- if (true) { "a" } else { "b" };` devrait type-checker : les
deux branches sont des `char`, l'annotation est `char`. C'est exactement le
même pattern que `let x0: int <- if (true) { 1 } else { 2 };` (OK) ou
`let x0: bool <- if (true) { true } else { false };` (OK) — seul `char`
échoue.

## Ce qui se passe

`Type char doesn't match type ("a" | "b")`. `Lang::If`'s typing (mod.rs
`Lang::If` arm, ~ligne 1439) essaie de collapser les deux branches via
`reduced_true == reduced_false || is_subtype` dans les deux sens avant de
retomber sur `union_type(&[...])` si rien ne collapse. Pour `int`/`bool`, la
branche `Type::Integer/_ => true` de `is_subtype_raw` (components/type/mod.rs
~ligne 271) rend n'importe quel entier littéral sous-type de n'importe quel
autre — donc les deux branches collapsent trivialement. Pour `char`, la
branche `(Type::Char(t1,_), Type::Char(t2,_)) => t1.is_subtype(t2)` délègue à
`Tchar::is_subtype`, qui n'autorise qu'un match exact de la valeur littérale
ou un élargissement vers `Unknown` (tchar.rs) — donc `"a"` et `"b"` ne
collapsent jamais, et le `if` retombe sur le type union littéral
`"a" | "b"`, qui ne matche pas l'annotation `char`.

## Piste explorée et écartée

Rendre `(Type::Char(_,_), Type::Char(_,_)) => true` (aligné sur `Integer`)
fait passer ce cas mais **casse deux tests existants et délibérés** :
`components::r#type::tests::test_litteral_subtyping3` (`"html"` ne doit PAS
être sous-type de `"h1"`) et
`function_application::tests::test_union_litteral2` (résolution
d'overload change de branche). La stricture de `Tchar::is_subtype` est donc
voulue ailleurs (probablement pour la résolution de surcharge sur
littéraux/dispatch) — ce n'est pas un simple bras manquant façon
`nlevels.default`. Le vrai fix doit vivre plus localement, probablement dans
la logique de collapse de `Lang::If` elle-même (généraliser les deux
branches — `Type::generalize()`, qui gère déjà `Char(Val(_)) ->
Char(Unknown)` — avant de comparer, plutôt que retoucher `is_subtype_raw`
globalement) ou dans la comparaison annotation-vs-valeur au niveau du `Let`.
Pas tenté ici : nécessite de vérifier l'impact sur la résolution de
surcharge basée sur les littéraux ailleurs dans `function_application.rs`.

## Deuxième occurrence trouvée (même famille, code path différent)

Le générateur Phase B a aussi trouvé que le champ d'un record littéral ne
widen pas non plus, **même sans aucun `if`** :

```typr
let x: list{f0: char} <- :{ f0 = "typr" };  // échoue : "type list{f0: char} doesn't match type list{f0: \"typr\"}"
let x: list{f0: int}  <- :{ f0 = 5 };       // OK
```

`Type::PartialEq` (components/type/mod.rs ~ligne 1197-1251) a exactement la
même asymétrie qu'`is_subtype_raw` : `(Integer(_,_), Integer(_,_)) => true`
(bras large) vs `(Char(t1,_), Char(t2,_)) => t1 == t2` (égalité exacte de
`Tchar`, donc `Val("typr") != Unknown`). `Lang::List`/`ConstructorCall`
(mod.rs, typage des champs) construisent le `Type::Record` du littéral en
gardant le type inféré brut de chaque champ (`tc.value.clone()`, sans
`.generalize()`) — contrairement à `Lang::Array`/`Lang::Vector`
(`typing_container`) qui appelle explicitement
`types[0].clone().generalize()` avant de construire le type du tableau, ce
qui est justement pourquoi `let x: [3, char] <- ["a","b","c"];` fonctionne
très bien (vérifié). Généraliser aussi les champs de record romprait
vraisemblablement les tests qui dépendent d'un type de champ littéral exact
pour la génération de validateurs (`test_literal_char_alias_generates_exact_validator`
et consorts dans `processes/transpiling/mod.rs`) — donc pas un fix évident
non plus, même circonscrit à ce point d'entrée.

**Conclusion** : ce n'est pas un bug isolé à `if`/`else`, c'est une question
de design plus large (« quand un type littéral doit-il s'élargir vers son
type de base ? ») qui traverse au moins 3 mécanismes indépendants
(subtyping, égalité, construction de record) avec des réponses
incohérentes entre eux et vis-à-vis d'`Integer`. Mérite une session dédiée
plutôt qu'un correctif ponctuel.

## Localisation du code

- `crates/typr-core/src/processes/type_checking/mod.rs`, `Lang::If` arm
  (~ligne 1415-1457), la logique de `collapses`.
- `crates/typr-core/src/components/type/mod.rs`, `is_subtype_raw`
  (~ligne 193), bras `Type::Char`/`Type::Integer` (~ligne 270-271).
- `crates/typr-core/src/components/type/tchar.rs`, `Tchar::is_subtype`.
- `Type::generalize()` (`components/type/mod.rs` ~ligne 813) sait déjà
  effacer un littéral `Char`/`Integer` — piste probable pour un fix localisé
  au lieu de toucher `is_subtype_raw`.
