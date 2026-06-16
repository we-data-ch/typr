# RFC-TR-033 — Spread operator dans `TypeName:{ ... }`

> Statut : **implémenté**. Voir §6 pour le détail des fichiers touchés ; tests : parsing
> (`elements.rs::test_constructor_call_spread_parsing`), typing (`type_checking/mod.rs`,
> tests `test_constructor_spread_*` / `test_constructor_call_*_errors`), transpilation
> (`snapshot_tests.rs::transpilation::constructor_call_spread`), et `lab/spread.ty`.

## 1. Motivation

`type Person <- list { name: char, age: int };` génère déjà un constructeur explicite
utilisable via `Person:{ name = "Bob", age = 12 }` (voir CLAUDE.md, « Constructor System »).

Il n'existe aujourd'hui aucun moyen de construire une nouvelle valeur d'un type record en ne
spécifiant que *certains* champs et en complétant les autres depuis une valeur existante du même
type (pattern « update / with » qu'on trouve dans beaucoup de langages : spread `{...obj}` en JS,
`record { x, ..base }` en Rust struct update syntax, etc.).

Objectif :

```typr
let bob <- Person:{ name = "Bob", age = 12 };
let alice <- Person:{ name = "Alice", ..bob };   // age = 12, repris de bob
```

## 2. Syntaxe

Dans la liste de champs d'un `ConstructorCall` (`TypeName:{ ... }` ou `mod$TypeName:{ ... }`),
un élément `..<variable>` est autorisé en plus des champs `name = expr` habituels :

```typr
TypeName:{ champ1 = expr1, ..source }
TypeName:{ ..source, champ1 = expr1 }   // position libre
```

Règles de syntaxe :

- Le token est `..` (deux points), à ne pas confondre avec `...` (variadique de paramètres de
  fonction, déjà existant) ni avec `Lang::Dots` / `dotdotdot()`.
- `<variable>` est une expression simple (`variable_exp`), pas une expression arbitraire — on
  réutilise la même brique que pour le `module_path` de `constructor_call`.
- **Un seul spread par appel de constructeur** dans cette v1 (erreur de parsing ou de typing si
  plusieurs `..x` apparaissent dans le même bloc — choisir l'un des deux, voir §6 « Décisions
  ouvertes »). Cette restriction simplifie nettement la résolution des priorités et peut être
  levée plus tard si besoin.
- Position libre dans la liste de champs (avant, après, ou entre des champs explicites).

## 3. Sémantique

- Les champs explicites **priment toujours** sur le champ correspondant fourni par le spread,
  **indépendamment de l'ordre textuel**. C'est-à-dire que `{ name = "Alice", ..bob }` et
  `{ ..bob, name = "Alice" }` sont strictement équivalents : `name` vaut `"Alice"` dans les deux
  cas. (Contrairement au spread JS où l'ordre d'écriture fait foi — ici la sémantique est
  « valeurs explicites + complétion », pas « merge ordonné ».)
- Le type de la variable spread doit être **exactement le même alias** que `TypeName` (égalité
  nominale, pas juste structurelle) en v1. Une variable d'un type structurellement compatible
  mais d'alias différent est rejetée — limitation volontaire pour rester simple et sûre ; pourra
  être relâchée plus tard vers une compatibilité structurelle.
- Tous les champs du record doivent être couverts après résolution (explicite ∪ spread). S'il en
  manque un (et qu'aucun spread n'est présent, ou que le spread ne couvre pas tout — impossible
  vu la règle précédente sauf champs ajoutés ultérieurement au type — donc en pratique : pas de
  spread + champ manquant), c'est une erreur de type, **comme c'est déjà censé être le cas pour
  un `ConstructorCall` sans spread** (voir §4, ce n'est actuellement pas vérifié — bug
  préexistant indépendant de cette RFC).
- Un champ ne doit pas être fourni deux fois explicitement (`{ name = "A", name = "B" }`) :
  erreur de type, indépendant du spread.

## 4. État actuel du pipeline (constat de la session de design)

Recherche faite dans la session précédente, valable à la date de rédaction :

- **Parsing** — `crates/typr-core/src/processes/parsing/elements.rs::constructor_call`
  (~ligne 618) construit `Lang::ConstructorCall { module_path, type_name, fields, help_data }`
  où `fields: Vec<ArgumentValue>`. `ArgumentValue` = tuple `(String, Lang)` défini dans
  `crates/typr-core/src/components/language/argument_value.rs`.
- **AST** — `Lang::ConstructorCall` défini dans
  `crates/typr-core/src/components/language/mod.rs` (~ligne 286).
- **Type-checking** — `crates/typr-core/src/processes/type_checking/mod.rs` (~ligne 2369), le
  match arm `Lang::ConstructorCall { module_path, type_name, fields: _, help_data: h }` **ignore
  totalement `fields`** (`fields: _`). Il résout seulement `type_name` en alias et renvoie
  `Type::Alias(type_name, ...)`, ou `Any` si l'alias n'existe pas. **Aucune validation des champs
  fournis n'est faite aujourd'hui**, spread ou non — c'est un prérequis à corriger pour cette RFC
  (et un bug à signaler indépendamment).
- **Transpilation** — `crates/typr-core/src/processes/transpiling/mod.rs` (~ligne 1712), génère
  `TypeName(field1 = val1, field2 = val2)` via `Translatable::join_arg_val(fields, ", ")`. Pour
  un chemin de module non vide : `module$TypeName(...)`.
- **Représentation du record** — `Type::Record(HashSet<ArgumentType>, HelpData)` dans
  `crates/typr-core/src/components/type/mod.rs`. **Non ordonné** (`HashSet`). Sans conséquence
  ici car les arguments R générés sont nommés (`champ = valeur`), donc l'ordre d'émission
  n'affecte pas la sémantique R.
- `ArgumentType` = tuple `(Type /* nom de champ encodé en Type::Char */, Type /* type du champ */,
  bool /* embedded */, bool /* variadic */)` dans
  `crates/typr-core/src/components/type/argument_type.rs`.

## 5. Traduction R

**Stratégie retenue : expansion statique à la transpilation** (pas de mécanisme runtime).

Comme le type record cible est connu statiquement (résolu via le contexte typant), on peut
calculer, au moment de transpiler, l'ensemble des champs non couverts explicitement et les
remplacer par un accès `source$champ` :

```typr
type Person <- list { name: char, age: int };
let bob <- Person:{ name = "Bob", age = 12 };
let alice <- Person:{ name = "Alice", ..bob };
```

transpile en :

```r
Person <- function(name, age) {
  structure(list(name = name, age = age), class = c("Person", "list"))
}
bob <- Person(name = "Bob", age = 12)
alice <- Person(name = "Alice", age = bob$age)
```

Pourquoi pas un merge runtime (`modifyList` / `do.call`) :

- `modifyList()` ne préserve pas les attributs (donc perd la classe `c("Person", "list")`) —
  il faudrait re-wrapper après coup, donc pas plus simple.
- Aucune information n'est dynamique ici : tous les champs et leurs noms sont connus à la
  compilation. Générer du code explicite est strictement aussi expressif, plus lisible dans le
  R généré, sans coût runtime ni dépendance supplémentaire — cohérent avec le reste du
  « Constructor System » qui mise sur la génération inline plutôt que sur des primitives R
  génériques.

## 6. Plan d'implémentation

1. **AST** : ajouter un champ optionnel à `Lang::ConstructorCall`, par ex.
   `spread: Option<(Vec<String> /* module_path de la variable spread, vide si locale */, String /* nom de variable */, HelpData)>`.
   Garder `ArgumentValue` inchangé (pas besoin d'y toucher, le spread est porté par
   `ConstructorCall` uniquement — `Lang::List` / `list{...}` n'a pas besoin de cette
   fonctionnalité pour l'instant).
2. **Parsing** (`elements.rs::constructor_call`) : dans la boucle qui consomme les éléments du
   bloc `{ ... }`, accepter en plus de `many0(argument_val)` une alternative reconnaissant
   `".." variable_exp ","?`. Mutuellement exclusif avec les `argument_val` au niveau du nom
   (un seul spread total, cf. §2) — le plus simple est de parser séparément :
   `many0(alt((argument_val, spread_field)))` puis, après coup, vérifier qu'il y a au plus un
   `spread_field` parmi les résultats (erreur de parsing sinon, via un check post-`many0` qui
   renvoie une `nom::Err` si > 1, ou repousser la vérification au type-checking — préférer le
   type-checking pour avoir un message d'erreur exploitable avec `HelpData`).
3. **Type-checking** (`type_checking/mod.rs`, match arm `ConstructorCall`) :
   - résoudre `type_name` → `Type::Record(fields, _)` (en suivant `module_path` comme c'est déjà
     fait) ;
   - si un spread est présent : type-checker la variable spread, vérifier qu'elle a exactement
     le type `Type::Alias(type_name, ...)` (égalité nominale) ; sinon erreur de type dédiée
     (nouveau variant `TypeError`, ex. `SpreadTypeMismatch { expected, found, help_data }`) ;
   - calculer l'ensemble des noms de champs requis par le record, l'ensemble fourni explicitement
     par `fields`, et vérifier :
     - pas de doublon dans `fields` (nouvelle erreur `DuplicateField` ou réutiliser une erreur
       existante si une vérification équivalente existe déjà ailleurs — à vérifier) ;
     - chaque champ explicite existe dans le record et a le bon type (comparaison structurelle
       habituelle, probablement déjà disponible via une fonction utilitaire de sous-typage) ;
     - tout champ non couvert explicitement doit être couvert par le spread (s'il y en a un),
       sinon erreur `MissingField` ;
   - cette étape corrige du même coup le bug préexistant où `fields` n'était pas vérifié du
     tout, même sans spread (voir §4).
4. **Transpilation** (`transpiling/mod.rs`, match arm `ConstructorCall`) :
   - si pas de spread : comportement actuel inchangé ;
   - si spread présent : avant d'appeler `join_arg_val`, construire la liste complète des
     `ArgumentValue` en ajoutant, pour chaque champ du record absent de `fields`, une entrée
     synthétique `ArgumentValue(champ, <expression d'accès source$champ>)`. Vérifier quel nœud
     `Lang` existant représente un accès de champ R (`x$champ`) — chercher comment `Op::Dollar`
     ou équivalent est généré ailleurs dans le transpiler (ex. accès `.` sur une variable de
     type record) et réutiliser ce nœud plutôt que de fabriquer une chaîne R à la main, pour
     rester cohérent avec le reste du pipeline (gestion de `module_path` etc. si la variable
     spread elle-même vient d'un autre module).
5. **Tests** :
   - `FluentParser` : parsing de `TypeName:{ a = 1, ..b }`, vérifier l'AST obtenu ;
   - typing : cas valide (tous champs couverts), cas `MissingField`, cas `SpreadTypeMismatch`,
     cas priorité champ explicite > spread ;
   - snapshot de transpilation : vérifier le R généré (`Person(name = "Alice", age = bob$age)`) ;
   - un cas dans `lab/` (ex. `lab/spread.ty`) couvrant le scénario `Person`/`bob`/`alice` du §5,
     à valider avec `nu debug.nu lab/spread.ty`.
   - éventuellement un cas dans `cases/` si le besoin de non-régression se confirme après
     implémentation.

## 7. Décisions ouvertes (à trancher avant ou pendant l'implémentation)

- **Plusieurs spreads dans un même bloc** : actuellement interdit (v1). Si on l'autorise plus
  tard, définir l'ordre de priorité entre spreads (ex. premier listé prioritaire ? dernier ?) —
  pas nécessaire pour la v1, mais documenter le choix si ça change.
- **Compatibilité structurelle vs nominale** : v1 exige l'égalité exacte d'alias entre la
  variable spread et `TypeName`. À revoir si un besoin de compatibilité structurelle (deux alias
  différents avec les mêmes champs) se manifeste.
- **Spread sur un module path** (`..mod$bob`) : la grammaire de `constructor_call` gère déjà un
  `module_path` pour le *type* construit ; il faudra décider si la variable spread elle-même peut
  être qualifiée par un chemin de module (`..person_mod$bob`), et réutiliser `variable_exp` en
  conséquence — `variable_exp` semble déjà gérer ce cas pour le `module_path` actuel, donc a
  priori oui sans travail supplémentaire, mais à confirmer en lisant `variable_exp`.
- **Nom de l'erreur de type** : vérifier les variants déjà existants dans
  `components/error_message/type_error.rs` avant de créer `MissingField` /
  `SpreadTypeMismatch` / `DuplicateField` — peut-être qu'un mécanisme générique de comparaison
  structurelle de record existe déjà ailleurs et peut être réutilisé tel quel.
