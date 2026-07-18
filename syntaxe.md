# TypR — carte de la syntaxe

> Référence non officielle, établie par lecture directe du parseur (`crates/typr-core/src/processes/parsing/`)
> — reflète l'état du code à la date de génération, pas une spécification figée.
>
> Sources principales : `parsing/mod.rs`, `parsing/elements.rs`, `parsing/types.rs`,
> `components/language/operators.rs`, `parsing/indexation.rs`, `components/language/mod.rs`.

## Sommaire

1. [Lexique & littéraux](#1--lexique--littéraux)
2. [Liaisons & mutation](#2--liaisons--mutation)
3. [Système de types](#3--système-de-types)
4. [Alias, opaque, typeconstructor, signatures](#4--alias-opaque-typeconstructor-signatures)
5. [Fonctions](#5--fonctions)
6. [Records & constructeurs](#6--records--constructeurs)
7. [Unions, tags & `match`](#7--unions-tags--match)
8. [Opérateurs & précédence](#8--opérateurs--précédence)
9. [Contrôle de flux](#9--contrôle-de-flux)
10. [Modules & imports](#10--modules--imports)
11. [Interfaces & validation structurelle](#11--interfaces--validation-structurelle)
12. [Échappatoires vers du code brut](#12--échappatoires-vers-du-code-brut)
13. [TypR vs R — ce qui change vraiment](#13--typr-vs-r--ce-qui-change-vraiment)
14. [Ambiguïtés & pièges connus du parseur](#14--ambiguïtés--pièges-connus-du-parseur)

---

## 1 · Lexique & littéraux

Chaque instruction de haut niveau se termine par `;`. L'oubli est toléré par le parseur mais
signalé (`SyntaxError::ForgottenSemicolon`) — sauf pour la toute dernière expression d'un bloc,
qui joue le rôle de valeur de retour implicite (comme en R).

| Littéral | Syntaxe | Remarque |
|---|---|---|
| Entier | `42`, `-7` | `Lang::Integer` |
| Nombre | `3.14`, `-0.5` | un point décimal est obligatoire pour `Number`, sinon c'est un `Integer` |
| Chaîne | `"texte"` ou `'texte'` | guillemets simples et doubles interchangeables ; échappe `\" \' \\ \n \t` |
| Booléen | `true` / `TRUE`, `false` / `FALSE` | forme minuscule *et* majuscule acceptées |
| Nul | `null` / `NULL` | `Lang::Null` — distinct de `NA` |
| Manquant | `na` / `NA` | `Lang::NA` — distinct de `null` |
| Identifiant variable | `mon_nom` | doit commencer par `a-z_` ; `snake_case` attendu |
| Identifiant type | `MonType` | `PascalCase` obligatoire pour `type`/`opaque`/alias |
| Identifiant cité | `` `+` ``, `` `nom bizarre` `` | backticks — utile pour nommer un opérateur custom |
| Commentaire | `# commentaire` | `//` n'existe pas — un fichier `.ty` avec `//` échoue silencieusement (piège connu, voir §14) |

Le parseur distingue explicitement `let NomPascal <- ...` (erreur `LetInsteadOfType`) de
`type nom_snake <- ...` (erreur `TypeInsteadOfLet`) — la casse du premier identifiant n'est donc
pas un simple style, elle est vérifiée et corrigée avec un message dédié.

---

## 2 · Liaisons & mutation

### Déclaration `let`

```typr
let x <- 42;
let y: int <- 5;
@pub let z <- "hello";             # exportée : publique + testable
@testable let cache <- state(0);   # privée, visible en build --test via M$.test_cache
@export let api <- fn(x: int): int { x };  # @pub + #' @export roxygen2
```

`<-` et `=` sont interchangeables comme opérateur d'affectation dans `let`. Un seul `=` n'est en
revanche **jamais** un opérateur infixe binaire dans une expression — il est réservé aux champs
nommés (`x = 1`), à l'affectation top-level et aux valeurs par défaut de paramètres.

### Déstructuration de tuple

```typr
let :{a, b, c} <- :{1, 2, 3};
let :{a, _, c} <- :{1, 2, 3};   # wildcard : ignore l'élément
```

Désucré en une variable temporaire + accès positionnel par point (`__tuple_tmp__.1`, `.2`, ...).

### Affectation & mutation

```typr
x <- 10;         # Assign sur une variable déjà liée
x <- x + 1;

# sucre "mutation implicite" : x!; réaffecte x au résultat de l'expression
x |> f() |> g()!;         # ≡  x <- x |> f() |> g();
obj.method()!;            # ≡  obj <- obj.method();
```

`expr!;` exige que la tête de la chaîne `.`/`|>` soit une variable assignable ; `3!;` par exemple
est rejeté au parsing.

---

## 3 · Système de types

### Primitifs

`int` · `num` · `bool` · `char` · `null` · `na` · `Any` · `Empty` · `Self`

Les littéraux peuvent aussi apparaître *comme type* (types singleton) : `3`, `3.14`, `true`,
`"chat"` sont des types valides, plus précis que `int`/`num`/`bool`/`char`.

### Records (`list{...}`)

```typr
type Point <- list { x: int, y: int };
type Config <- record { name: char, timeout: int };   # synonyme explicite
```

### Tuples

```typr
tuple{int, char}          # explicite
Tuple[int, char]           # notation crochets
Tuple[T..., U]             # variadique : T... capture une séquence de types
```

### Tableaux, vecteurs, dataframes

| Forme | Exemple | Sens |
|---|---|---|
| Array (S3, court) | `[int]` | tableau d'entiers, taille libre (`Any`) |
| Array (S3, complet) | `[#N, int]` | taille indexée par le générique `#N` |
| `Array[...]` | `Array[3, int]` | variante nommée, taille fixe = 3 |
| `Vec[...]` | `Vec[num]`, `Vec[#N, num]` | vecteur R natif |
| `dataframe[...]{...}` | `df[#N]{ name: char, age: int }` | `df` = alias court de `dataframe` |
| Record générique nommé | `Tibble[3]{ id: int, active: bool }` | nécessite une déclaration `typeconstructor` préalable |

Règle d'or du parseur : un bloc `{ ... }` qui suit fait toujours de la construction un *record* —
`Tibble[3]` seul (sans accolades) est un simple alias paramétré, `Tibble[3]{ ... }` est un record
constructeur.

### Génériques & sigils de « kind »

```typr
let id <- fn(x: T): T { x };         # T majuscule = générique libre
#N     # générique "indice" (dimension de tableau)
$T     # générique "label" (nom de champ)
%R     # générique contraint : doit être un Record
@I     # générique contraint : doit être une Interface
^S     # générique contraint : doit être un char
?B     # générique contraint : doit être un bool
```

### Unions & intersections

```typr
type Shape <- .Circle(num) | .Square(num);   # union par tags
type Combined <- Movable & Drawable;          # intersection d'interfaces
```

### Fonctions & interfaces (en position de type)

```typr
(int, char) -> bool                        # type fonction anonyme
(a: int, b: int) -> int                     # noms de paramètres optionnels, ignorés au typage
interface { view: (Self) -> char }          # capacité structurelle
```

Écrire `fn(a: int) -> int` en *position de type* (au lieu de `(int) -> int`) est capté
explicitement et renvoie `SyntaxError::FunctionTypeSyntax` — `fn(...)` n'existe qu'au niveau des
expressions, jamais des types.

### Arithmétique de types & conditions

```typr
type Combined <- A + B;      # Type::Operator sur des index/dimensions
T if T1 in T2                 # type conditionnel (raffinement expérimental)
```

---

## 4 · Alias, opaque, typeconstructor, signatures

```typr
type Meters <- int;                 # alias transparent
opaque Meters <- int;                # alias opaque : le sous-jacent est caché du typage externe
type Option<T> = .Some(T) | .None;   # alias générique — <T>, pas [T]
opaque Factor<L> <- int;             # paramètre "fantôme" : L n'apparaît que dans de futures @signature

typeconstructor Tibble[N] record;    # enregistre un constructeur générique de record
typeconstructor Matrix[N, M, T] recursive;
```

#### Signatures (déclaration de type sans corps)

```typr
@map: (a: [#N, T], f: (T) -> U) -> [#N, U];   # générique
@add: (a: int, b: int) -> int;                 # surcharge : répéter @add avec d'autres types
@add: (a: num, b: num) -> num;
@as__character: (Self) -> char;                # "__" → "." en sortie R (as.character)

@extern stats::sd: (x: [Any, num]) -> num;      # fonction R externe, appel réel package::fn
@extern base::readRDS: (path: char) -> Foreign<Any>;  # nom nu = fonction déjà visible sans préfixe pkg::
@importFrom dplyr filter select mutate;          # hisse un @importFrom roxygen2
```

Une signature `.ty` peut contenir un vrai corps `let nom <- fn(...){...}` et être type-checkée
avec succès — mais ce corps est **silencieusement jeté** : seule la paire `(nom, type)` survit.
Le seul patron qui marche pour la stdlib est signature seule + implémentation R écrite à la main
dans `std.R`.

#### `Foreign<T>` — valeurs R externes opaques

`opaque Foreign<T> <- Any;` (`configs/std/foreign.ty`) est le type compagnon idiomatique de
`@extern` pour *nommer* une valeur R qui existe déjà à l'extérieur (objet S3/S4/RC/R6, package
tiers) sans jamais la construire depuis TypR :

```typr
type LmModel <- Foreign<Any>;
@extern base::readRDS: (path: char) -> LmModel;
@extern stats::coef: (m: LmModel) -> Foreign<Any>;   # accesseur dédié pour toucher au contenu

let m: LmModel <- readRDS("modele.rds");
```

La valeur traverse `let` annoté, arguments et retours de fonction **sans jamais être touchée**
(pas de `as.X()`/`struct()` appliqué). Deux limites volontaires à connaître : `m.champ`/`m$champ`
ne type-check **jamais** (`Any` n'a aucun champ structurel connu — il faut un accesseur `@extern`
dédié pour chaque champ/slot/méthode) ; et un appel `@extern` est **positionnel uniquement**, donc
impossible d'appeler directement une fonction R qui exige des arguments nommés (`new("X", x=1)`).
Détail complet — mécanisme, quatre bugs trouvés et corrigés, grille de conformité — dans
`CLAUDE.md` § "Foreign R Values" et `interop_matrix.md` (racine du repo).

---

## 5 · Fonctions

```typr
let add <- fn(a: int, b: int): int { a + b };
let greet <- fn(name: char, greeting: char = "Hello"): char { greeting };  # défaut, param final seulement
greet("World");        # "Hello"
greet("World", "Hi");   # "Hi"

let sum_all <- fn(...xs: int): int { /* ... */ };   # variadique

let sq <- \(x) x * x;                          # lambda non typée
let add5 <- \add(a = 5);                        # application partielle d'une fonction
let origin <- \Point:{ x = 0, y = 0 };           # application partielle d'un constructeur de record
```

`fn(...)` exige toujours `: Type` de retour — l'omettre panique explicitement ("You forgot to
specify the function return type"). `\(...)` (lambda) n'a en revanche ni type de paramètres ni
type de retour déclarés.

**Application partielle vs lambda** : les deux utilisent `\`, désambiguïsés par ce qui suit —
`\(` → lambda, `\identifiant(` → application partielle. `PartialApp` se désucre en
`Lang::Function` pendant le typage — il n'atteint jamais la transpilation.

---

## 6 · Records & constructeurs

```typr
let p <- :{ x = 1, y = 2 };                 # record anonyme (nécessite un contexte de type)
let p <- Point:{ x = 1, y = 2 };             # constructeur explicite → transpile en Point(x=1, y=2)
let q <- Point:{ ...p, y = 9 };              # spread "runtime" : fusion structurelle, override après
let r <- mod$Point:{ x = 1, y = 2 };          # constructeur qualifié par chemin de module

let arr <- IntBox:[1, 2, 3];                  # ArrayConstructorCall
```

Autres écritures équivalentes du littéral record : `record{...}`, `object{...}`, `list{...}`,
`:{...}` — la forme est choisie par la *forme* des champs, pas par le mot-clé : des champs
`nom = valeur` donnent un record (`Lang::List`), des valeurs positionnelles donnent un tuple
(`Lang::Tuple`) même avec `list{1, 2, 3}`.

#### Spread — deux mécanismes distincts

```typr
Point:{ ..source }     # spread "statique" nominal — un seul par appel
Point:{ ...source }    # spread "runtime" structurel — un seul dans un constructeur, plusieurs dans un record littéral
:{ ...a, ...b, z = 1 } # record littéral : plusieurs spreads runtime autorisés, fusionnés dans l'ordre puis overridés
```

#### Named type embedding

```typr
type Widget <- list { embed coords: Position, label: char };
```

`embed` est un mot-clé « doux » — reconnu seulement devant `nom: Type` avec espace derrière, donc
un champ réellement nommé `embed` reste parsable.

---

## 7 · Unions, tags & `match`

```typr
type Shape <- .Circle(num) | .Square(num);
let s <- .Circle(3.14);
let n <- .None;                              # tag sans valeur

match s {
    .Circle(r) => r * 2.0,
    .Square(side) => side,
}
```

#### Motifs de `match` disponibles

| Motif | Exemple |
|---|---|
| Tag avec binding | `.Some(a) => a` |
| Tag sans binding | `.None => 0` |
| Motif de type | `x as int => x + 1` |
| Motif record | `:{nom: n, age: a} => a` ou `list(nom = n, age = a) => a` |
| Motif tuple | `:{a, b} => a` |
| Joker | `_ => défaut` |
| Variable simple | `v => v` |

#### Constructeur d'union qualifié

```typr
type Color <- .Red | .Blue;
Color.Red                       # référence qualifiée à un tag sans charge utile (bare, sans `:{...}`)

type Rgb <- list { r: int, g: int, b: int };
type Palette <- .Red | .Blue | Rgb;
Palette.Rgb:{ r = 10, g = 20, b = 30 }   # Rgb est un alias record utilisé tel quel comme membre
                                          # de l'union (pas un tag `.Rgb(...)`) — son constructeur
                                          # généré accepte bien des champs nommés.
```

**Piège** (audit_type_checking.md U1, corrigé) : la syntaxe `Union.Variant:{ field = val }` /
`TagName:{ field = val }` ne fonctionne **que** quand `Variant` est un alias record utilisé
directement comme membre de l'union (cas `Rgb` ci-dessus) — jamais pour un vrai tag `.Variant(...)`
(scalaire ou à corps record), qu'on l'écrive avec ou sans `:{...}`. Le constructeur généré pour un
tag prend toujours **une seule charge utile positionnelle** (`Variant <- function(x) {...}`), jamais
des champs nommés séparés. Pour construire un tag, utiliser `.Variant(valeur)` — et
`.Variant(:{ ... })` quand la charge utile est elle-même un record. Rejeté à la vérification de
type (`TagFieldConstructorNotSupported`) plutôt que de compiler vers du R qui plante.

---

## 8 · Opérateurs & précédence

Précédence réelle (la plus élevée se regroupe en premier) — notez que l'accès membre / pipe se
lie *plus fort* que l'arithmétique, contrairement à la plupart des langages.

| Rang | Opérateurs | Rôle |
|---|---|---|
| 4 (fort) | `.` `\|>` `$` `::` `as!` `in` | accès membre / UFCS, pipe, cast validant |
| 3 | `*` `/` `%` `@` | multiplicatif, produit matriciel |
| 2 | `+` `-` | additif |
| 1 (faible) | `== != < > <= >=`, `and/&&/&`, `or/\|\|/\|`, `%op%` | comparaison, logique, opérateurs custom |

### UFCS — `.` et `|>`

```typr
x.f(y)          # ≡ f(x, y) — appel de méthode uniforme
x |> f() |> g()  # pipe — même désucrage
t.1              # accès positionnel d'un tuple (index 1-based)
mod$member       # accès champ de record / module — "::" est un alias historique de "$"
```

```typr
# TypR
data |> filter(x > 0) |> mean()
```
```r
# R
data |> filter(x > 0) |> mean()
# ou via magrittr : data %>% filter(...) %>% mean()
```

### Cast validant

```typr
x as! Point                 # appelle validate_Point(x) à l'exécution
xs as! [Any, int]           # cast vers un type structurel inline (pas un alias)
```

### Intervalles

```typr
1:10        # ≡ seq(1, 10, 1)
1:2:10      # ≡ seq(1, 10, 2) — pas explicite au milieu
```

Les variantes doublées (`++ -- ** // %% @@ .. $$ |>>`) et `@`/`@@`/`=` en infixe ont été retirées
du tokenizer (`components/language/operators.rs::op()`/`pipe_op()`) — aucune n'avait de bras de
typage/transpilation ni de signature stdlib `` `op` `` pour les porter, contrairement à
`+ - * / % && || ...` qui retombent réellement sur un appel de fonction stdlib. Les laisser
tokeniser silencieusement un typo (ex. un commentaire `//` de style C, TypR n'utilisant que `#`)
en un opérateur "valide" ne faisait que déplacer l'erreur vers un message confus, loin du vrai
problème ; voir §14. `@` en tête de ligne appartient de toute façon aux annotations (`@pub`,
`@extern`...), pas aux opérateurs.

---

## 9 · Contrôle de flux

```typr
if (x > 0) { "positif" } else if (x < 0) { "négatif" } else { "zéro" }

for (item in items) { print(item); };
while (n > 0) { n <- n - 1; };
loop { /* ... */ break; };

next;      # continue de boucle
break;
return valeur;
```

Toute forme itérable dotée d'une fonction `as_vec: (Self) -> [T]` peut être utilisée dans un
`for (... in ...)`, pas seulement les tableaux.

---

## 10 · Modules & imports

```typr
module Math {
    let pi <- 3.14159;
    @pub let pi_approx <- 3.14;
    @pub opaque Radians <- num;
};

use Math::pi_approx;
use Math::{pi_approx, sin as s};
use Math::*;

import Math;
import Math as M;

mod Utils;                 # forme d'import historique équivalente
library(dplyr);             # dépendance R classique
use("dplyr", c("filter", "select"));   # adaptateur legacy
```

Un module compile vers un environnement R ; les membres non `@pub` restent invisibles depuis
l'extérieur (sauf en build `--test` via `@testable`, exposés en `M$.test_nom`).

---

## 11 · Interfaces & validation structurelle

```typr
type Movable <- interface { mv: (Self, int, int) -> Self };

let p <- Point:{ x = 1, y = 2 };
let q <- Movable(p);   # validateur compile-time — transpile en `q <- p`, jamais un appel réel
```

Une interface décrit une **capacité structurelle** : n'importe quel type dont les fonctions
libres ont ce type comme premier paramètre « l'implémente », sans mot-clé `impl`. Appeler `I(x)`
où `I` est un alias d'interface n'est donc jamais un vrai appel de fonction (les alias vivent dans
un espace de noms séparé des variables) — c'est un contrôle de compatibilité à la compilation qui
échoue avec `InterfaceNotSatisfied` / `IncompatibleInterfaceMethod`.

---

## 12 · Échappatoires vers du code brut

```typr
extern (x: int, y: char) -> char r#"paste0(x, y)"#;   # corps R brut, typé en entrée/sortie

function(x, y) { x + y }     # fonction R brute non typée (RFunction), corps capturé tel quel

JS { /* ... */ }               # bloc JavaScript brut (cible JS)

Class("data.frame", "tbl")    # type dénotant une classe R existante (RClass)

@{ 1 + x * 2 }@                # bloc vectoriel : capture littérale, non re-parsée élément par élément
```

Ce sont les points de sortie délibérés du système de types : `extern` garde une signature
vérifiée par TypR autour d'un corps R opaque ; `function(...)` / `JS { ... }` n'ont aucune
vérification du tout.

---

## 13 · TypR vs R — ce qui change vraiment

| Aspect | R classique | TypR |
|---|---|---|
| Fin d'instruction | saut de ligne suffit | `;` attendu (sinon avertissement de parsing) |
| Typage | dynamique, aucune annotation | statique ; `fn(...)` exige toujours un type de retour |
| Casse des noms | convention libre | vérifiée par le parseur : `snake_case` pour `let`, `PascalCase` pour `type`/alias |
| Listes | `list(a = 1, b = 2)`, sans validation | `list{a=1,b=2}` génère un vrai constructeur + validateur (`as.T`, `validate_T`) |
| Sommes de types | pas de type natif — conventions `class` ad hoc | tags `.A(T) \| .B` + `match` exhaustif |
| Appel de méthode | `UseMethod`/S3, ou `$` sur les objets R6/environnement | UFCS générale : `x.f(y)` ≡ `f(x, y)` pour *tout* type |
| Interfaces | aucune notion formelle | `interface { ... }` + validateur structurel `I(x)` à la compilation |
| Pattern matching | `switch()`, peu typé | `match` avec motifs tag/type/record/tuple/joker |
| Modules | packages / `local()` / environnements ad hoc | `module M { ... }` → environnement R, visibilité `@pub` explicite |
| Génériques | inexistants (dispatch S3 dynamique) | paramètres de type `T`, sigils de kind (`%R @I ^S ?B #N`) |
| Mutation | `x <- f(x)` explicite | sucre `expr!;` équivalent, plus `State<T>` pour la mutation partagée réelle |
| Sortie | du R directement exécuté | compile vers du R idiomatique (`R/*.R`) — 100% du R généré reste lisible et exécutable tel quel |

> **À retenir** : TypR n'est pas un nouveau runtime — c'est une couche de vérification statique et
> de sucre syntaxique (UFCS, tags, match, interfaces, modules, générique) qui se désucre
> entièrement en R conventionnel avant exécution. Rien de tout ceci n'existe à l'exécution R ;
> tout est résolu par le compilateur TypR.

---

## 14 · Ambiguïtés & pièges connus du parseur

Le parseur est fait de dizaines de petits combinators `nom` assemblés via `alt()`, sans grammaire
déclarative unique — la plupart des pièges viennent d'un ordre d'essai particulier, ou d'un
tokenizer générique réutilisé dans deux contextes différents. Tout ce qui suit est vérifié
directement dans le code source, pas déduit.

### Instruction sans `;` avalant la suivante — *corrigé*

`elements()` assemblait autrefois ses tokens via
`many1(alt((as_excl_operator_token, single_element_token, element_operator_token)))`, sans
imposer l'alternance `Expression (Opérateur Expression)*`. Deux `single_element_token`
consécutifs — donc deux instructions collées faute de `;` — étaient acceptés dans le même vecteur
de tokens sans erreur. `variable_exp` n'excluant aucun mot-clé, la fonction/l'identifiant de
l'instruction suivante finissait comme jeton « expression » surnuméraire, silencieusement absorbé
dans le corps de la première instruction. La seconde instruction disparaissait purement et
simplement de l'AST, sans le moindre message d'erreur.

```typr
let add_object <- fn(self: Scene, object: Object): Scene {
    Scene:{ objects: self.objects.extend(object), ...self }
}                                    // <- ';' manquant ici

@pub
let print <- fn(self: Scene): Empty { /* ... */ };   // avalé, invisible dans l'AST
```

Fixé (`cases/0012`) en remplaçant le tokenizer par
`single_element_token, many0(pair(operator_like_token, single_element_token))` : un jeton
non-opérateur après une expression complète arrête désormais `many0` *sans consommer d'entrée*,
laissant l'instruction suivante au parseur appelant plutôt que de l'avaler. L'oubli d'un `;`
redevient une simple `ForgottenSemicolon` récupérable, pas une perte silencieuse de code.

### `@` en tête de ligne juste après une instruction non terminée — *corrigé*

Conséquence directe du piège précédent : le tokenizer générique d'opérateurs reconnaissait aussi
`@`/`@@` comme `Op::At`/`Op::At2` — un opérateur binaire qui n'a jamais eu le moindre bras de
typage ou de transpilation (syntaxe totalement morte à l'exécution). Une instruction sans `;`
suivie d'une déclaration `@pub`/`@export`/`@testable` se retrouvait donc avec son `@` avalé comme
« continuation d'opérateur » du corps précédent, corrompant la première instruction *et*
dépouillant la seconde de son annotation de visibilité.

Fixé en retirant `@`/`@@` de la liste d'opérateurs infixes génériques
(`components/language/operators.rs::op()`). `@` reste pleinement actif, mais uniquement comme
*préfixe* d'annotation (`@pub`, `@export`, `@testable`, `@extern`, `@nom: Type;`), reconnu par des
parseurs dédiés essayés avant la grammaire d'expression générale.

### Opérateurs morts du tokenizer — *purgés*

Suite directe du point précédent : `@`/`@@` n'étaient pas les seuls jetons reconnus par `op()`/
`pipe_op()` sans avoir le moindre bras de typage/transpilation ni de signature stdlib `` `op` ``
pour les porter. `++ -- ** // %% .. $$ |>>` ainsi qu'un `=` infixe (`Op::Eq2`, déjà exclu de
`bool_op()`) étaient dans le même cas — tombant sur le bras générique de `Lang::Operator` qui
désucre tout opérateur non géré explicitement en appel de `` `op`(e1, e2) ``, sans jamais trouver
de définition correspondante. Le risque concret : un typo courant (`//` pour un commentaire
C-style, alors que TypR n'utilise que `#`) parsait *avec succès* comme une division, et n'échouait
qu'au typage, avec un message éloigné du vrai problème (« Function `` `//` `` not defined »).

Purgé en retirant ces jetons de `op()`/`pipe_op()` et les variantes `Op` correspondantes
(`Add2/Minus2/Mul2/Div2/Modulo2/Dot2/Dollar2/Pipe2/At/At2/Eq2`) entièrement de l'énum — elles
n'étaient produites nulle part ailleurs dans la base de code une fois le tokenizer purgé.

Un `//` égaré échoue désormais dès le parsing au lieu de produire un opérateur fantôme — mais pas
comme une erreur brute : un parseur dédié `wrong_comment` (`parsing/mod.rs`, à côté de `comment()`)
le reconnaît explicitement, le traite exactement comme un vrai commentaire `#` (consommé jusqu'à
fin de ligne, ignoré, non fatal) et pousse un `SyntaxError::WrongCommentSyntax` récupérable. Sans
ce traitement dédié, `//` serait retombé sur le piège générique décrit juste en dessous (texte de
fin d'instruction imparsable = **silencieusement tronqué de l'AST**, pas juste mal signalé) — un
typo `//` est un cas assez courant (habitude venue d'autres langages) pour mériter mieux qu'un
`SyntaxError::UnknownElement` générique.

> ⚠️ **Tentative avortée pour `=` en position de comparaison** (`if (a = b)`) : un plan initial
> prévoyait la même récupération dédiée qu'au-dessus pour `//`, en repêchant un `=` isolé dans
> `op()` lui-même (retomber sur `Op::Eq`, avec un `SyntaxError::SingleEqualsComparison` récupérable).
> Abandonné : `op()` est aussi appelé par la grammaire de *type* (`types.rs::index_operator`/
> `compute_operators`, pour l'arithmétique de types `type Combined <- A + B;`), qui se trouve juste
> avant le séparateur `= valeur` d'un paramètre par défaut (`greeting: char = "Hello"`). Là, `=`
> n'a aucun sens d'opérateur — le repêcher en `Op::Eq` faisait paniquer `compute_operators` sur une
> combinaison non gérée (`compute_operators`'s `_ => panic!()`, testé via
> `test_non_trailing_default_param_is_an_error` et 3 autres tests `function::tests` cassés).
> `=` reste donc simplement absent du tokenizer, sans erreur dédiée — un futur correctif devrait
> cibler spécifiquement le tokenizer d'expressions (`elements.rs`), pas la primitive `op()` partagée.

### Découverte connexe — le CLI avalait toutes les erreurs de syntaxe (`typr check`/`build`) — *corrigé*

En vérifiant le point ci-dessus, découvert que `typr check`/`typr build` (contrairement à
`typr debug --ast`) n'affichaient **aucune** erreur de syntaxe collectée au parsing —
`engine.rs::parse_code`/`parse_code_with_info`/`parse_code_from_str` appelaient
`parse_with_errors()` puis jetaient silencieusement `.errors`, ne gardant que `.ast`. Même chose
une deuxième fois, indépendamment, dans `metaprogramming.rs::ModuleExpander::expand_one` pour
*chaque fichier de module importé* (`mod x;`) : son propre `parse(...)` local jetait aussi ses
erreurs. Concrètement, tout contenu de fin de fichier illisible par la grammaire
(`SyntaxError::UnknownElement`, ou tout autre `push_parse_error` récupérable comme
`WrongCommentSyntax` ci-dessus) était **invisible** via `check_file`/`check_project` dans
`typr-cli/src/project.rs`, alors que `typr debug --ast` les affichait très bien. Reproductible avec
n'importe quel charabia de fin d'instruction, dans le fichier d'entrée ou dans n'importe quel module
importé.

Corrigé en changeant les signatures de `parse_code`/`parse_code_with_info`/`parse_code_from_str`
(`typr-cli/src/engine.rs`) pour renvoyer aussi `Vec<SyntaxError>` (entry file + tous les modules
importés, via un nouveau champ `ExpansionInfo::syntax_errors` alimenté par `expand_one`), et en
câblant les 7 points d'appel dans `project.rs` (`check_project`, `check_file`, `build_project_impl`,
`build_file`, `run_file_impl`, `generate_spg`, `document_impl`) sur un nouveau helper
`report_syntax_errors`. Politique de sévérité (décidée explicitement, pas par défaut) : seul
`SyntaxError::UnknownElement` (code réellement perdu de l'AST) fait échouer le pipeline
(`step.fail()` + `exit(1)`) — tout le reste (`ForgottenSemicolon`, `WrongCommentSyntax`,
`LetInsteadOfType`, ...) est toujours entièrement récupéré par le parseur et n'est affiché que
comme avertissement, pour ne pas casser tout projet existant dont le dernier `let` n'a pas de `;`
final (un style aujourd'hui parfaitement toléré).

En chemin, corrigé un bug d'étiquetage préexistant révélé par ce câblage : le template partagé
`MsgTemplate::Single`/`Double` (`message_template.rs`) avait `"Type error: {text}"` codé en dur —
`SyntaxError::display()` n'avait simplement jamais été appelé en dehors des tests avant cette
correction, donc personne n'avait remarqué qu'une vraie erreur de syntaxe s'affichait comme
« Type error: TypR comments use `#`, not `//` ». `SingleBuilder`/`DoubleBuilder` ont gagné un champ
`kind` (défaut `"Type error"`, préservant les 24 sites d'appel de `type_error.rs` sans changement) ;
les 13 sites de `syntax_error.rs` appellent désormais `.kind("Syntax error")`.

### Un seul `=` n'est jamais un opérateur binaire

`bool_op()` exclut délibérément `tag("=")` de ses alternatives, et `op()` n'a plus aucun repli sur
un `=` isolé (voir la tentative avortée documentée juste au-dessus — `compute_operators` paniquerait
dessus, ce qui est exactement ce qui a été observé en essayant). `=` n'existe que dans des positions
de grammaire dédiées : champs nommés (`x = 1`), affectation top-level (`assign()`), valeurs par
défaut de paramètre. Un test d'égalité s'écrit toujours `==`.

```typr
if (a = b) { /* ... */ }    // ne teste PAS l'égalité — utiliser a == b
```

### `record` vs `tuple` : la forme des champs décide, pas le mot-clé

`list{...}`/`:{...}` peuvent produire un `Lang::List` (record) *ou* un `Lang::Tuple` selon la
forme des éléments — `record` est toujours essayé en premier dans `single_element`, mais son
parseur de champ (`argument_val`) exige la forme `nom = valeur` ; s'il échoue sur un premier
élément positionnel, tout le bloc retombe sur `tuple_exp`.

```typr
list{ a = 1, b = 2 }   // → Lang::List  (record, champs nommés)
list{ 1, 2, 3 }        // → Lang::Tuple (même mot-clé "list", positionnel !)
:{ x = 1, y = 2 }       // → Lang::List
:{ 1, 2, 3 }            // → Lang::Tuple
```

### La « règle d'or » des blocs `{...}` génériques

Un bloc `{ ... }` qui suit un nom de type paramétré en fait toujours un *record constructeur*
(`Tibble[3]{ id: int }`), jamais un simple usage d'alias. Deux violations de cette règle sont
détectées explicitement plutôt que de mal parser silencieusement :

```typr
Df[8, int]{ name: char }        // SyntaxError::RecordConstructorIndex — un seul index autorisé
Array[5, { a: int }]             // SyntaxError::RecordInRecursiveParams — bloc record interdit ici
```

Un cas plus subtil couvert par un test de régression dédié : `ltype` ne doit pas confondre le
type de retour d'une fonction avec le corps qui suit — `fn(i: int): Feux { .Rouge }` doit
s'arrêter à `Feux` et laisser `{ .Rouge }` au corps de la fonction, pas l'avaler comme si
`Feux{...}` était un record constructeur.

### Alias de type PascalCase à une seule lettre : imparsable — *ouvert*

`pascal_case_no_space` (utilisé par `type_alias`, donc par `type`/`opaque`/`typeconstructor`) est
défini comme `(one_of("A".."Z"), alphanumeric1)` — `alphanumeric1` exige **au moins un** caractère
alphanumérique après la majuscule initiale. Un nom de type d'une seule lettre échoue donc
totalement à ce niveau et retombe ailleurs dans la grammaire (souvent une variable bidon).

```typr
type A <- int;     // n'est PAS un Alias — utiliser au moins 2 caractères
type Ab <- int;    // OK
```

Asymétrie notable : les noms de *tag*/variante d'union utilisent `pascal_case_helper`
(`opt(alpha1)` — le suffixe est optionnel), donc `.X` (tag à une lettre) parse très bien. Seuls
les noms d'alias de type exigent 2 caractères ou plus.

### `fn(...)` interdit en position de type

`fn(args) -> Type` ressemble à un type fonction mais n'est légal qu'au niveau expression ; en
position de type, seule la forme `(Type, Type) -> Type` (sans `fn`) est valide. Le parseur capture
spécifiquement le cas d'erreur (`fn_function_type_error`) pour donner un message ciblé plutôt
qu'un échec générique.

```typr
let f: fn(a: int) -> int <- ...;   // SyntaxError::FunctionTypeSyntax
let f: (int) -> int <- ...;         // OK
```

### Commentaires `.ty` : uniquement `#`

`comment()` ne reconnaît que `tag("#")` — il n'existe aucune alternative `//` nulle part dans
`base_parse`. Un fichier de signatures stdlib écrit avec des commentaires `//` n'est pas juste mal
commenté : il échoue à parser et `build_typed_vartype` l'ignore silencieusement en entier
(« Skipped … (syntax not supported) ») — surveiller cette ligne dans la sortie de `typr std`.

### `use` : un seul mot-clé, quatre grammaires distinctes

`use` introduit quatre constructions entièrement différentes, désambiguïsées seulement par ce qui
suit immédiatement, dans cet ordre d'essai :

| Forme | Reconnue si | Produit |
|---|---|---|
| `use("dplyr", c(...));` | `use(` littéral, sans espace | adaptateur R legacy (`Lang::Use`) |
| `use Math::pi;` | un `::` suit le premier identifiant | `Lang::UseModule` |
| `use MonAlias;` | identifiant `PascalCase`, pas de `::` | `Lang::Import` (type) |
| `use ma_variable;` | identifiant `snake_case`, pas de `::` | `Let`/`Alias` (variable) |

### Détails plus fins

- `in` comme opérateur de type-condition est tokenisé `tag("in ")` — l'espace final fait partie
  du motif, donc `in` immédiatement suivi d'une parenthèse sans espace ne matchera pas.
- `embed` (embedding de type nommé sur un champ de record) est un mot-clé « doux » reconnu
  seulement via `tag("embed") + multispace1` — un champ réellement nommé `embed` (suivi
  directement de `:`, sans espace) reste donc parsable comme nom de champ ordinaire.
- Le spread statique `..source` (nominal) exige explicitement `not(char('.'))` après les deux
  points pour ne jamais être confondu avec `...source` (spread runtime) ou `...` (paramètre
  variadique) — les trois partagent un préfixe visuel.
- `nom::alt()` plafonne à 21 alternatives par tuple : `single_element` et `single_type` sont
  chacun scindés en deux `alt()` imbriqués pour rester sous la limite. Ajouter un nouveau
  combinator à l'un de ces points d'entrée demande de respecter ce découpage, pas d'ajouter une
  22ᵉ branche à plat.
