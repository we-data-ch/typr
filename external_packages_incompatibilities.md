# Incompatibilités TypR ↔ packages R externes

## Contexte technique : ce que TypR génère au runtime

Pour donner sens aux incompatibilités, les types TypR au runtime R :

| Type TypR | Représentation R |
|---|---|
| `int` | `structure(x, class = c("Integer", "integer", "Any", "Generic"))` |
| `num` | `structure(x, class = c("Number", "numeric", "Any", "Generic"))` |
| `char` | `structure(x, class = c("Character", "character", "Any", "Generic"))` |
| `bool` | `structure(x, class = c("Boolean", "logical", "Any", "Generic"))` |
| `[N, T]` | `structure(list(data=...), class="typed_vec", typed_dim=N)` |
| `list{x:int}` (record alias) | `structure(list(x=1), class=c("TypeName", "list"))` |
| `.Some(x)` | `structure(list("Some", body=x), class=c("Some","Option","Tag","list"))` |
| `State<T>` | environnement R avec `class="State"` |

---

## Catégorie 1 — Dispatch S3 sur classe inconnue *(fatal, erreur immédiate)*

**Mécanisme** : Les packages qui utilisent S3 dispatch (comme `jsonlite`, `reactable`, `gt`, etc.)
tentent de dispatcher sur la classe TypR (`Integer`, `typed_vec`, `Point`, `Tag`...) et échouent
avec "no applicable method" quand aucune méthode pour cette classe n'existe.

**Exemples concrets** :
- `jsonlite::toJSON(Integer(5L))` → cherche `toJSON.Integer` → absent → erreur
- `jsonlite::toJSON(typed_vec(1,2,3))` → cherche `toJSON.typed_vec` → absent → erreur
- `jsonlite::toJSON(Point(x=1, y=2))` → cherche `toJSON.Point` → absent → erreur
- `knitr::knit_print(Integer(5L))` → même pattern

**Portée** : tout package qui dispatch sur la classe du premier argument sans méthode `.default`
robuste.

**Atténuation possible** : définir des méthodes `toJSON.Integer`, `toJSON.typed_vec`, etc. dans
`std.R`.

---

## Catégorie 2 — Vérification stricte de classe *(fatal ou silencieux selon la fonction)*

**Mécanisme** : Les packages qui vérifient `class(x) == "integer"` (comparaison stricte) au lieu
de `inherits(x, "integer")` ou `is.integer(x)`.

**Exemples** :
- `class(Integer(5L)) == "integer"` → `FALSE` (class retourne `c("Integer", "integer", "Any", "Generic")`)
- Beaucoup de packages utilisent `checkmate::assert_integer(x)` qui vérifie via `is.integer()`
  (fonctionne) mais aussi via `!inherits(x, "integer")` selon la version
- `data.table` fait des vérifications internes sur le type de stockage SEXP — ça passe généralement,
  mais les attributs S3 peuvent surprendre

**Cas où ça passe** : `is.integer(Integer(5L))` → `TRUE` (vérifie le type de stockage SEXP, pas la
classe S3). Les packages qui utilisent `is.*()` correctement s'en sortent.

---

## Catégorie 3 — `typed_vec` n'est pas un vecteur R *(fatal en général)*

**Mécanisme** : Les vecteurs TypR (`[N, T]`) sont des **listes S3** (`typed_vec`), pas des vecteurs
atomiques R. Tout le code R qui suppose qu'un "vecteur" est atomique (longueur via `length()`,
indexation via `[`, `vapply`, `colMeans`, etc.) casse.

**Exemples** :
- `dplyr::mutate(df, col = my_typed_vec)` → la colonne attend un vecteur atomique
- `ggplot2::aes(x = typed_vec_col)` → détection du type d'échelle brisée
- `mean(typed_vec_of_numbers)` → `mean.default` sur une liste → résultat incorrect ou NA
- `length(typed_vec(1,2,3))` → retourne `3` (TypR surcharge `length.typed_vec`) mais certains
  packages appellent `.Internal(length(x))` qui court-circuite le dispatch S3

**Note** : TypR surcharge `[[.typed_vec`, `length.typed_vec` et `apply.typed_vec` pour les usages
internes, mais les packages externes ignorent ces surcharges s'ils n'utilisent pas le dispatch S3.

---

## Catégorie 4 — Perte d'attributs S3 après opération R *(silencieux, bug de type)*

**Mécanisme** : La plupart des opérations R (arithmétique, logique, `c()`, `rbind()`, etc.)
**strippent les attributs** S3 de leurs résultats. Un `Integer` qui passe dans une fonction externe
et revient n'est plus un `Integer`.

**Exemples** :
```r
x <- Integer(5L)
y <- x + 1          # y est plain `integer`, plus `Integer`
dplyr::mutate(df, x = int_col * 2)  # résultat : colonne integer sans classe TypR
```

**Impact** : Les résultats de fonctions externes ne peuvent pas être réinjectés dans du code TypR
typé sans re-wrapping explicite.

---

## Catégorie 5 — Systèmes OOP alternatifs : S4, R6, S7, R5 *(fatal ou silencieux)*

TypR est exclusivement S3. L'interopérabilité avec les autres systèmes OOP de R est très limitée.

### S4 (Bioconductor, Matrix, etc.)

- `is(x, "SomethingS4")` → `FALSE` pour tous les objets TypR
- Passer un record TypR dans un `validObject()` S4 → erreur de validation
- `setMethod("myFunc", "Integer", ...)` : peut se définir mais TypR ne le sait pas
- Les classes S4 avec slots ne sont pas accessibles via `$` comme les records TypR

### R6 (httr2, curl, cli, etc.)

- Les objets R6 retournés par des packages sont des environnements avec méthodes actives
- `obj$method()` peut fonctionner syntaxiquement depuis TypR (TypR traduit `.` en accès de
  liste/env), mais le type-checker n'a aucune information sur la signature
- Passer un objet R6 à une fonction TypR typée → le type-checker voit `Any` ou échoue

### S7 (nouveau système unifié, emerging)

- `S7::method(generic, class)` : les classes TypR ne sont pas enregistrées comme S7 classes
- Les packages qui migrent vers S7 (ggplot2 futur, cli, etc.) pourraient casser l'interop

### R5 / Reference Classes

- Cas marginal, même problème que R6.

---

## Catégorie 6 — Shadowing de fonctions R base *(silencieux, comportement inattendu)*

TypR définit des génériques S3 qui **écrasent** des fonctions R de base dans le namespace global :

`get`, `apply`, `reduce`, `max`, `min`, `replace`, `append`, `plot`, `expect_equal`, `set`,
`update`, `version`, `state`, `map`, `derive`, `unwrap`, `factor`, `nlevels`, `validate`

**Exemples** :
- Du code externe qui appelle `get("variable_name")` → tombe sur `get.default` de TypR → probablement
  `no applicable method`
- Un package de test qui appelle `expect(condition, msg)` → TypR's `expect` S3 generic → crash
- `purrr::map()` puis `map(x, f)` dans le même scope → ambiguïté avec le `map` TypR
- `base::append(list1, list2)` → TypR's `append` generic → dispatch sur la classe de `list1`

**Cas critique** : `factor()` — TypR redéfinit `factor` comme S3 generic. Tout code qui appelle
`factor(x, levels=...)` attend le comportement base R mais tombe sur le dispatcher TypR.

---

## Catégorie 7 — NULL et Option\<T\> *(silencieux, erreur sémantique)*

R utilise `NULL` comme valeur d'absence canonique. TypR utilise `.None` (un Tag S3). Ces deux
représentations sont mutuellement incompréhensibles.

**Exemples** :
```r
# Fonction externe retourne NULL
result <- external_pkg::find_something(x)
# → result est NULL, pas .None
# → du code TypR attendant Option<T> voit NULL, pas reconnu comme Option

# Passer .None à une fonction externe qui attend NULL
external_pkg::process(is_none_value)
# reçoit une list avec classe c("None","Option","Tag","list"), pas NULL
```

**Impact** : nécessite une couche d'adaptation explicite (conversion `NULL` ↔ `Option<T>`) à chaque
frontière avec un package externe.

---

## Catégorie 8 — Non-Standard Evaluation (NSE) *(fatal)*

Beaucoup de packages du tidyverse (`dplyr`, `rlang`, `tidyeval`, `data.table`) utilisent le NSE
pour capturer des expressions R non évaluées.

**Exemples** :
```r
dplyr::filter(df, col > 0)
# dplyr capture l'expression `col > 0` comme quosure
# TypR transpile col > 0 en `col %>=%.default 0` ou une forme interne similaire
# → la quosure capturée est une expression TypR opaque pour dplyr
```

Le code TypR transpilé utilise des helpers internes (`typed_vec`, `vec_apply`, `%==%.default`...)
dans ses expressions. Ces expressions, capturées par NSE, sont opaques pour les packages qui les
évaluent dans leur propre environnement (sans `std.R` chargé).

---

## Catégorie 9 — Sérialisation/Persistance *(fatal)*

Toutes les formes de sérialisation qui inspectent la structure interne :

| Format | Problème |
|---|---|
| `jsonlite::toJSON` | Dispatch S3 inconnu → erreur (cas original) |
| `yaml::as.yaml` | Idem |
| `saveRDS`/`readRDS` | Préserve les classes, mais session sans TypR → objets brisés |
| `arrow`/`parquet` | Types Arrow ne mappent pas aux classes TypR |
| `qs`/`qs2` | Idem RDS |
| CSV/`readr` | Perte totale de type au round-trip |

---

## Catégorie 10 — Data frames et données tabulaires *(silencieux)*

- `data.frame(x = Integer(1:5))` → R essaie de mettre une colonne S3 en data.frame → résultat
  imprévisible
- `tibble(x = typed_vec(...))` → échoue car `typed_vec` est une liste, pas un vecteur atomique
- `dplyr::bind_rows(list_of_typr_records)` → records sont des listes S3, le bind peut fonctionner
  partiellement mais les classes sont perdues ou mélangées

---

## Résumé par sévérité

| Catégorie | Sévérité | Détectabilité |
|---|---|---|
| 1 — S3 dispatch inconnu | **Fatal** (erreur) | Immédiate |
| 3 — `typed_vec` ≠ vecteur | **Fatal** (erreur) | Immédiate |
| 8 — NSE | **Fatal** (erreur) | Immédiate |
| 9 — Sérialisation | **Fatal** (erreur) | Immédiate |
| 2 — Vérif. de classe stricte | Fatal/silencieux | Difficile |
| 5 — OOP S4/R6/S7 | Fatal/silencieux | Selon usage |
| 4 — Perte d'attributs | **Silencieux** (bug) | Difficile |
| 6 — Shadowing base R | Silencieux | Difficile |
| 7 — NULL vs Option | Silencieux | Difficile |
| 10 — Data frames | Silencieux | Difficile |

---

## Ce qui fonctionne sans adaptation

Pour équilibrer le tableau : les fonctions qui utilisent les prédicats `is.*()` standard (vérifient
le stockage SEXP, pas la classe S3) fonctionnent en général — `is.integer(Integer(5L))` → `TRUE`.
Les packages purement fonctionnels qui retournent des valeurs simples (strings, numbers, plain lists)
sans dispatch S3 sur leurs entrées fonctionnent. Les fonctions d'I/O fichier qui prennent des
`char`/`character` fonctionnent si on passe `unclass()` en amont.
