# Interopérabilité avec les packages R externes

Ce tutoriel explique comment appeler des fonctions de packages R existants (`dplyr`, `jsonlite`,
`ggplot2`, etc.) depuis du code TypR, en gérant proprement la frontière entre les types TypR et
le R brut.

---

## Le problème fondamental

TypR enrichit chaque valeur de classes S3 supplémentaires (`Integer`, `typed_vec`, `.Some`,
`.None`, etc.) et génère ses propres génériques S3 (`map`, `get`, `factor`, `append`…). Quand
vous passez une valeur TypR à une fonction externe, deux problèmes surgissent :

1. **Shadowing de noms** : `filter(df, cond)` appelle le générique S3 TypR `filter`, pas
   `dplyr::filter`.
2. **Classes inattendues** : `jsonlite::toJSON(x)` échoue si `x` porte la classe `Integer` que
   jsonlite ne connaît pas.

Les cinq niveaux ci-dessous couvrent ces cas du plus simple au plus complexe.

---

## Niveau 0 — Primitives de marshalling `to_native` / `from_native`

La stdlib expose deux fonctions pour convertir manuellement à la frontière.

| Fonction                          | Sens          | Comportement                                                  |
|-----------------------------------|---------------|---------------------------------------------------------------|
| `to_native(x)`                    | TypR → R brut | Supprime les classes TypR (`unclass`), déroule les enveloppes |
| `from_int(x)` / `from_num(x)` / … | R brut → TypR | Réapplique la classe TypR correcte                            |

```typr
# Conversion manuelle avant d'appeler une fonction externe
let raw <- to_native(my_typed_value);
```

En pratique vous n'utiliserez pas ces primitives directement — les niveaux 1 et 2 les injectent
automatiquement. Elles restent utiles pour des cas ad-hoc ou du débogage.

---

## Niveau 1 — `@extern` : déclaration typée avec marshalling automatique

`@extern` est le mécanisme principal pour déclarer une fonction d'un package R externe avec un
type TypR. Le transpileur injecte `to_native()` sur chaque argument et `from_xxx()` sur le
retour.

### Syntaxe

```typr
@extern [pkg::]name: (T1, T2) -> Ret;
```

- `pkg::name` : nom qualifié côté R (recommandé pour éviter le shadowing)
- `name` seul : le nom TypR et le nom R sont identiques

### Exemple — jsonlite

```typr
@extern jsonlite::toJSON: (Any) -> char;
@extern jsonlite::fromJSON: (char) -> Any;

let json_str <- toJSON(my_record);   # transpile en : jsonlite::toJSON(to_native(my_record))
let parsed   <- fromJSON(json_str);  # transpile en : fromJSON(to_native(json_str))
```

### Exemple — stringr

```typr
@extern stringr::str_detect: (char, char) -> bool;
@extern stringr::str_replace: (char, char, char) -> char;

let found   <- str_detect(text, "^foo");
let cleaned <- str_replace(text, "\\s+", " ");
```

### Règles de marshalling automatique

| Type de retour  | Wrapping émis                |
|-----------------|------------------------------|
| `int`           | `from_int(pkg::f(...))`      |
| `num`           | `from_num(pkg::f(...))`      |
| `char`          | `from_char(pkg::f(...))`     |
| `bool`          | `from_bool(pkg::f(...))`     |
| `Option<T>`     | `from_nullable(pkg::f(...))` |
| Tout autre type | aucun wrapping (retour brut) |

---

## Niveau 2 — `from_nullable` / `to_nullable` et `opaque Foreign<T>`

### Valeurs nullable avec `Option<T>`

Certaines fonctions R retournent `NULL` pour signifier l'absence de valeur. TypR modélise cela
avec `Option<T>`. Le transpileur génère `from_nullable()` automatiquement quand le type de
retour d'un `@extern` est `Option<T>`.

```typr
@extern base::Sys__getenv: (char) -> Option<char>;

let home <- Sys.getenv("HOME");   # retourne .Some("...") ou .None

match home {
    .Some(path) => path,
    .None       => "/tmp",
}
```

Pour aller dans l'autre sens (passer un `Option<T>` à une fonction R qui attend `NULL` ou une
valeur), utilisez `to_nullable` :

```typr
@extern DBI::dbConnect: (Any, Any) -> Any;

let maybe_dsn: Option<char> <- .Some("my_db");
let conn <- dbConnect(driver, to_nullable(maybe_dsn));
```

### Valeurs R opaques avec `Foreign<T>`

Quand vous récupérez un objet R dont TypR ne connaît pas la structure interne (connexion de base
de données, objet R6, data.frame brut…), utilisez `opaque Foreign<T>` :

```typr
# Dans un fichier .ty de votre projet
type DataFrame  <- Foreign<Any>;
type Connection <- Foreign<Any>;
```

Ces types sont purement fictifs côté R (ils ne génèrent aucun code), mais permettent à
TypR de distinguer statiquement un `DataFrame` d'une `Connection` dans les signatures.

```typr
@extern DBI::dbConnect:    (Any, char) -> Connection;
@extern DBI::dbReadTable:  (Connection, char) -> DataFrame;
@extern DBI::dbDisconnect: (Connection) -> bool;

let conn  <- dbConnect(RSQLite__SQLite(), "my.db");
let users <- dbReadTable(conn, "users");
dbDisconnect(conn);
```

---

## Niveau 3 — Bloc R brut typé `extern: (...) -> Ret r#"...R..."#`

Pour du code R qui ne peut pas s'exprimer avec des appels de fonctions simples (initialisation
complexe, pipe natif R, syntaxe tidyverse inline…), utilisez le bloc `extern` qui embarque du R
verbatim dans une fonction TypR.

### Syntaxe

```typr
let my_fn <- extern: (param1: T1, param2: T2) -> Ret r#"
  # code R arbitraire ici
  # la valeur de la dernière expression est retournée
"#;
```

> Le type de retour doit être un type simple ou un alias — pas de `A | B` inline (définissez un
> alias à part).

### Exemple — pipeline dplyr complexe

```typr
type DataFrame <- Foreign<Any>;

let filter_and_count <- extern: (df: DataFrame, col: char, threshold: num) -> DataFrame r#"
  df |>
    dplyr::filter(.data[[col]] > threshold) |>
    dplyr::mutate(count = dplyr::n())
"#;
```

### Exemple — initialisation ggplot2

```typr
type Plot <- Foreign<Any>;

let make_histogram <- extern: (df: DataFrame, var: char) -> Plot r#"
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::theme_minimal()
"#;
```

---

## Niveau 4 — Appels qualifiés `pkg::fun` via `@importFrom`

`@importFrom` résout le problème de shadowing de noms : les fonctions déclarées ainsi sont
appelées en `pkg::fun(...)` dans le R généré, bypassa les génériques S3 TypR du même nom.

### Syntaxe

```typr
@importFrom pkg fun1 fun2 ...;
```

Puis déclarez les signatures avec `@` comme d'habitude :

```typr
@fun1: (T) -> R;
```

### Exemple — dplyr

Sans `@importFrom`, `filter(df, cond)` appelle le générique TypR `filter`, pas `dplyr::filter`.

```typr
@importFrom dplyr filter mutate select arrange summarise group_by;

@filter:    (Any, Any) -> Any;
@mutate:    (Any, Any) -> Any;
@select:    (Any, Any) -> Any;
@arrange:   (Any, Any) -> Any;
@summarise: (Any) -> Any;
@group_by:  (Any, Any) -> Any;

let result <- df
  |> filter(age > 18)
  |> group_by(country)
  |> summarise(mean_age);
```

R généré :
```r
result <- df |>
  dplyr::filter(age > 18) |>
  dplyr::group_by(country) |>
  dplyr::summarise(mean_age)
```

### Différence avec `@extern`

|                  | `@extern pkg::fun`                                          | `@importFrom pkg fun`                                              |
|------------------|-------------------------------------------------------------|--------------------------------------------------------------------|
| Appel généré     | `pkg::fun(to_native(x), ...)`                               | `pkg::fun(x, ...)`                                                 |
| Marshalling args | `to_native()` automatique                                   | aucun                                                              |
| Wrapping retour  | `from_xxx()` si type connu                                  | aucun                                                              |
| Usage typique    | Fonctions qui reçoivent/retournent des types TypR scalaires | Fonctions qui opèrent sur des données R brutes (data.frames, etc.) |

---

## Récapitulatif des mécanismes

| Mécanisme                                  | Quand l'utiliser                                                           |
|--------------------------------------------|----------------------------------------------------------------------------|
| `to_native(x)` / `from_xxx(x)`             | Conversion manuelle ponctuelle                                             |
| `@extern pkg::fun: (T) -> R;`              | Fonction externe qui reçoit des types TypR scalaires                       |
| `Option<T>` comme type de retour `@extern` | Fonction R qui peut retourner `NULL`                                       |
| `opaque Foreign<T>`                        | Objet R opaque (data.frame, connexion, R6…)                                |
| `extern: (...) -> T r#"...R..."#`          | Code R inexprimable en appels simples                                      |
| `@importFrom pkg fun1 fun2;`               | Fonctions d'un package dont le nom entre en conflit avec un générique TypR |

---

## Exemple complet : lecture et analyse d'un CSV avec dplyr

```typr
# Types
type DataFrame <- Foreign<Any>;

# Imports
@importFrom dplyr filter mutate select summarise group_by n;
@importFrom readr read_csv;

@read_csv:  (char) -> DataFrame;
@filter:    (Any, Any) -> Any;
@mutate:    (Any, Any) -> Any;
@select:    (Any, Any) -> Any;
@group_by:  (Any, Any) -> Any;
@summarise: (Any, Any) -> Any;
@n:         () -> int;

@extern jsonlite::toJSON: (Any) -> char;

# Pipeline
let df <- read_csv("data.csv");

let summary <- df
  |> filter(year >= 2020)
  |> mutate(revenue_k = revenue / 1000)
  |> group_by(region)
  |> summarise(total = sum(revenue_k), count = n());

# Export JSON (marshalling automatique via @extern)
let json_out <- toJSON(summary);
```
