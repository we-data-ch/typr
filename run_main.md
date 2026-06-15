# 📘 RFC‑TR‑030 — *Loader de Projet TypR*

## 1. Objet de la RFC  
Cette RFC définit le **loader de projet TypR**, un composant responsable de :

- charger un projet TypR transpilé en R  
- résoudre les `#' @include`  
- initialiser les modules dans le bon ordre  
- exposer les symboles publics (`@pub`)  
- fournir un environnement d’exécution cohérent pour `typr run`, les tests, et les REPL

Le loader remplace totalement l’usage de `source()` et garantit une **encapsulation modulaire stricte**.

---

## 2. Motivations

### 2.1 Problème fondamental  
Les fichiers R générés par TypR contiennent des :

- définitions de modules  
- définitions de fonctions  
- `#' @include` pour les dépendances internes  

Mais **Rscript ne sait pas interpréter `@include`**, car c’est un mécanisme Roxygen2.

Donc :

> Un fichier R généré ne peut pas être exécuté directement.  
> Il doit être chargé dans un environnement où les includes sont résolus.

### 2.2 Objectifs du loader  
- reproduire le comportement d’un namespace R, mais **sans package**  
- charger les modules dans le bon ordre  
- exposer uniquement les symboles publics  
- permettre `typr run`  
- permettre les tests unitaires  
- permettre un REPL TypR cohérent  
- éviter toute utilisation de `source()`  

---

## 3. Spécification fonctionnelle

### 3.1 Point d’entrée  
Le loader est exposé via une fonction R :

```r
load_module <- function(project_root = ".", test = FALSE) -> environment
```

Cette fonction :

- charge le projet  
- retourne un environnement contenant les modules chargés  
- respecte la visibilité (`@pub` vs privé)  
- applique les règles de testabilité (`@testable`) si `test = TRUE`

---

### 3.2 Résolution des fichiers à charger

Le loader doit :

1. lire le fichier `typr.toml`  
2. identifier le dossier de sortie R (par défaut : `R/generated/`)  
3. lister tous les fichiers `.R` générés  
4. construire un graphe de dépendances basé sur les `#' @include`

#### Format d’un include généré :

```r
#' @include module_math.R
```

#### Règle de résolution :

- les includes sont **relatifs au dossier généré**  
- les cycles sont interdits  
- l’ordre de chargement est l’ordre topologique du graphe

---

### 3.3 Chargement des modules

Pour chaque fichier R :

1. créer un environnement propre au module  
2. évaluer le fichier dans cet environnement  
3. enregistrer le module dans un environnement global `Modules`  
4. appliquer les règles de visibilité

#### Structure interne :

```
Modules (env)
 ├─ Math (env)
 ├─ Main (env)
 └─ Utils (env)
```

Chaque module est un environnement isolé.

---

### 3.4 Visibilité (`@pub`)

Dans TypR, un symbole est public si annoté :

```typ
@pub fn f() { ... }
```

La transpilation génère :

```r
#' @export
f <- function(...) { ... }
```

Le loader doit :

- exposer uniquement les symboles marqués `@export`  
- cacher les autres dans l’environnement interne du module  

#### Règle :

> L’environnement retourné par `load_module()` contient uniquement les symboles publics.

Exemple :

```
Modules$Main$main
Modules$Main$helper   # inaccessible si non @pub
```

---

### 3.5 Mode test (`test = TRUE`)

Si `test = TRUE` :

- les symboles annotés `@testable` deviennent publics  
- les modules sont chargés dans un environnement spécial `ModulesTest`  
- les tests TypR peuvent accéder aux fonctions privées exposées

#### Règle :

> `@testable` n’a d’effet que dans le mode test.

---

## 4. API du loader

### 4.1 Fonction principale

```r
load_module <- function(project_root = ".", test = FALSE) -> environment
```

Retourne un environnement contenant :

- un sous‑environnement par module  
- uniquement les symboles publics (ou testables)  
- un attribut `__modules__` listant les modules chargés  
- un attribut `__order__` indiquant l’ordre de chargement

---

### 4.2 Exemple d’utilisation dans `typr run`

Wrapper généré :

```r
modules <- load_module(".")

modules$Main$main()
```

---

### 4.3 Exemple d’utilisation dans un test

```r
modules <- load_module(".", test = TRUE)

testthat::expect_equal(modules$Math$add(1,2), 3)
```

---

## 5. Invariants

### 5.1 Aucun `source()` n’est jamais utilisé  
Le loader repose uniquement sur :

- `sys.source()` pour charger dans un environnement dédié  
- les includes résolus statiquement  
- les environnements isolés

### 5.2 Les modules sont immuables après chargement  
Une fois chargés :

- aucun symbole ne peut être ajouté  
- aucun symbole ne peut être supprimé  
- aucun symbole ne peut être modifié

### 5.3 L’ordre de chargement est déterministe  
Basé sur :

- les includes  
- l’ordre topologique  
- les règles de dépendance

---

## 6. Structure interne du loader

### 6.1 Environnement global

```
LoaderEnv
 ├─ load_module()
 ├─ resolve_includes()
 ├─ load_file()
 ├─ build_module_env()
 └─ export_public_symbols()
```

### 6.2 Graphe de dépendances

Représenté par une liste :

```
includes = list(
  "Main.R" = c("Utils.R", "Math.R"),
  "Math.R" = character(0),
  "Utils.R" = character(0)
)
```

---

## 7. Implications pour le runtime

### 7.1 `typr run`  
Utilise :

- `load_module(".", test = FALSE)`
- appelle `Main$main()`

### 7.2 Tests TypR  
Utilisent :

- `load_module(".", test = TRUE)`
- accèdent aux symboles `@testable`

### 7.3 REPL TypR  
Peut charger :

- tous les modules  
- les symboles publics  
- les symboles testables si demandé

---

## 8. Évolutions futures possibles

- support des **multi‑binaires** (`src/bin/*.typ`)  
- cache de modules pour accélérer le chargement  
- hot‑reload pour le REPL  
- introspection des modules (`modules$__meta__`)  

---

# 🧩 Synthèse

> Le Loader de Projet TypR est un système de chargement modulaire, déterministe, sans `source()`, qui reconstruit un namespace complet à partir des fichiers transpilés, en respectant la visibilité, les includes, et les règles de testabilité.
