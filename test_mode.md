# 📘 RFC‑TR‑031 — *Système d’exposition conditionnelle des fonctions privées pour les tests*

## 1. Objet de la RFC  
Cette RFC définit le mécanisme permettant à TypR d’exposer certaines fonctions privées **uniquement dans le contexte des tests**, afin de permettre leur utilisation par les frameworks de test externes (ex. testthat), tout en préservant l’encapsulation dans les builds normaux.

Le mécanisme repose sur :

- une annotation `@testable`  
- un mode de compilation `--test`  
- une règle de transpilation conditionnelle TypR → R  
- une convention d’exposition dans l’environnement du module

---

## 2. Motivations

### 2.1 Problème  
Dans TypR, les fonctions privées sont encapsulées dans un bloc `local({ ... })`, ce qui les rend **réellement inaccessibles** depuis l’extérieur.  
Les tests générés par TypR (via les blocs `Test { ... }`) sont exécutés dans un environnement séparé, typiquement celui de testthat, qui ne peut donc pas accéder aux fonctions privées.

### 2.2 Objectifs  
- permettre le test des fonctions privées  
- ne pas casser l’encapsulation en production  
- ne pas exposer accidentellement des symboles privés  
- conserver une sémantique simple et prévisible  
- minimiser les modifications au runtime R

---

## 3. Syntaxe : annotation `@testable`

### 3.1 Définition  
L’annotation `@testable` peut être appliquée à :

- une fonction privée  
- une valeur privée  
- un type privé (optionnel, selon les besoins futurs)

### 3.2 Exemple

```r
@testable private fun helper(x: int) -> int = x + 1
```

### 3.3 Règle  
Une déclaration annotée `@testable` :

- reste privée dans un build normal  
- est exposée sous un nom contrôlé dans un build test

---

## 4. Sémantique

### 4.1 Build normal (`typR build`)  
Les symboles privés, testables ou non, sont :

- définis dans un environnement fermé  
- non accessibles depuis l’extérieur  
- non exportés dans l’API du module

### 4.2 Build test (`typR build --test`)  
Les symboles annotés `@testable` sont :

- définis normalement dans l’environnement privé  
- **exposés dans l’environnement public du module** sous un nom réservé  
- accessibles depuis les tests générés ou externes

### 4.3 Nom d’exposition  
Le nom d’exposition suit la convention :

```
.test_<nom_original>
```

Exemple :

```
@testable private fun helper(...)  →  M$.test_helper
```

---

## 5. Transpilation TypR → R

### 5.1 Build normal  
Transpilation standard :

```r
M <- local({
    helper <- function(x) x + 1
    public <- function(...) ...
    list(public = public)
})
```

### 5.2 Build test  
Transpilation augmentée :

```r
M <- local({
    helper <- function(x) x + 1
    public <- function(...) ...

    # exposition conditionnelle
    .test_helper <- helper

    list(
        public = public,
        .test_helper = .test_helper
    )
})
```

### 5.3 Règles de génération  
- seuls les symboles annotés `@testable` sont exposés  
- les symboles privés non testables restent invisibles  
- les symboles publics ne sont pas modifiés  
- l’exposition ne modifie pas la sémantique interne du module

---

## 6. Intégration avec les blocs `Test { ... }`

### 6.1 Génération des fichiers test  
Les blocs TypR :

```r
Test {
    expect_equal(helper(5), 6)
}
```

sont transpilés en fichiers testthat classiques :

```r
test_that("helper works", {
    expect_equal(M$.test_helper(5), 6)
})
```

### 6.2 Chargement du module en mode test  
Le runner de test TypR charge automatiquement les modules en mode test :

```r
M <- load_module("M", test = TRUE)
```

---

## 7. Contraintes et invariants

### 7.1 Encapsulation  
- en build normal : encapsulation **totale**  
- en build test : encapsulation **préservée**, mais exposition contrôlée

### 7.2 Nom réservé  
Les noms commençant par `.test_` sont réservés et ne peuvent pas être utilisés par l’utilisateur.

### 7.3 Pureté du runtime  
Le runtime R n’est pas modifié :  
toute la logique est dans le transpileur.

---

## 8. Erreurs et diagnostics

### 8.1 Annotation invalide  
Erreur si `@testable` est appliqué à :

- un symbole public  
- un symbole inexistant  
- un symbole déjà exposé

### 8.2 Conflit de nom  
Erreur si un symbole `.test_<name>` existe déjà dans le module.

---

## 9. Exemple complet

### 9.1 Code TypR

```r
module Math:

    @testable private fun sq(x: int) -> int = x * x

    fun norm(x: int, y: int) -> int =
        sq(x) + sq(y)

    Test {
        expect_equal(sq(3), 9)
    }
```

### 9.2 Build normal  
`sq` est inaccessible.

### 9.3 Build test  
`sq` devient `M$.test_sq`.

### 9.4 Test généré

```r
test_that("sq works", {
    expect_equal(M$.test_sq(3), 9)
})
```

---

## 10. Extensions futures possibles

- `@testable(module)` pour exposer tout un module privé  
- `@testonly` pour marquer des fonctions *uniquement* utilisées dans les tests  
- exposition sélective par pattern (`expose Test { sq, norm }`)

---

# 🎯 Conclusion  
Cette spécification :

- préserve l’encapsulation  
- permet de tester les fonctions privées  
- reste simple à implémenter  
- s’aligne sur les pratiques modernes (Swift, Rust, Kotlin)
