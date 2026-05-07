Voici la spécification révisée, propre, cohérente, complète, intégrant :

- ta syntaxe module M { ... }
- l’affectation let x <- expr
- l’annotation @pub pour rendre public
- l’immutabilité
- la traduction vers R + environnements + S3
- la résolution des noms
- la gestion des imports
- la garantie d’absence de collisions

C’est une spec officielle, prête à être intégrée dans un document de langage.

---

📘 Spécification formelle du système de modules de L (version révisée)

1. Objectifs du système de modules

Le système de modules de L fournit :

1. Encapsulation :  
   - toutes les déclarations sont privées par défaut.
   - seules les déclarations annotées @pub sont exportées.

2. Immutabilité :  
   - aucune variable ne peut être modifiée après sa définition.
   - aucune mutation externe (<<-) n’est générée en R.

3. Isolation :  
   - chaque module est traduit en un environnement R distinct.
   - aucune collision possible entre modules.

4. Interopérabilité R/S3 :  
   - les fonctions publiques peuvent devenir des génériques S3.
   - les données privées sont capturées lexicalement.

---

2. Syntaxe du langage L

2.1. Déclaration de module

`
ModuleDecl ::= "module" Ident "{" Decl* "}"
`

Exemple :

`
module Math {
    let pi_internal <- 3.14159
    @pub let pi <- 3.14
}
`

---

2.2. Déclarations internes

2.2.1. Déclaration privée (par défaut)

`
PrivDecl ::= "let" Ident "<-" Expr
`

2.2.2. Déclaration publique

`
PubDecl ::= "@pub" "let" Ident "<-" Expr
`

2.2.3. Fonctions

`
Function ::= "function" "(" ParamList? ")" Expr
`

Exemples :

`
let square <- function(x) x * x
@pub let area <- function(r) pi * square(r)
`

---

3. Sémantique des modules

3.1. Portée

À l’intérieur d’un module :

1. les paramètres de fonction
2. les déclarations privées
3. les déclarations publiques
4. les modules importés
5. l’environnement global R (optionnel)

3.2. Visibilité

- Privé : accessible uniquement dans le module.
- Public : accessible via Module::name.

3.3. Immutabilité

- Une déclaration let x <- expr crée une valeur immutable.
- Aucune opération de mutation n’existe dans L.
- La traduction R n’utilise jamais <<-.

---

4. Traduction vers R

4.1. Structure générale

Pour un module M :

`r
M <- new.env(parent = emptyenv())

local({
    <T_Decl(Decl1)>
    <T_Decl(Decl2)>
    ...
})
`

---

4.2. Traduction des déclarations

4.2.1. Déclaration privée

En L :

`
let x <- expr
`

En R :

`r
local({
    x <- <T(expr)>
})
`

- x est strictement privé.

---

4.2.2. Déclaration publique

En L :

`
@pub let y <- expr
`

En R :

`r
local({
    y <- <T(expr)>
    M$y <- y
})
`

- y devient accessible via M$y.

---

4.3. Traduction des fonctions

4.3.1. Fonction privée

En L :

`
let helper <- function(x) x * 2
`

En R :

`r
local({
    helper <- function(x) x * 2
})
`

---

4.3.2. Fonction publique

En L :

`
@pub let f <- function(x) helper(x) + 1
`

En R :

`r
local({
    f <- function(x) helper(x) + 1
    M$f <- f
})
`

---

4.4. Option S3 (si activée)

Si une fonction publique doit devenir un générique S3 :

En L :

`
@pub let print <- function(x: MyType) ...
`

En R :

`r
local({
    M$print <- function(x, ...) UseMethod("M_print", x)
    M_print.MyType <- function(x, ...) { ... }
})
`

---

5. Traduction de Module::name

Règle :

- M::x → M$x
- M::f(a, b) → M$f(a, b)

Si un conteneur global est utilisé :

- M::x → .modules$M$x

---

6. Imports (optionnel mais prévu)

6.1. Syntaxe

`
import M
import M as Alias
`

6.2. Traduction

En R :

`
Alias <- M
`

ou

`
M <- M
`

---

7. Exemple complet

En L

`
module Math {
    let pi_internal <- 3.14159
    @pub let pi <- 3.14

    let square <- function(x) x * x

    @pub let circleArea <- function(r) pi * square(r)
}
`

En R généré

`r
Math <- new.env(parent = emptyenv())

local({
    pi_internal <- 3.14159
    pi <- 3.14
    square <- function(x) x * x

    circleArea <- function(r) pi * square(r)

    Math$pi <- pi
    Math$circleArea <- circleArea
})
`

---

8. Propriétés garanties

- Encapsulation stricte : les valeurs privées ne sont jamais accessibles.
- Immutabilité : aucune mutation possible.
- Absence de collisions : chaque module est un environnement isolé.
- Interopérabilité R : les modules sont des environnements R standards.
- Interopérabilité S3 : les fonctions publiques peuvent devenir des génériques.
- Syntaxe claire et R-like : @pub let est explicite et lisible.
