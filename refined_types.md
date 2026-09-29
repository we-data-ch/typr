# Types raffinés — Spécification d’implémentation

## 1. Statut

Cette spécification définit l'implémentation des **types raffinés** dans TypR. Elle a pour but de remplacer les types dépendants dans le future.

L'objectif est de permettre l'expression de contraintes supplémentaires sur des types existants sans créer un nouveau type structurel pour chaque combinaison de contraintes.

Exemples :

```typr
int
int & (> 0)

[int]
[int] & length(5)
```

Le système doit également permettre, lorsque cela est possible, de transformer automatiquement les raffinements en **validations runtime dans le code R généré**.

---

# 2. Modèle conceptuel

Un type raffiné est une **intersection de type** entre un type de base et une ou plusieurs propriétés.

```text
RefinedType(A, P₁, ..., Pₙ)
```

est représenté conceptuellement par :

```text
A & P₁ & ... & Pₙ
```

où :

* `A` est un type TypR ;
* `Pᵢ` est une propriété vérifiable ;
* `&` est l'opérateur général d'intersection de types.

Exemple :

```typr
[int] & length(5)
```

signifie :

> une valeur de type `[int]` qui satisfait également la propriété `length(5)`.

Le mécanisme de raffinement est donc **générique** et ne doit pas être implémenté comme une famille de types spéciaux (`VectorOfLength`, `PositiveInt`, etc.).

---

# 3. Syntaxe

## 3.1 Intersection

La syntaxe générale est :

```text
TypeExpr & TypeExpr
```

`&` est associatif et commutatif au niveau sémantique.

Ainsi :

```typr
A & B
B & A
```

désignent le même ensemble de valeurs.

Une expression peut contenir plusieurs intersections :

```typr
A & B & C
```

---

## 3.2 Raffinements

Un raffinement est une expression reconnue comme une propriété applicable à un type.

Exemples initiaux :

```typr
int & (> 0)
int & (< 10)

[int] & length(5)
```

La grammaire exacte des propriétés n'est pas imposée par cette spécification ; celles-ci sont définies par le registre interne des raffinements du compilateur.

---

# 4. Sucre syntaxique des vecteurs

La notation :

```typr
[5, int]
```

doit être acceptée comme sucre syntaxique pour :

```typr
[int] & length(5)
```

Le parser ou une phase de désucrage doit normaliser les deux formes vers la même représentation interne.

Le type :

```typr
[5, int]
```

ne doit donc pas créer un nouveau constructeur de type distinct.

La forme canonique interne est :

```text
Intersection(
    Vector(Int),
    Length(5)
)
```

---

# 5. Représentation de type

L'AST/type representation doit représenter explicitement les intersections.

Une représentation minimale est :

```rust
enum Type {
    ...
    Intersection(Vec<Type>),
    ...
}
```

Cependant, une représentation normalisée séparant type de base et propriétés est recommandée :

```rust
struct RefinedType {
    base: Type,
    refinements: Vec<Refinement>,
}
```

avec :

```rust
enum Refinement {
    ...
}
```

Cette représentation est une optimisation sémantique et ne doit pas changer le comportement du langage.

Le compilateur doit être capable de retrouver l'équivalent général :

```text
A & P & Q
```

à partir de cette structure.

---

# 6. Normalisation

Toute intersection doit être normalisée avant les principales opérations du type checker.

La normalisation doit :

1. aplatir les intersections imbriquées ;
2. supprimer les doublons ;
3. rendre l'ordre des composants insignifiant ;
4. séparer les contraintes du type de base lorsque cela est possible ;
5. détecter les contradictions évidentes.

Exemple :

```typr
int & (> 0) & int
```

devient :

```typr
int & (> 0)
```

Et :

```typr
A & (B & C)
```

devient :

```typr
A & B & C
```

---

# 7. Types et propriétés

Une propriété ne constitue pas nécessairement un type indépendant utilisable partout.

Elle représente une contrainte applicable à un type compatible.

Exemple :

```typr
[int] & length(5)
```

est valide car `[int]` possède la capacité nécessaire à l'évaluation de `length`.

En revanche, une expression telle que :

```typr
int & length(5)
```

doit être rejetée si `int` ne possède pas cette capacité.

Le type checker doit donc vérifier la compatibilité :

```text
Base type
    ↓
supports refinement?
    ↓
yes / no
```

---

# 8. Capacités internes

Les propriétés doivent être associées à des **capacités internes** du compilateur.

Exemples :

```text
Lengthable
Comparable
Ordered
```

Ces capacités sont des concepts d'implémentation et ne constituent pas, dans cette première version, un mécanisme extensible par les utilisateurs.

Exemples :

```text
length(5)       → nécessite Lengthable
(> 0)           → nécessite Comparable / Ordered
(< 10)          → nécessite Comparable / Ordered
```

Le système doit permettre au type checker de déterminer qu'un type possède la capacité requise.

---

# 9. Sous-typage

Un type raffiné est un sous-type du type qu'il raffine.

```text
A & P <: A
```

Exemples :

```text
int & (> 0) <: int

[int] & length(5) <: [int]
```

De même :

```text
A & P & Q <: A & P
```

Cette règle est fondamentale.

Une fonction demandant :

```typr
let f(x: [int]) = ...
```

doit accepter une valeur de type :

```typr
[int] & length(5)
```

sans conversion.

---

# 10. Relations entre raffinements

Le type checker doit exposer au minimum trois opérations conceptuelles sur les raffinements :

```text
compatible(P, Q)
implies(P, Q)
contradicts(P, Q)
```

Exemples :

```text
length(5) compatible length(5)       → true
length(5) contradicts length(10)     → true
(> 10) contradicts (< 5)             → true
(> 5) implies (> 0)                   → true
```

L'analyse exacte peut être limitée dans une première version.

Lorsqu'une relation ne peut pas être déterminée statiquement, le compilateur doit conserver la contrainte plutôt que supposer qu'elle est vraie ou fausse.

---

# 11. Contradictions

Le type checker doit détecter les contradictions manifestes.

Exemples :

```typr
int & (> 10) & (< 5)
```

```typr
[int] & length(5) & length(10)
```

Ces types doivent être considérés comme inhabités.

Le compilateur peut produire une erreur au moment de leur construction ou de leur utilisation.

Une analyse complète des domaines numériques n'est pas requise dans la première implémentation.

---

# 12. Raffinements sur les paramètres

Les raffinements sont autorisés dans les types de paramètres.

```typr
let f(x: int & (> 0)) = ...
```

Le corps de `f` peut considérer `x` comme satisfaisant le raffinement.

Le compilateur ne doit pas générer une vérification supplémentaire à chaque utilisation de `x`.

La vérification éventuelle est associée à l'entrée dans le contexte où le contrat est requis.

---

# 13. Raffinements sur les valeurs de retour

Les raffinements sont également autorisés sur les types de retour.

```typr
let f(): [int] & length(5) = ...
```

Le compilateur doit vérifier que la valeur produite par le corps satisfait le type de retour.

Lorsque cette propriété ne peut pas être démontrée statiquement, une validation runtime doit être générée.

---

# 14. Inférence

Le type checker doit conserver les raffinements connus statiquement.

Exemple :

```typr
let x: [int] & length(5) = ...
```

`x` possède ce type dans l'environnement de typage.

La version initiale doit également permettre l'inférence de raffinements structurels lorsqu'ils sont déterminables sans analyse complexe.

Exemple :

```typr
let x = [1, 2, 3, 4, 5]
```

peut être typé :

```text
[int] & length(5)
```

Cette capacité doit être conçue comme une optimisation de l'information disponible et ne doit pas être nécessaire à la correction du système.

---

# 15. Préservation des raffinements

Les opérations doivent déclarer leur effet sur les raffinements.

Une opération peut :

```text
preserve
produce
weaken
invalidate
```

une propriété.

Exemple :

```typr
let x: [int] & length(5)
let y = x
```

Le type de `y` conserve :

```text
[int] & length(5)
```

À l'inverse :

```typr
let z = x[1]
```

produit :

```text
int
```

et ne conserve pas `length(5)`.

---

# 16. Opérations produisant de nouveaux raffinements

Lorsqu'une opération permet de déduire statiquement une nouvelle propriété, le type checker doit pouvoir l'ajouter au type résultat.

Exemple :

```typr
let x: [int] & length(5)
let y = x[1:3]
```

peut être typé :

```text
[int] & length(3)
```

si la sémantique de l'opération garantit cette longueur.

Cette fonctionnalité doit être implémentée de façon déclarative autant que possible.

---

# 17. Invalidations

Une opération dont le résultat ne permet plus de garantir un raffinement doit supprimer celui-ci du type statique.

Exemple conceptuel :

```text
x : [int] & length(5)

operation(x)

result : [int]
```

si `operation` ne garantit aucune longueur.

Il est interdit au type checker de conserver automatiquement un raffinement qu'une opération peut rendre faux.

---

# 18. Vérification statique

Lorsqu'un raffinement peut être prouvé au compile-time, aucune validation runtime correspondante ne doit être générée.

Exemple :

```typr
let x: [int] & length(5) = [1,2,3,4,5]
```

Le compilateur peut déduire :

```text
length(x) = 5
```

Aucun test supplémentaire n'est requis dans le code R généré.

---

# 19. Vérification runtime

Lorsqu'un raffinement est requis mais ne peut pas être prouvé statiquement, le compilateur doit générer une validation runtime.

Exemple :

```typr
let f(x: [int] & length(5)) = ...
```

Si `x` provient d'une source dynamique, le code généré doit effectuer une vérification équivalente à :

```r
if (length(x) != 5) {
    stop(...)
}
```

La forme exacte du code R généré reste une décision de codegen.

---

# 20. Principe "check at boundary"

Une propriété doit être vérifiée au point où elle devient nécessaire, plutôt qu'à chaque utilisation.

Exemple :

```text
source dynamique
      ↓
validation length(5)
      ↓
x : [int] & length(5)
      ↓
plusieurs utilisations sans nouvelle validation
```

Une fois la vérification effectuée dans un contexte donné, le type checker peut considérer la propriété comme établie dans ce contexte.

Le compilateur doit éviter les validations redondantes lorsqu'il dispose déjà d'une preuve ou d'une vérification antérieure encore valide.

---

# 21. Génération des validations

Chaque `Refinement` doit fournir au codegen les informations nécessaires pour générer sa validation.

Conceptuellement :

```rust
trait RefinementCodegen {
    fn check(
        &self,
        value: ValueRef,
        context: &CodegenContext,
    ) -> Expr;
}
```

Exemple :

```text
Length(5)
    ↓
length(value) == 5
```

et :

```text
GreaterThan(0)
    ↓
value > 0
```

Le code généré doit être idiomatique R autant que possible.

---

# 22. Origine des validations

Une validation runtime ne doit être générée que pour une contrainte effectivement imposée par le programme.

Le compilateur ne doit pas instrumenter arbitrairement toutes les valeurs avec toutes les propriétés connues.

Exemple :

```typr
let x: [int] = ...
```

ne doit pas déclencher de vérification de longueur.

En revanche :

```typr
let y: [int] & length(5) = x
```

doit déclencher une validation si aucune preuve statique ne permet de garantir `length(x) == 5`.

---

# 23. Raffinements et coercions

Une valeur ne doit pas automatiquement être considérée comme raffinée uniquement parce qu'elle possède le type de base.

Ainsi :

```text
[int]
```

n'implique pas :

```text
[int] & length(5)
```

Une conversion vers un type raffiné doit donc constituer un **site de preuve ou de validation**.

Conceptuellement :

```text
[int]
   ↓
check
   ↓
[int] & length(5)
```

---

# 24. Narrowing / raffinement par condition

La conception doit permettre ultérieurement le raffinement d'un type à la suite d'une condition.

Exemple :

```typr
if length(x) == 5 {
    ...
}
```

À l'intérieur du bloc, le compilateur pourrait établir :

```text
x : [int] & length(5)
```

Cette fonctionnalité n'est pas obligatoire pour la première version, mais la représentation des raffinements doit être conçue pour la supporter.

---

# 25. Génériques

Les raffinements doivent être compatibles avec les types génériques.

Exemple cible :

```typr
let first<T>(x: [T] & length(> 0)): T = ...
```

La validation de ce type nécessite que le système puisse déterminer les capacités dont dépend le raffinement.

La version initiale peut limiter les raffinements génériques à ceux dont les capacités sont connues statiquement.

---

# 26. Types composites

Les raffinements ne doivent pas être limités aux vecteurs.

Le mécanisme doit être applicable à tout type pour lequel la propriété possède une sémantique.

Exemples futurs :

```typr
string & length(10)

int & (> 0)

Record & some_property(...)
```

Les raffinements spécifiques à un domaine ne doivent donc pas être codés directement dans le système général des vecteurs.

---

# 27. Interaction avec les fonctions R

Pour chaque fonction ou opérateur connu du compilateur, les informations de typage doivent pouvoir préciser :

```text
arguments consommés
type résultat
raffinements produits
raffinements préservés
raffinements invalidés
```

Cela est particulièrement important pour les opérations vectorielles et le mécanisme de lifting de TypR.

Exemple conceptuel :

```text
length
    [T] → int

slice
    [T] & length(N), range(a,b)
        → [T] & length(...)
```

Les fonctions externes dont les effets ne sont pas connus doivent être traitées de manière conservatrice.

---

# 28. Interaction avec `c()`

`c()` et les autres opérations de combinaison de vecteurs doivent être considérées comme potentiellement génératrices de nouveaux raffinements.

Par exemple :

```typr
let x = c([1,2], [3,4,5])
```

peut éventuellement être inféré comme :

```text
[int] & length(5)
```

si les longueurs sont connues.

La conservation des raffinements doit cependant être dépendante de la sémantique réelle de l'opération et non d'une règle générale supposée.

---

# 29. Diagnostic statique

Les erreurs de typage doivent afficher les raffinements sous une forme lisible.

Exemple :

```text
Type mismatch

Expected:
    [int] & length(5)

Found:
    [int] & length(3)
```

Pour une contradiction :

```text
Unsatisfiable type refinement:

[int] & length(5) & length(10)
```

Pour une propriété incompatible :

```text
Invalid refinement:

length(5) cannot be applied to int
```

---

# 30. Diagnostic runtime

Les validations générées doivent produire des messages permettant d'identifier :

1. la propriété attendue ;
2. la valeur observée lorsque cela est pertinent ;
3. le point du programme où le contrat a été violé.

Exemple :

```text
Type refinement violation:
expected vector length 5, got 3
```

Le format exact du message peut être défini indépendamment du type checker.

---

# 31. Erreurs de vérification

Lorsqu'une propriété est impossible à établir statiquement, le compilateur ne doit jamais transformer silencieusement l'incertitude en vérité.

Il existe trois états conceptuels :

```text
Proven true
Proven false
Unknown
```

`Unknown` déclenche une validation runtime lorsqu'une garantie est nécessaire.

`False` produit une erreur statique lorsque la contradiction est certaine.

---

# 32. Phases d'implémentation

L'implémentation dans `typr-core` doit suivre au minimum les étapes suivantes :

```text
Parser
  ↓
AST
  ↓
Desugaring
  ↓
Type normalization
  ↓
Refinement validation
  ↓
Subtype checking
  ↓
Refinement propagation
  ↓
Static proof / constraint checking
  ↓
Runtime-check insertion
  ↓
R code generation
```

Le codegen ne doit pas avoir à comprendre la logique complète des raffinements : les décisions de typage et les points de validation doivent être déterminés auparavant.

---

# 33. Invariants du type checker

Le type checker doit garantir les invariants suivants.

### Invariant 1

Une intersection est indépendante de l'ordre de ses composants.

```text
A & B = B & A
```

### Invariant 2

Une intersection redondante est normalisée.

```text
A & A = A
```

### Invariant 3

Un raffinement est toujours associé à un type compatible.

### Invariant 4

Un type raffiné est toujours un sous-type de son type de base.

### Invariant 5

Une propriété statiquement prouvée ne nécessite pas de validation runtime.

### Invariant 6

Une propriété inconnue requise au runtime doit provoquer une validation.

### Invariant 7

Une propriété invalidée par une opération ne peut pas être conservée dans le type résultant.

---

# 34. Tests obligatoires

L'implémentation doit inclure des tests pour au minimum :

```text
int & (> 0)

[int] & length(5)

[5, int]

A & B == B & A

A & A == A

(A & B) & C == A & B & C

[int] & length(5) <: [int]

[int] & length(5) & length(10) → contradiction

[int] → [int] & length(5) → runtime check

statically provable refinement → no runtime check

refinement invalidation

refinement propagation

parameter refinement

return refinement
```

Des tests de codegen doivent vérifier à la fois :

```text
correctness of generated R
absence of unnecessary checks
presence of required checks
```

---

# 35. Première implémentation recommandée

La première version du système doit rester volontairement limitée.

Elle doit fournir :

```text
&
intersection de types

length(N)
longueur exacte

>
< 
comparaisons numériques simples

normalisation
déduplication
contradictions évidentes

subtyping
A & P <: A

runtime validation
génération automatique des checks R
```

La première version ne doit pas chercher à construire un solveur SMT ou un système de preuve général.

---

# 36. Extensions futures

Le système doit être conçu pour pouvoir accueillir ultérieurement :

```text
length(> 0)
length(< 100)

>=
<=

membership
x ∈ Set

regex / string constraints

numeric ranges

nullability constraints

data-frame shape constraints

column constraints

refinements dépendant de champs

flow-sensitive refinement

generic dependent refinements
```

Ces extensions ne doivent pas nécessiter de modifier le principe fondamental :

```text
base type & property
```

---

# 37. Exemple complet

Code TypR :

```typr
type Coordinates = [float] & length(2)

let distance(
    a: Coordinates,
    b: Coordinates
): float = ...
```

Une valeur déjà connue :

```typr
let a: Coordinates = [1.0, 2.0]
```

peut être prouvée statiquement.

Une valeur dynamique :

```typr
let a: [float] = read_coordinates()
let d = distance(a, b)
```

nécessite une validation avant l'appel :

```text
a : [float]

distance expects:
    [float] & length(2)

therefore:
    insert runtime check
```

Le R généré pourra conceptuellement contenir :

```r
if (length(a) != 2) {
    stop("Type refinement violation: expected length 2")
}

distance(a, b)
```

Le corps de `distance` peut ensuite manipuler `a` en supposant :

```text
a : [float] & length(2)
```

sans réintroduire la vérification à chaque accès.

---

# 38. Principe architectural final

Les types raffinés de TypR doivent être implémentés comme une **extension du système d'intersection existant**, et non comme un système de types parallèle.

Le cœur du modèle est :

```text
Type
  =
Base Type
  &
Properties
```

Par conséquent :

```typr
[int]
```

reste un type ordinaire,

```typr
[int] & length(5)
```

est une intersection,

et :

```typr
[5, int]
```

est uniquement une notation abrégée.

Le compilateur a ensuite la responsabilité de déterminer, pour chaque propriété :

```text
Est-elle compatible avec le type ?
        ↓
Est-elle déjà prouvée ?
        ↓
Peut-elle être déduite ?
        ↓
A-t-elle été invalidée ?
        ↓
Faut-il générer une validation R ?
```

Cette architecture permet à TypR d'utiliser progressivement les raffinements comme **contrats statiques et runtime**, tout en conservant un système de types simple, composable et compatible avec la nature incrémentale de TypR.
