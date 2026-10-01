# RFC TypR – Génériques implicites basés sur les interfaces

> **Objet :** permettre plusieurs paramètres génériques distincts partageant la même interface,  
> sans introduire de `T`, `U` explicites, ni casser le modèle “interface = type”.

---

## 1. Contexte et objectifs

### 1.1. Contexte

Dans TypR :

- **Les interfaces sont des types à part entière**, définis via des alias :
  ```typr
  type Printable <- interface {
      print: (Self) -> char
  };
  ```
- Une interface sert **à la fois** :
  - de **type structurel** (annotation classique),
  - de **contrainte générique** (on peut écrire `fn(xs: Iterable): Iterable`).

Ce modèle est volontairement simple pour des développeurs venant de R :  
pas de `T`, `U`, pas de syntaxe de type paramétrique explicite.

### 1.2. Problème

Avec ce modèle, une fonction :

```typr
let compare <- fn(a: Lovable, b: Lovable): bool {
    ...
};
```

ne peut pas distinguer deux **types concrets différents** pour `a` et `b` :

- `Lovable` est un **type structurel unique**,
- donc `a` et `b` sont du **même type structurel**.

On ne peut pas exprimer :  
“`a` est un type qui satisfait `Lovable`, `b` est un autre type (potentiellement différent) qui satisfait aussi `Lovable`”.

### 1.3. Objectif

Introduire un mécanisme de **génériques implicites** qui :

- **sépare les paramètres** (`a` et `b` peuvent être de types différents),
- **reste structurel** (pas de nominalisme),
- **reste simple** pour des devs R (pas de `T`, `U`),
- **respecte** le modèle mental : *interface = type = contrainte*.

---

## 2. Design proposé – paramètres implicites `@Id`

### 2.1. Idée centrale

On ajoute une **annotation légère** sur les paramètres :

```typr
let compare <- fn(a: Lovable@A, b: Lovable@B): bool {
    ...
};
```

- `Lovable` : interface/type structurel.
- `@A`, `@B` : **identifiants de paramètre générique**, *non typés*.
- Chaque identifiant (`A`, `B`) représente un **paramètre générique distinct**.

### 2.2. Règle intuitive

- Deux annotations `Lovable@A` et `Lovable@B` :
  - partagent la **même interface** (`Lovable`),
  - mais sont **deux paramètres génériques différents**.
- L’inférence peut donc déduire :
  - `A = Cat`,
  - `B = Dog`,
  - tant que `Cat` et `Dog` satisfont `Lovable`.

---

## 3. Syntaxe formelle

### 3.1. Définition d’interface

Syntaxe inchangée :

```typr
type Lovable <- interface {
    love: (Self) -> char
};
```

### 3.2. Annotation de paramètre

#### 3.2.1. Forme générale

Pour un paramètre :

```typr
name: Interface@Id
```

- **`name`** : identifiant du paramètre (variable).
- **`Interface`** : alias de type pointant vers une interface.
- **`Id`** : identifiant de paramètre générique (nom court, style `A`, `B`, `Item`, etc.).

#### 3.2.2. Forme minimale

Si aucun `@Id` n’est fourni :

```typr
name: Interface
```

- Comportement **actuel** :  
  tous les paramètres annotés avec `Interface` **partagent le même type structurel**.
- Ce mode reste valide et est le **mode par défaut**.

#### 3.2.3. Exemple

```typr
let pair <- fn(a: Lovable@Left, b: Lovable@Right): (Lovable@Left, Lovable@Right) {
    (a, b)
};
```

---

## 4. Sémantique – modèle de types

### 4.1. Interfaces comme types structurels

On conserve :

- Une interface `I` est un **type structurel**.
- Un alias :
  ```typr
  type I <- interface { ... };
  ```
  introduit un **type nommé** `I` avec une **structure requise**.

### 4.2. Paramètres génériques implicites

Pour chaque identifiant `Id` utilisé dans `Interface@Id` :

- Le compilateur introduit un **paramètre de type implicite** \(\alpha_{Id}\).
- La contrainte associée est :
  \[
  \alpha_{Id} : Interface
  \]
  c’est‑à‑dire :  
  “\(\alpha_{Id}\) doit satisfaire la structure de `Interface`”.

### 4.3. Distinction des paramètres

Deux annotations :

```typr
a: Lovable@A
b: Lovable@B
```

induisent :

- deux paramètres de type distincts \(\alpha_A\) et \(\alpha_B\),
- chacun contraint par `Lovable`.

Il n’y a **aucune relation** imposée entre \(\alpha_A\) et \(\alpha_B\)  
(sauf si le corps de la fonction impose des contraintes supplémentaires).

---

## 5. Règles d’inférence

### 5.1. Collecte des paramètres

Pour une fonction :

```typr
let f <- fn(a: Lovable@A, b: Lovable@B): bool {
    ...
};
```

Le compilateur :

1. **Collecte** les identifiants `A`, `B`.
2. Crée un ensemble de paramètres de type :
   \[
   \{\alpha_A, \alpha_B\}
   \]
3. Associe les contraintes :
   \[
   \alpha_A : Lovable,\quad \alpha_B : Lovable
   \]

### 5.2. Résolution à l’appel

À l’appel :

```typr
f(cat, dog);
```

Le compilateur :

- infère le type de `cat` : `Cat`,
- infère le type de `dog` : `Dog`,
- vérifie :
  - `Cat` satisfait `Lovable`,
  - `Dog` satisfait `Lovable`,
- unifie :
  - \(\alpha_A = Cat\),
  - \(\alpha_B = Dog\).

### 5.3. Cas sans `@Id`

Pour :

```typr
let f <- fn(a: Lovable, b: Lovable): bool {
    ...
};
```

Le compilateur :

- introduit **un seul** paramètre \(\alpha\),
- avec contrainte :
  \[
  \alpha : Lovable
  \]
- à l’appel, `a` et `b` doivent être du **même type** \(\alpha\).

---

## 6. Exemples d’usage

### 6.1. Comparaison de deux Lovable distincts

```typr
type Lovable <- interface {
    love: (Self) -> char
};

let compare <- fn(a: Lovable@A, b: Lovable@B): bool {
    a.love() == b.love()
};
```

- `A` et `B` peuvent être deux types différents.
- On compare leur “score d’amour”.

### 6.2. Fonction symétrique

```typr
let swap <- fn(a: Lovable@A, b: Lovable@B): (Lovable@B, Lovable@A) {
    (b, a)
};
```

- Le type de retour encode la permutation des paramètres génériques.

### 6.3. Interface générique + paramètres implicites

```typr
type Box <- interface<T> {
    value: T
};

let merge <- fn(a: Box@A, b: Box@B): (Box@A, Box@B) {
    (a, b)
};
```

- `Box@A` et `Box@B` peuvent encapsuler des types différents.
- L’inférence doit gérer :
  - le paramètre implicite `A`,
  - le paramètre explicite `T` de `Box`.

---

## 7. Points ouverts et variantes

### 7.1. Identifiants optionnels

On peut autoriser :

- `Lovable@A` (nom explicite),
- `Lovable@_` (paramètre anonyme),
- `Lovable#` (syntaxe alternative plus légère).

À discuter selon la **lisibilité** et la **culture R**.

### 7.2. Réutilisation d’un identifiant

Si on écrit :

```typr
let pair <- fn(a: Lovable@X, b: Lovable@X): (Lovable@X, Lovable@X) {
    (a, b)
};
```

- `a` et `b` partagent le **même paramètre générique** \(\alpha_X\),
- donc doivent être du **même type**.

C’est l’équivalent de `T` réutilisé dans les langages classiques,  
mais sans introduire `T` comme type nominal.

### 7.3. Erreurs de typage

Cas à définir clairement :

- utilisation de `Lovable@A` dans le retour sans paramètre correspondant,
- identifiant `@A` utilisé mais jamais contraint,
- conflit entre deux interfaces incompatibles sur le même `@A`.

---

## 8. Résumé conceptuel

- **Interfaces** restent :
  - des **types structurels**,
  - des **contrats**,
  - des **espaces de fonctions**.
- Les **génériques implicites** sont :
  - des **identifiants de contrainte** (`@A`, `@B`),
  - non typés,
  - résolus par **inférence**.

> On ne demande jamais au développeur R de “penser en `T`, `U`”.  
> On lui dit juste :  
> “si tu veux que deux paramètres soient indépendants, donne‑leur chacun un petit tag `@A`, `@B`”.
