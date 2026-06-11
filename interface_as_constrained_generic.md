# 📘 Spécification : les interfaces comme génériques contraints (bounded polymorphism)

Cette spécification définit la sémantique de typage des **interfaces** lorsqu'elles apparaissent en
**position de paramètre**. Elle corrige un comportement où une interface était traitée comme un
**type structurel concret**, ce qui provoque une **récursion infinie** du vérificateur de types sur
les interfaces auto-référentes (méthodes de la forme `(Self) -> Self`).

---

## 1) 🎯 Vue d'ensemble

Une interface n'est **pas** un type de valeur. C'est une **contrainte** sur un type.
Un paramètre de type interface se comporte comme un **générique universellement quantifié et
contraint** :

> `fn(i: I): R { ... }`  ≡  `forall A. (A: I) ⇒ (A) -> R[I ↦ A]`

Autrement dit, à l'intérieur du corps de la fonction, `i` **n'a pas** le type `I` ;
`i` a le type d'une **variable de type rigide fraîche** `A`, sous la contrainte « `A` implémente `I` ».

### Le problème corrigé

Soit l'interface auto-référente :

```typr
type Incrementable <- interface { incr: (Self) -> Self };
let double <- fn(i: Incrementable): Incrementable { i.incr().incr() };
```

- **Ancien comportement (incorrect)** : `i : Incrementable`. Résoudre `i.incr()` donne `Self = Incrementable`,
  donc `i.incr() : Incrementable`, et l'appel suivant ré-expanse l'interface indéfiniment ⇒
  **stack overflow** du vérificateur.
- **Comportement spécifié (correct)** : `i : A` avec `A: Incrementable`. La méthode
  `incr: (Self) -> Self` instanciée en `A` donne `(A) -> A`. Donc `i.incr() : A`, et
  `i.incr().incr() : A`. Aucune ré-expansion, **typage fini**.

---

## 2) 🧩 Terminologie

| Terme                         | Définition                                                                 |
|-------------------------------|----------------------------------------------------------------------------|
| **Interface** `I`             | Ensemble de signatures de méthodes ; une *contrainte*, pas un type de valeur. |
| **Méthode** `m: (Self) -> T`  | Membre d'une interface. `Self` désigne le type implémenteur.                |
| **`Self`**                    | Variable liée à l'interface ; pointe vers le type qui implémente `I`.       |
| **Variable de type rigide** `A` | Variable « skolem » fraîche, **opaque** : elle ne se réduit jamais vers la définition de `I`. |
| **Contrainte** `A: I`         | « `A` implémente structurellement `I` ».                                    |
| **Satisfaction** `C ⊨ I`      | Le type concret `C` fournit toutes les méthodes exigées par `I`.           |

---

## 3) 🟢 Règle d'or

> **Une interface en position de paramètre introduit une variable de type rigide fraîche
> contrainte. Dans le corps, le paramètre a le type de cette variable — jamais le type de
> l'interface elle-même. `Self` est identifié à cette variable.**

Conséquence directe : on **ne réduit jamais** une variable de type contrainte vers la définition
structurelle de l'interface pendant le typage du corps. C'est l'invariant qui garantit la
**terminaison**.

---

## 4) 🔧 Désucrage formel

### 4.1) Paramètre interface ⇒ variable rigide + contrainte

Pour chaque paramètre `iₖ: Iₖ` dont le type **se réduit** à une interface :

1. Générer une variable de type rigide fraîche `Aₖ`.
2. Ajouter au contexte la liaison `iₖ : Aₖ` et la contrainte `Aₖ: Iₖ`.
3. Substituer `Self ↦ Aₖ` dans les signatures de méthodes de `Iₖ`.

Le type de la fonction devient :

```
fn(i₁: I₁, …, iₙ: Iₙ): R
   ≡  ∀A₁…Aₙ. (A₁: I₁, …, Aₙ: Iₙ) ⇒ (A₁, …, Aₙ) -> R[I₁ ↦ A₁, …, Iₙ ↦ Aₙ]
```

### 4.2) Paramètres distincts ⇒ variables distinctes

Deux paramètres de **même** interface introduisent **deux** variables rigides distinctes :

```typr
fn(a: I, b: I): I    ≡    ∀A,B. (A: I, B: I) ⇒ (A, B) -> ???
```

Le type de retour `I` est ambigu (`A` ou `B` ?) — voir §7.3 (cas limites).

---

## 5) 🧠 Résolution de méthode sur une variable contrainte

C'est le cœur de la correction. Soit `e` d'un type variable contrainte `A: I`.

```
Γ ⊢ e : A        (A: I) ∈ Γ        (m : (Self, P₁, …, Pⱼ) -> T) ∈ methods(I)
───────────────────────────────────────────────────────────────────────────
Γ ⊢ e.m(x₁, …, xⱼ) : T[Self ↦ A]          (avec xₖ : Pₖ[Self ↦ A])
```

- Le receveur `e: A` **satisfait trivialement** le paramètre `Self` de la méthode (car `A: I`),
  **sans** appeler la machinerie de sous-typage structurel ni ré-expanser `I`.
- Le type de retour est `T` où `Self` est remplacé par `A`.

### Cas `incr: (Self) -> Self`

`T = Self`, donc `e.incr() : Self[Self ↦ A] = A`. Chaîner `e.incr().incr() : A`. ∎

---

## 6) 🧭 Sémantique au site d'appel

L'instanciation de la variable rigide a lieu **au moment de l'application**, avec un argument
**concret**.

```
Γ ⊢ arg : C        C ⊨ I        (instancie A := C)
──────────────────────────────────────────────────
Γ ⊢ f(arg) : R[A ↦ C]
```

### 6.1) Satisfaction `C ⊨ I`

`C` satisfait `I` si, pour **chaque** méthode `m: (Self, …) -> T` de `I`, le type `C` possède une
fonction `m: (C, …) -> T[Self ↦ C]` accessible dans le contexte.
(C'est le sous-typage structurel existant — `is_subtype_raw` côté implémentation.)

> ⚠️ La vérification `C ⊨ I` porte sur un type **concret** `C` : elle est **finie**, même si `I`
> est auto-référente, car `Self ↦ C` fige le receveur dès le premier niveau.

### 6.2) Substitution du retour

Le type de retour déclaré `R` (souvent `I` lui-même) est instancié `R[A ↦ C]`.
Pour `double(3)` avec `3: int` : `A := int`, retour `Incrementable[A ↦ int] = int`. ∎

---

## 7) 🧱 Règles sémantiques (récapitulatif)

### 7.1) ✔ Introduction (corps de fonction)
- Un paramètre interface devient une variable rigide fraîche, **opaque**.
- `Self` ≡ cette variable.
- Le corps est typé sous la contrainte ; la variable ne se déréduit **jamais** vers `I`.

### 7.2) ✔ Élimination (appel de méthode)
- Une méthode de l'interface, appelée sur la variable, retourne son type avec `Self ↦ A`.
- Les méthodes retournant `Self` **préservent** le type concret tout au long d'une chaîne.

### 7.3) ✔ Instanciation (site d'appel)
- L'argument concret `C` doit satisfaire `I` structurellement.
- La variable est instanciée `A := C` ; le retour est substitué.

### 7.4) ⚠ Invariant de terminaison
> Pendant le typage d'un corps, **aucune** variable de type contrainte n'est jamais remplacée par
> la définition structurelle (`reduce`) de son interface. Toute satisfaction d'interface
> (`⊨`) ne se fait que sur des types **concrets**, au site d'appel.

---

## 8) 🎁 Exemples

### 8.1) Valide — chaînage homogène
```typr
type Incrementable <- interface { incr: (Self) -> Self };
let double <- fn(i: Incrementable): Incrementable { i.incr().incr() };  // i:A, retour A
let incr   <- fn(i: int): int { i + 1 };                               // int ⊨ Incrementable
double(3)        // A := int  ⇒  : int
```

### 8.2) Valide — méthode ne retournant pas `Self`
```typr
type Sized <- interface { size: (Self) -> int };
let f <- fn(x: Sized): int { x.size() };   // x:A, x.size() : int (Self↦A non utilisé)
```

### 8.3) Valide — interface multi-méthodes
```typr
type Ord <- interface { lt: (Self, Self) -> bool, eq: (Self, Self) -> bool };
let min <- fn(a: Ord, b: Ord): Ord { ... };   // voir 8.4 pour l'ambiguïté du retour
```

### 8.4) Cas limite — paramètres multiples de même interface
`fn(a: I, b: I): I` introduit `A: I` et `B: I` **distincts**. Le retour `I` est **ambigu**.
Politique recommandée : exiger que les paramètres destinés à partager le type concret utilisent un
**générique explicite contraint** plutôt que deux occurrences de l'interface :

```typr
// ambigu (rejeté ou : retour = Any) :
let min <- fn(a: Ord, b: Ord): Ord { ... };

// explicite (A unique partagé) — à privilégier :
let min <- fn(a: A, b: A): A where A: Ord { ... };
```
*(La syntaxe `where A: I` est une extension possible ; voir §10.)*

### 8.5) Invalide — interface en position de retour seule
```typr
let make <- fn(): Incrementable { ... };   // existentiel : non supporté
```
Une interface qui n'apparaît **qu'en retour** (sans paramètre correspondant à instancier) est un
**type existentiel**. Elle est **rejetée** (erreur dédiée : utiliser un type `opaque`).

---

## 9) 🔬 Pourquoi cela termine

1. **Rigidité/opacité** : `A` est un atome. La résolution de méthode (§5) produit des types
   **construits à partir de `A`** (`A`, `int`, `(A) -> A`, …), jamais la définition dépliée de `I`.
2. **Satisfaction sur concret uniquement** : `C ⊨ I` (§6.1) fige `Self ↦ C` dès le premier niveau ;
   une interface auto-référente ne génère donc qu'une vérification de profondeur bornée.
3. **Pas de re-désucrage** : typer `i.m()` via la règle d'élimination (§5) ne reconstruit pas une
   nouvelle expression d'appel à re-typer (contrairement au chemin fautif qui passait par
   `function_application` ⇄ `typing`).

Ces trois points éliminent le cycle `typing ↔ function_application ↔ get_expanded_parameters`.

---

## 10) 🛠 Notes d'implémentation (mapping TypR)

Repères dans la base de code actuelle :

- **Typage du corps de fonction** : `processes/type_checking/function.rs` (`function`).
  C'est ici qu'un paramètre dont le type se réduit à `Type::Interface(_, _)` doit être lié à une
  **variable rigide fraîche** (`Type::Generic` « skolem » dédiée, distincte des génériques
  ordinaires) au lieu de conserver `Type::Interface`. Enregistrer la contrainte (var → interface)
  dans le `Context`.
- **Self** : `Self` est déjà normalisé en générique (`builder::self_generic_type`,
  `normalize_fn_param_names` dans `parsing/types.rs::interface`). Identifier `Self` à la variable
  rigide du receveur.
- **Résolution de méthode** : `processes/type_checking/function_application.rs`
  (`apply_from_variable`, `try_interface_subtype_match`). Ajouter une voie « receveur = variable
  contrainte » qui applique directement la règle d'élimination §5 (substitution `Self ↦ A`),
  **avant** toute tentative de sous-typage structurel.
- **Non-réduction** : `processes/type_checking/type_comparison.rs::reduce_type` ne doit jamais
  déplier une variable rigide contrainte vers la définition de l'interface.
- **Garde** : tant que la voie §5 n'est pas en place, un **garde-fou de profondeur** dans
  `apply_from_variable` (couper avec une `TypeError` au-delà d'un seuil) évite le crash.
- **Extension `where A: I`** (§8.4) : optionnelle ; permettrait des génériques explicitement
  contraints et de lever l'ambiguïté des paramètres interface multiples.

### Régressions à couvrir par les tests
- `test_interface_alias_structural_subtyping_dot_call` et la famille `test_interface_*`
  (actuellement en *stack overflow*) doivent passer.
- `(3).double()` et `double(3)` doivent inférer `int`.
- `int ⊨ Incrementable` via le `incr: (int) -> int` concret.

---

## 11) 📌 Résumé minimal

```
Paramètre interface   :  fn(i: I): R  ≡  ∀A. (A: I) ⇒ (A) -> R[I↦A]
Dans le corps         :  i : A   (rigide, jamais réduit vers I) ;  Self ≡ A
Appel de méthode      :  (m:(Self)->T) ∈ I,  e:A   ⊢   e.m() : T[Self↦A]
Site d'appel          :  arg:C,  C ⊨ I   ⊢   f(arg) : R[A↦C]
Terminaison           :  A opaque + satisfaction sur types concrets uniquement
```
