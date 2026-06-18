# 1. Objet du document

**Titre:** TypR Type Arithmetic RFC  
**But:** Définir formellement les opérateurs de composition de types de TypR (`+`, `-`, `*`, `/`, `|`, `&`), leurs domaines de définition, leurs règles de typage et de réduction, ainsi que les contraintes de *kind* (kinds de types).

---

# 2. Kinds et univers de types

### 2.1. Ensemble des kinds

On suppose un ensemble fini de kinds :

- **`Number`** — types numériques (ex : `Int`, `Float`, `NumberLiteral(3)`).
- **`Boolean`**
- **`String`**
- **`Record`** — types structurés à champs nommés.
- **`Function`**
- **`Union`** — types union normalisés.
- **`Intersection`** — types intersection normalisés.
- **`Any`**
- **`Never`**

On définit une fonction de classification :

- **`kind : Type → Kind`**

Certaines constructions imposent un kind particulier (ex : un record a toujours kind `Record`).

---

# 3. Syntaxe des types

### 3.1. Grammaire informelle

```bnf
Type ::=
    BaseType
  | NumberLiteral(n)
  | Record { FieldList }
  | Type "|" Type
  | Type "&" Type
  | Type "+" Type
  | Type "-" Type
  | Type "*" Type
  | Type "/" Type
  | Never
  | Any
  | ...
```

```bnf
FieldList ::= /* liste finie de champs distincts par nom */
Field     ::= Ident ":" Type
```

On suppose une forme normalisée pour les records :  
les champs sont uniques par nom et ordonnés (ordre canonique).

---

# 4. Domaine de définition des opérateurs

## 4.1. Opérateurs numériques `+`, `-`, `*`, `/`

**Règle de validité (kinds)**

Pour tout opérateur binaire `⊙ ∈ {+, -, *, /}` :

- **Validité :**

  \[
  T_1 ⊙ T_2 \text{ est bien formé ssi } kind(T_1) = Number \land kind(T_2) = Number
  \]

Sinon, l’expression de type est **illégale** (erreur de type au niveau du système de types, pas un type `Never`).

---

## 4.2. Union `|`

**Règle de validité**

- **Toujours bien formé** :  
  \[
  T_1 | T_2 \text{ est toujours bien formé}
  \]

Le kind de l’union est déterminé par une fonction `joinKind` (voir §6).

---

## 4.3. Intersection `&`

**Contrainte que tu veux :**

- `&` n’est autorisé **que pour les types de kind `Record` (et éventuellement `Interface`)**.

On formalise :

- **Validité :**

  \[
  T_1 \& T_2 \text{ est bien formé ssi } kind(T_1) = Record \land kind(T_2) = Record
  \]

Sinon, c’est une **erreur de type** (pas un type `Never`).

---

# 5. Règles de typage (style jugement Γ ⊢ T : K)

On note `Γ ⊢ T : K` “dans le contexte Γ, T a le kind K”.

### 5.1. Numérique

\[
\frac{Γ ⊢ T_1 : Number \quad Γ ⊢ T_2 : Number}
     {Γ ⊢ T_1 ⊙ T_2 : Number}
\quad ⊙ ∈ \{+, -, *, /\}
\]

### 5.2. Union

\[
\frac{Γ ⊢ T_1 : K_1 \quad Γ ⊢ T_2 : K_2}
     {Γ ⊢ T_1 | T_2 : joinKind(K_1, K_2)}
\]

### 5.3. Intersection

\[
\frac{Γ ⊢ T_1 : Record \quad Γ ⊢ T_2 : Record}
     {Γ ⊢ T_1 \& T_2 : Record}
\]

---

# 6. Sémantique des kinds pour `|` et `&`

## 6.1. Fonction `joinKind` pour `|`

On définit une fonction partielle (ou totale avec `Any`/`Union`) :

- **Cas simples :**

  - `joinKind(Number, Number) = Number`
  - `joinKind(Record, Record) = Record`
  - `joinKind(K, K) = K`
  - `joinKind(Any, K) = Any`
  - `joinKind(K, Any) = Any`

- **Cas hétérogènes** (ex : `Number | String`) :

  Tu as deux options :
  1. **Kind générique `Union`** :  
     `joinKind(K1, K2) = Union` si `K1 ≠ K2` et aucun cas spécial.
  2. Ou bien tu gardes `Any` comme “super-kind” :  
     `joinKind(K1, K2) = Union` au niveau type, mais `kind(T1 | T2) = Union`.

Je te recommande :

- **Décision :**  
  - `kind(T1 | T2) = Union` si `kind(T1) ≠ kind(T2)` et aucun cas spécial.  
  - Sinon, `kind(T1 | T2) = kind(T1)`.

---

## 6.2. Intersection `&` et kinds

Comme tu restreins `&` à `Record` (et éventuellement `Interface`), on a simplement :

- `kind(T1 & T2) = Record`.

---

# 7. Règles de réduction / normalisation

On définit une relation de réduction `⇒` sur les types, et une fonction de **normalisation** `norm(T)` qui applique ces règles jusqu’à forme normale.

---

## 7.1. Numérique

On suppose des littéraux numériques `NumberLiteral(n)`.

### 7.1.1. Réduction forte sur littéraux

- **Addition :**

  \[
  NumberLiteral(a) + NumberLiteral(b) ⇒ NumberLiteral(a + b)
  \]

- Idem pour `-`, `*`, `/` (avec règle spéciale pour division par zéro).

### 7.1.2. Propagation vers `Number`

- `NumberLiteral(a) + Number ⇒ Number`  
- `Number + NumberLiteral(a) ⇒ Number`  
- `Number + Number ⇒ Number`  

Même schéma pour `-`, `*`, `/`.

### 7.1.3. Erreurs

- `NumberLiteral(a) / NumberLiteral(0)` → **erreur de type** (ou `Never` si tu veux l’encoder comme type impossible).

---

## 7.2. Union `|` — lois algébriques

On veut `|` **associatif**, **commutatif**, **idempotent**.

### 7.2.1. Idempotence

- `T | T ⇒ T`

### 7.2.2. Neutralité de `Never`

- `Never | T ⇒ T`

### 7.2.3. Absorption par `Any`

- `Any | T ⇒ Any`

### 7.2.4. Flatten

- `(T1 | T2) | T3 ⇒ T1 | T2 | T3`  
  (et symétrique)

En pratique, `norm` représente une union comme un **multiset / set** de types, trié, sans doublons.

---

## 7.3. Intersection `&` — fusion de records

On suppose des records normalisés :

```text
Record{ a: A, b: B, ... }
```

### 7.3.1. Fusion de champs

Soient :

```text
R1 = Record{ a: A, b: B }
R2 = Record{ b: C, c: D }
```

Alors :

```text
R1 & R2 ⇒ Record{ a: A, b: B & C, c: D }
```

Plus généralement :

- Pour chaque champ `x` :
  - si `x` est dans un seul record, il est recopié tel quel ;
  - si `x` est dans les deux, son type devient `T1 & T2`.

### 7.3.2. Interaction avec `Never` et `Any`

- `Record & Never ⇒ Never`  
- `Record & Any ⇒ Record`  

### 7.3.3. Champ impossible

Si un champ fusionné devient `Never` :

```text
Record{ a: A } & Record{ a: Never } ⇒ Never
```

(on peut définir une règle : si un champ obligatoire a type `Never`, le record entier est `Never`).

---

## 7.4. Interaction `|` / `&` (distributivité contrôlée)

On autorise une **distributivité unidirectionnelle** :

- **Distributivité de `&` sur `|` à droite :**

  \[
  (T_1 | T_2) \& R ⇒ (T_1 \& R) | (T_2 \& R)
  \]

Sous condition :

- `kind(R) = Record`
- `kind(T1) = kind(T2) = Record` ou bien on laisse `T_i & R` se typer (et éventuellement échouer).

On **n’impose pas** la distributivité inverse (pour éviter l’explosion combinatoire).

---

# 8. Algorithme de normalisation

On définit une fonction :

- **`norm : Type → Type`**

### 8.1. Stratégie générale

1. **Évaluer récursivement** les sous-types.
2. **Appliquer les règles locales** (numérique, union, intersection).
3. **Canonicaliser** :
   - unions → ensemble trié, sans doublons, sans `Never`, avec absorption de `Any` ;
   - intersections de records → fusion de champs, propagation de `Never`.

### 8.2. Esquisse de pseudo-code

```text
norm(T):
  match T:
    case NumberLiteral(a) ⊙ NumberLiteral(b):
      return reduceNumeric(a, ⊙, b)  // peut lever erreur

    case T1 ⊙ T2 where ⊙ ∈ {+, -, *, /}:
      T1' = norm(T1)
      T2' = norm(T2)
      // vérifier kind(T1') = kind(T2') = Number
      return normNumeric(T1', ⊙, T2')

    case T1 | T2:
      U1 = norm(T1)
      U2 = norm(T2)
      return normUnion(U1, U2)

    case T1 & T2:
      I1 = norm(T1)
      I2 = norm(T2)
      // vérifier kind(I1) = kind(I2) = Record
      return normIntersection(I1, I2)

    case Record{ fields }:
      // normaliser chaque champ
      fields' = { x: norm(Tx) ... }
      return Record{ canonicalize(fields') }

    case _:
      return T
```

Avec :

```text
normUnion(U1, U2):
  // flatten
  S = flattenUnion(U1) ∪ flattenUnion(U2)
  // enlever Never
  S = S \ { Never }
  // si Any ∈ S → Any
  if Any ∈ S: return Any
  // enlever doublons, trier
  return buildUnionFromSet(S)

normIntersection(R1, R2):
  // R1, R2 sont des Record normalisés
  fields = {}
  for each field name x in (dom(R1) ∪ dom(R2)):
    if x ∈ R1 and x ∈ R2:
      Tx = norm( R1[x] & R2[x] )
      if Tx = Never: return Never
      fields[x] = Tx
    else if x ∈ R1:
      fields[x] = R1[x]
    else:
      fields[x] = R2[x]
  return Record{ canonicalize(fields) }
```

---

# 9. Erreurs de type vs `Never`

Tu as deux philosophies possibles :

- **A. Erreur de type statique**  
  Quand une opération est hors domaine (ex : `String + Number`), le type-checker rejette le programme.

- **B. Encodage dans `Never`**  
  Tu peux décider que certaines combinaisons donnent `Never` (type impossible), mais **seulement** si l’expression est syntaxiquement valide dans le langage de types.

Je te conseille :

- **Opérations hors kind** → erreur de type (pas un type).  
- Réductions internes (ex : champ fusionné en `Never`) → `Never`.

---

# 10. Résumé des contraintes clés pour TypR

1. **Kinds explicites** pour contrôler les opérateurs.
2. **Numérique** :
   - `+ - * /` seulement sur `kind = Number`.
   - Réduction sur littéraux, propagation vers `Number`.
3. **Union `|`** :
   - Toujours bien formé.
   - Normalisation : associatif, commutatif, idempotent, `Never` neutre, `Any` absorbant.
4. **Intersection `&`** :
   - **Uniquement** pour `kind = Record` (et éventuellement `Interface`).
   - Fusion de champs, propagation de `Never`.
5. **Interaction `|` / `&`** :
   - Distributivité contrôlée : `(T1 | T2) & R ⇒ (T1 & R) | (T2 & R)`.
6. **Normalisation** :
   - Passe systématique pour obtenir une forme canonique.
