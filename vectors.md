# RFC: Système de vecteurs `Vec[N, T]` pour TypR

## 1. Objet et portée

**Objet:**  
Cette RFC spécifie le type de vecteur paramétré `Vec[N, T]` dans TypR, avec :

- **T** restreint aux **types primitifs**  
- **N** taille déterminable statiquement ou marquée `Any`  
- **aucune coercition implicite**, **aucun recycling**  
- **interop R stricte**  
- règles formelles pour `c()` et l’indexation.

**Hors scope (futur):**

- unions dans `T`  
- vecteurs de listes ou structures  
- contraintes avancées sur `N` (dépendants, bornes, etc.)  

---

## 2. Syntaxe et grammaire

### 2.1 Forme générale

Un type vecteur est noté :

\[
\texttt{Vec[N, T]}
\]

où :

- \(\texttt{N} \in \mathbb{N}^+ \cup \{\texttt{Any}\}\)
- \(\texttt{T} \in \{\texttt{int}, \texttt{double}, \texttt{bool}, \texttt{char}, \texttt{complex}, \dots\}\) (ensemble des types primitifs de TypR)

### 2.2 Grammaire (informelle)

```bnf
TypeVec   ::= "Vec" "[" SizeParam "," PrimType "]"
SizeParam ::= IntLiteral | "Any"
PrimType  ::= "int" | "double" | "bool" | "char" | "complex" | ...
```

---

## 3. Sémantique de `Vec[N, T]`

### 3.1 Homogénéité

**Invariant 1 — Homogénéité:**

Un `Vec[N, T]` contient exclusivement des valeurs de type **exactement** `T` (primitive).  
Aucun élément d’un autre type n’est autorisé, même s’il existe une coercition R classique (ex. `int → double`).

### 3.2 Taille

**Cas 1 — Taille déterminée:**

- `N` est un entier strictement positif connu statiquement.
- La taille du vecteur est **exactement** `N`.

**Cas 2 — Taille indéterminée:**

- `N = Any` signifie : *taille inconnue statiquement*.
- À l’exécution, la taille est un entier \(\ge 0\), mais non reflété dans le type.

**Invariant 2 — Stabilité de la taille:**

- Pour un `Vec[N, T]` avec `N` déterminé, toute opération qui change la taille doit produire un nouveau type avec une taille calculable (ex. `N+1`, `N+M`).
- Pour un `Vec[Any, T]`, les opérations peuvent produire `Vec[Any, T]` si la taille n’est pas déterminable statiquement.

---

## 4. Règles de typage de base

### 4.1 Formation de type

\[
\frac{
  N \in \mathbb{N}^+ \cup \{\texttt{Any}\}
  \quad
  T \in \text{PrimType}
}{
  \texttt{Vec[N, T]} \ \text{est un type valide}
}
\]

### 4.2 Interdictions

- **Interdit:** `Vec[N, A | B]` (unions)  
- **Interdit:** `Vec[N, List[T]]` ou tout type non primitif  
- **Interdit:** coercions implicites entre `Vec[N, T1]` et `Vec[M, T2]` si `T1 ≠ T2`  

---

## 5. Opérations primitives

### 5.1 Construction

#### 5.1.1 Littéraux / constructeurs

Forme conceptuelle :

```typr
Vec[N, T](v1, v2, ..., vN)
```

**Règle de typage:**

\[
\frac{
  \forall i, \ \Gamma \vdash v_i : T
}{
  \Gamma \vdash \texttt{Vec[N, T](v1, ..., vN)} : \texttt{Vec[N, T]}
}
\]

- Si le nombre d’arguments est connu et égal à \(N\), le type est `Vec[N, T]`.
- Si le nombre d’arguments n’est pas déterminable statiquement, le type est `Vec[Any, T]`.

### 5.2 Fonction `c(...)`

On définit une version TypR stricte de `c` pour les vecteurs primitifs.

#### 5.2.1 Signature conceptuelle

Cas de base (homogène, tailles connues) :

\[
\texttt{c} : \texttt{Vec[N, T]} \times \texttt{Vec[M, T]} \to \texttt{Vec[N+M, T]}
\]

Plus généralement, pour une séquence d’arguments \(a_1, \dots, a_k\) où chaque \(a_i\) est soit un scalaire de type `T`, soit un `Vec[Ni, T]` :

- On définit la taille totale \(S = \sum_i s_i\), où \(s_i = 1\) si scalaire, \(s_i = N_i\) si vecteur.
- Si tous les \(s_i\) sont déterminables statiquement, alors :

\[
\texttt{c}(a_1, \dots, a_k) : \texttt{Vec[S, T]}
\]

- Sinon :

\[
\texttt{c}(a_1, \dots, a_k) : \texttt{Vec[Any, T]}
\]

#### 5.2.2 Règle de typage formelle (homogénéité)

\[
\frac{
  \forall i, \ \Gamma \vdash a_i : \tau_i
  \quad
  \forall i, \ \tau_i \in \{T, \texttt{Vec[Ni, T]}\}
}{
  \Gamma \vdash \texttt{c}(a_1, \dots, a_k) : \texttt{Vec[N', T]}
}
\]

où :

- si tous les `Ni` sont connus et tous les scalaires sont comptés comme `1` :  
  \[
  N' = \sum_i s_i
  \]
- sinon :  
  \[
  N' = \texttt{Any}
  \]

**Rejet de type:**

- Si \(\exists i, j\) tels que `τi` et `τj` n’ont pas le même `T` primitif → **erreur de typage** (aucune coercition implicite, aucun upcast).

### 5.3 Indexation

On distingue :

- **indexation scalaire**  
- **indexation vectorielle**

#### 5.3.1 Indexation scalaire

Expression :

```typr
x[i]
```

**Règle de typage:**

\[
\frac{
  \Gamma \vdash x : \texttt{Vec[N, T]}
  \quad
  \Gamma \vdash i : \texttt{int}
}{
  \Gamma \vdash x[i] : T
}
\]

- Valable pour tout `N` (y compris `Any`).
- Les erreurs d’index hors bornes sont **dynamiques**, pas statiques (sauf cas particuliers d’analyse ultérieure).

#### 5.3.2 Indexation vectorielle

Expression :

```typr
x[I]
```

**Règle de typage:**

\[
\frac{
  \Gamma \vdash x : \texttt{Vec[N, T]}
  \quad
  \Gamma \vdash I : \texttt{Vec[M, int]}
}{
  \Gamma \vdash x[I] : \texttt{Vec[M, T]}
}
\]

- Si `M` est connu, le résultat est `Vec[M, T]`.
- Si `M = Any`, le résultat est `Vec[Any, T]`.

**Remarque:**  
Aucun recycling n’est appliqué entre `x` et `I`. Si `I` contient des indices hors bornes, l’erreur est dynamique.

---

## 6. Absence de coercition et de recycling

### 6.1 Coercion implicite

**Règle:**

- Il n’existe **aucune** règle de typage qui permette :

\[
\texttt{Vec[N, T1]} \Rightarrow \texttt{Vec[N, T2]} \quad \text{si } T1 \ne T2
\]

- Toute tentative d’utiliser un `Vec[N, T1]` là où un `Vec[M, T2]` est attendu avec `T1 ≠ T2` est une **erreur de typage**.

### 6.2 Recycling

Le comportement R suivant est **interdit** dans TypR :

```r
c(1:3, 1:2)  # recycling en R
```

En TypR, toute opération qui nécessiterait un recycling implicite est :

- soit **rejetée au typage** si détectable statiquement,
- soit **interdite par la sémantique** (erreur d’exécution) si elle survient dynamiquement.

---

## 7. Interopérabilité avec R

### 7.1 Principe général

Interop **stricte par défaut** :

- Un vecteur R natif n’est **jamais** implicitement considéré comme un `Vec[N, T]`.
- Une conversion explicite est requise.

### 7.2 Conversion explicite

On suppose une primitive conceptuelle :

```typr
as_vec[T](x) -> Vec[Any, T]
```

**Règle de typage:**

\[
\frac{
  \Gamma \vdash x : \texttt{RVector}
  \quad
  \text{conversion R} \Rightarrow T \in \text{PrimType}
}{
  \Gamma \vdash \texttt{as_vec`[Il semble que le résultat n’était pas sûr à afficher. Changeons un peu et essayons autre chose !]`} : \texttt{Vec[Any, T]}
}
\]

- Si la conversion R → `T` n’est pas possible sans coercion (au sens R), la conversion doit **échouer** (erreur d’exécution ou de typage selon le modèle choisi).
- TypR ne simule pas les règles de coercion R à ce niveau : la conversion est **sémantiquement stricte**.

---

## 8. Inférence de taille et de type

### 8.1 Inférence de `T`

Pour une expression `c(...)` ou un constructeur `Vec[...]` :

- `T` est inféré comme le **type primitif commun** de tous les arguments.
- Si les arguments ne partagent pas le même type primitif → **erreur de typage** (aucun LUB implicite).

### 8.2 Inférence de `N`

- Si tous les arguments ont des tailles connues et le nombre d’arguments scalaires est connu, `N` est calculé statiquement.
- Sinon, `N` est inféré comme `Any`.

---

## 9. Exemples canoniques

### 9.1 Construction simple

```typr
let v: Vec[3, int] = Vec[3, int](1, 2, 3)
```

Valide, type exact.

```typr
let v = Vec[3, int](1, 2, 3)
// v : Vec[3, int] (inférence)
```

### 9.2 `c()` homogène

```typr
let a: Vec[2, double] = Vec[2, double](1.0, 2.0)
let b: Vec[1, double] = Vec[1, double](3.0)

let c = c(a, b)
// c : Vec[3, double]
```

### 9.3 `c()` avec taille indéterminée

```typr
let n = read_int()          // inconnu statiquement
let x = make_vec_double(n)  // x : Vec[Any, double]

let y: Vec[2, double] = Vec[2, double](1.0, 2.0)

let z = c(x, y)
// z : Vec[Any, double]
```

### 9.4 Indexation

```typr
let v: Vec[4, int] = Vec[4, int](10, 20, 30, 40)

let x = v[2]        // x : int
let idx: Vec[2, int] = Vec[2, int](1, 4)
let y = v[idx]      // y : Vec[2, int]
```

### 9.5 Interop stricte

```typr
let r_vec = some_r_function()   // type R natif

let v = as_vec[int](r_vec)      // v : Vec[Any, int]
```

---

## 10. Résumé des invariants

1. **Homogénéité:** `Vec[N, T]` ne contient que des valeurs de type primitif `T`.  
2. **Taille:** `N` est soit un entier positif connu, soit `Any`.  
3. **Pas de coercion implicite:** aucun passage automatique entre types primitifs.  
4. **Pas de recycling:** aucune extension implicite de vecteurs.  
5. **Interop stricte:** conversion explicite requise depuis les vecteurs R.  
6. **`c()` et indexation:** typage déterministe, calculable, sans comportement implicite à la R.
