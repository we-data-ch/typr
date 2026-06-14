# 📘 RFC‑TR‑014 — *Type paramétrique `Factor` dans TypR*

## 1. Résumé  
Cette RFC définit le type paramétrique **`Factor[L]`**, où `L` est un vecteur constant de chaînes représentant les niveaux autorisés.  
Le type factor fournit une représentation statique et sûre des facteurs R, tout en conservant la compatibilité avec la sémantique native de R.

---

## 2. Motivation  
Les facteurs sont un type fondamental en R, mais leur représentation est :

- dynamique  
- peu sûre  
- dépendante d’attributs runtime  
- sujette aux erreurs silencieuses (recode, levels manquants, coercions implicites)

TypR vise à fournir :

- **une représentation statique des niveaux**  
- **une vérification de validité à la compilation**  
- **une compatibilité totale avec les facteurs R**  

---

## 3. Spécification formelle

### 3.1 Définition du type  
Le type factor est défini comme un alias paramétrique :

```
type Factor[L <: Vec[char]] <- int
```

- `L` est un vecteur constant de chaînes (`Vec[k, char]`)  
- la représentation interne est un entier (`int`)  
- les métadonnées `levels = L` sont attachées par l’annotateur

### 3.2 Constructeur  
La fonction standard `factor` est définie comme :

```
factor(x: char, levels: Vec[char]) -> Factor[levels]
```

Elle :

- vérifie que `x ∈ levels`  
- retourne l’index correspondant (1‑based)  
- construit un `Factor[levels]`

### 3.3 Annotateur  
Pour tout `L`, TypR génère automatiquement :

```
Factor[L](x: int) -> Factor[L]
```

Il :

- vérifie que `1 ≤ x ≤ length(L)`  
- attache les attributs R nécessaires  
- retourne un `Factor[L]`

---

## 4. Règles de typage

### 4.1 Typage du constructeur  
Si `x : char` et `levels : Vec[n, char]`, alors :

```
factor(x, levels) : Factor[levels]
```

### 4.2 Typage de l’annotateur  
Si `x : int` et `1 ≤ x ≤ |L|`, alors :

```
Factor[L](x) : Factor[L]
```

### 4.3 Compatibilité structurelle  
Deux types `Factor[L1]` et `Factor[L2]` sont compatibles **uniquement si** :

```
L1 == L2
```

Il n’existe **aucune** sous‑typage entre facteurs.

### 4.4 Interdiction des coercions implicites  
Les conversions suivantes sont interdites :

- `char → Factor[L]` (sauf via `factor`)  
- `int → Factor[L]` (sauf via annotateur)  
- `Factor[L1] → Factor[L2]` si `L1 != L2`

---

## 5. Sémantique opérationnelle

### 5.1 Construction  
Pour `factor(x, L)` :

```
i = indexOf(x, L)
if i == 0 → erreur
return structure(i, class="factor", levels=L)
```

### 5.2 Annotateur  
Pour `Factor[L](i)` :

```
if i < 1 or i > length(L) → erreur
return structure(i, class="factor", levels=L)
```

### 5.3 Égalité  
Deux valeurs `a : Factor[L]` et `b : Factor[L]` sont égales si :

```
a.index == b.index
```

---

## 6. Transpilation vers R

### 6.1 Constructeur  
```
factor(x, levels = L)
```

### 6.2 Annotateur  
TypR génère :

```r
Factor_L <- function(x) {
  structure(x, class = "factor", levels = L)
}
```

### 6.3 Représentation interne  
Un `Factor[L]` est toujours un entier R avec :

- `class = "factor"`  
- `levels = L`

---

## 7. Erreurs

### 7.1 Niveau invalide  
```
factor("X", c("A","B"))
→ Error: value "X" not in levels
```

### 7.2 Annotateur hors bornes  
```
Factor[c("A","B")](3)
→ Error: index 3 out of bounds for Factor[c("A","B")]
```

### 7.3 Incompatibilité de types  
```
Factor[c("A","B")] != Factor[c("A","B","C")]
```

---

## 8. Notes d’implémentation

- Les vecteurs `L` doivent être **constants** pour permettre la vérification statique.  
- Le solveur de types doit propager les paramètres `L` dans les dataframes.  
- Les opérations sur facteurs (comparaison, tri, etc.) doivent être définies dans une RFC séparée.  

---

## 9. Résumé  
Cette RFC introduit :

- un type paramétrique `Factor[L]`  
- un constructeur sûr  
- un annotateur généré automatiquement  
- une compatibilité totale avec R  
- une sécurité statique forte  
