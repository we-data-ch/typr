# 📘 Spécification TypR — Fonction polymorphe `c(...)`

## 1. Objet  
La fonction `c(...)` est une primitive polymorphe de TypR permettant :

- **la concaténation de vecteurs**  
- **la concaténation de listes ou tuples**  
- **la construction de vecteurs homogènes**  

La résolution suit un ordre strict de spécialisation.

---

## 2. Syntaxe  
```
c(e1, e2, ..., en)
```

où `n ≥ 1`.

---

## 3. Règles de typage (hiérarchisées)

### 3.1 🔹 Règle 1 — Concaténation de vecteurs  
**Condition :**  
\[
\forall i,\ \Gamma \vdash e_i : \text{Vec}[n_i, T]
\]

**Conclusion :**  
\[
\Gamma \vdash c(e_1,\ldots,e_n) : \text{Vec}\Big[\sum_i n_i,\ T\Big]
\]

**Notes :**  
- Les types internes doivent être identiques (`T`).  
- Les tailles sont additionnées statiquement.  

---

### 3.2 🔹 Règle 2 — Concaténation de listes ou tuples  
**Condition (listes) :**  
\[
\forall i,\ \Gamma \vdash e_i : \text{list}\{A_i...\}
\]

**Conclusion :**  
\[
\Gamma \vdash c(e_1,\ldots,e_n) : \text{list}\{A_1..., A_2..., ..., A_n...\}
\]

**Condition (tuples) :**  
\[
\forall i,\ \Gamma \vdash e_i : \text{Tuple}[A_i...]
\]

**Conclusion :**  
\[
\Gamma \vdash c(e_1,\ldots,e_n) : \text{Tuple}[A_1..., A_2..., ..., A_n...]
\]

**Notes :**  
- La concaténation est purement structurelle.  
- Aucun renommage, aucune fusion de champs.  

---

### 3.3 🔹 Règle 3 — Vecteur homogène  
**Condition :**  
\[
\forall i,\ \Gamma \vdash e_i : T
\]

et `T` est un type scalaire (non conteneur).

**Conclusion :**  
\[
\Gamma \vdash c(e_1,\ldots,e_n) : \text{Vec}[n, T]
\]

**Notes :**  
- Cette règle ne s’applique que si les règles 1 et 2 ont échoué.  
- Elle ne doit jamais capturer des vecteurs ou des listes.  

---

### 3.4 🔹 Règle 4 — Erreur de typage  
Si aucune des règles précédentes ne s’applique :

\[
\Gamma \vdash c(e_1,\ldots,e_n) : \text{error}
\]

**Exemples d’erreurs :**

- `c(1, Vec[2,int])`  
- `c(list{a:int}, 3)`  
- `c(Vec[2,int], list{a:int})`  

---

## 4. Ordre de résolution (obligatoire)

1. **Vecteurs**  
2. **Listes / tuples**  
3. **Scalaires homogènes**  
4. **Erreur**

Cet ordre garantit :

- aucune ambiguïté  
- aucune capture indésirable par la règle homogène  
- une sémantique cohérente avec R mais typée statiquement  

---

## 5. Sémantique opérationnelle

### 5.1 Vecteurs  
```
c(Vec(x1), Vec(x2), ...)  ↦  Vec(concat(x1, x2, ...))
```

### 5.2 Listes / tuples  
```
c(list{A}, list{B}, ...)  ↦  list{A..., B..., ...}
```

### 5.3 Vecteur homogène  
```
c(v1, v2, ..., vn)  ↦  Vec([v1, v2, ..., vn])
```

### 5.4 Erreur  
```
c(...) ↦ error("incompatible arguments for c(...)")
```

---

## 6. Exemples validés

| Expression | Type inféré |
|-----------|--------------|
| `c(1,2,3)` | `Vec[3,int]` |
| `c(Vec[2,int], Vec[3,int])` | `Vec[5,int]` |
| `c(list{a:int}, list{b:char})` | `list{a:int, b:char}` |
| `c((1,"a"), (TRUE,3.14))` | `Tuple[int,char,bool,float]` |
| `c(1, Vec[2,int])` | erreur |

---

## 7. Extension future (optionnelle)

Tu peux ajouter plus tard :

- **concaténation de matrices**  
- **concaténation de dataframes**  
- **coercions contrôlées**  

La hiérarchie restera la même.

---

# 🎯 Conclusion  
Cette spécification est **propre, formelle, extensible et sans ambiguïté**.  
Elle s’intègre parfaitement dans ton système de types paramétriques et dans la philosophie TypR.
