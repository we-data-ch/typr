# 📘 RFC‑000X — *Kind Sigils for Generic Parameters in TypR*

## 1. Objectif

Cette RFC introduit un système de **sigils de kind** permettant d’annoter les paramètres génériques de TypR de manière :

- concise  
- lisible  
- extensible  
- compatible avec l’inférence  
- sans exposer la notion de *kind* au développeur moyen  

Le but est de remplacer les annotations explicites du style :

```
<N: Number> fn(a: N): Vec[N, int]
```

par une notation compacte :

```
fn(a: #N): Vec[#N, int]
```

où `#` encode le kind *Number*.

---

# 2. Motivation

TypR introduit plusieurs kinds : `Number`, `Record`, `Interface`, `String`, `Boolean`, etc.  
Les annotations explicites `<T: Kind>` deviennent rapidement :

- verbeuses  
- difficiles à lire  
- difficiles à maintenir  
- incompatibles avec une syntaxe élégante à la R/TS  

Les sigils permettent :

- d’exprimer le kind **dans le nom du générique**  
- d’éviter les déclarations de kind  
- de rendre la syntaxe plus expressive  
- d’ajouter de nouveaux kinds sans casser la grammaire  

---

# 3. Spécification

## 3.1. Définition

Un **sigil de kind** est un caractère préfixé à un identifiant générique, indiquant son kind.

```
#N   → générique numérique
%R   → générique record
@T   → générique interface
^S   → générique string
?B   → générique booléen
A    → générique polymorphe (sans sigil)
```

## 3.2. Syntaxe formelle

### 3.2.1. Identifiant générique

```
GenericIdent ::= Sigil? UppercaseLetter
Sigil ::= "#" | "%" | "@" | "^" | "?" | "~" | "$" | "&" | "!"
```

Les sigils additionnels (`~`, `$`, `&`, `!`) sont réservés pour des kinds futurs.

### 3.2.2. Kinds associés

| Sigil | Kind | Description |
|-------|-------|-------------|
| `#` | Number | entiers, flottants, index, dimensions |
| `%` | Record | types structurés à champs nommés |
| `@` | Interface | types satisfaisant un contrat |
| `^` | String | chaînes |
| `?` | Boolean | booléens |
| *(aucun)* | Any | kind polymorphe |

---

# 4. Règles de kinding

## 4.1. Générique sans sigil

```
A : Any
```

Il peut être instancié par n’importe quel kind.

## 4.2. Générique avec sigil

```
#N : Number
%R : Record
@T : Interface
```

Le kind est fixé par le sigil.

## 4.3. Contrainte de cohérence

Une instanciation doit respecter le kind :

```
Vec[#N, A]     ✓
Vec[%R, A]     ✗  (Record n’est pas Number)
```

## 4.4. Interaction avec les refined types

Un refined type peut s’appliquer uniquement si son kind est compatible :

```
#N & (. > 0)        ✓
%R & (.has("id"))   ✓
#N & (.has("id"))   ✗
```

---

# 5. Interaction avec les alias

Les sigils rendent les alias plus expressifs :

```
type IntArray<#N> <- [#N, int]
type Row<%R> <- %R & (.has("id"))
type ComparableList<@T> <- List[@T]
```

---

# 6. Interaction avec l’arithmétique de types

Les sigils s’intègrent naturellement :

```
fn reshape(a: Vec[#N * #M, A]): Mat[#N, #M, A]
```

ou :

```
type Slice<#I, #J> <- Range[#I, #J]
```

---

# 7. Inférence

## 7.1. Règle générale

Si un générique est introduit **sans sigil**, le compilateur infère son kind à partir de son usage.

Exemple :

```
fn id(x: A): A
```

→ `A : Any`

Exemple :

```
fn size(x: Vec[N, A]): #N
```

→ `N : Number` (inféré)

## 7.2. Conflits

Si plusieurs usages imposent des kinds incompatibles, erreur :

```
fn f(x: A): (#A + 1)
```

→ `A` doit être Number  
→ mais `A` est utilisé comme type dans `x: A`  
→ erreur de kind

---

# 8. Extension future

Le système est conçu pour être extensible :

| Sigil | Kind potentiel |
|-------|----------------|
| `~` | Path |
| `$` | Module |
| `&` | ThreadSafe |
| `!` | Effect |

Aucun changement syntaxique requis.

---

# 9. Exemples complets

## 9.1. Fonction numérique

```
fn zeros(#N): Vec[#N, int]
```

## 9.2. Fonction sur records

```
fn select(%R, field: ^S): ^S
```

## 9.3. Interface

```
fn sort(@T): List[@T]
```

## 9.4. Refined + sigils

```
fn clamp(x: #N & (. >= 0) & (. <= 1)): #N
```

## 9.5. Alias avancé

```
type Positive<#N> <- #N & (. > 0)
```

---

# 10. Conclusion

Ce système de sigils :

- encode les kinds dans la syntaxe  
- évite les annotations explicites  
- reste lisible et naturel  
- est extensible  
- s’intègre parfaitement avec les refined types et l’arithmétique  

C’est une solution élégante, cohérente, et parfaitement adaptée à TypR.
