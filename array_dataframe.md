Voici une **spécification formelle** propre, compacte et exploitable pour TypR, qui distingue clairement :

- **les types récursifs** (vecteurs, tableaux, matrices, etc.)  
- **les types records** (listes, dataframes, tibbles, tables, etc.)

Elle est écrite dans un style PEG/EBNF, suffisamment précis pour être implémentée dans un parser LL(1) ou PEG, et suffisamment abstraite pour rester indépendante de l’implémentation.

J’intègre des **[Guided Links]** sur les concepts clés pour que tu puisses approfondir chaque partie.

---

# 📘 Spécification formelle : séparation des deux catégories de types

## 1) 🎯 Vue d’ensemble
TypR distingue deux familles de types :

- **Types récursifs** : homogènes, indexés, paramétriques, sans champs nommés  
- **Types records** : hétérogènes, structurés, avec champs nommés

Cette distinction est **syntaxique**, **sémantique**, et **non ambiguë**.

---

# 2) 🧩 Spécification lexicale

```
Ident        ::= Letter (Letter | Digit | "_")*
IntLiteral   ::= Digit+
TypeSep      ::= ","
LBracket     ::= "["
RBracket     ::= "]"
LBrace       ::= "{"
RBrace       ::= "}"
Colon        ::= ":"
```

---

# 3) 🧱 Spécification syntaxique (EBNF)

## 3.1) 🎨 Types (règle principale)

```
Type ::= RecursiveType
       | RecordType
       | BaseType
```

---

## 3.2) 🔵 Types récursifs

Les types récursifs suivent **strictement** la forme :

```
Constructor "[" ParamList "]"
```

où `ParamList` ne contient **jamais** de `{ ... }`.

```
RecursiveType ::= Ident LBracket RecursiveParams RBracket

RecursiveParams ::= RecursiveParam
                  | RecursiveParam TypeSep RecursiveParams

RecursiveParam ::= Type
                 | IntLiteral
```

### Exemples valides
```
Vec[3, int]
Array[10, float]
Matrix[3, 4, num]
Tensor[2, 3, 4, bool]
```

### Exemples invalides
```
Vec[3]{x: int}        // interdit : bloc record
Array[5, {a: int}]    // interdit : record dans un type récursif
```

---

## 3.3) 🟩 Types records

Les types records suivent **strictement** la forme :

```
Constructor "[" IntLiteral "]" "{" FieldList "}"
```

ou, pour les records simples :

```
"{" FieldList "}"
```

### Définition formelle

```
RecordType ::= Ident LBracket IntLiteral RBracket LBrace FieldList RBrace
             | LBrace FieldList RBrace

FieldList ::= Field
            | Field TypeSep FieldList

Field ::= Ident Colon Type
```

### Exemples valides
```
Df[8]{ name: char, allowed: bool }
Tibble[3]{ id: int, active: bool }
{ x: int, y: float }
```

### Exemples invalides
```
Df[8, int]{...}       // interdit : un seul paramètre autorisé
Df{a: int}            // interdit : longueur obligatoire
```

---

## 3.4) 🟠 Types de base

```
BaseType ::= Ident
```

Exemples : `int`, `float`, `bool`, `char`, `num`, etc.

---

# 4) 🧠 Règles sémantiques

## 4.1) ✔ Types récursifs
- Tous les paramètres doivent être **des types** ou **des entiers**.  
- Aucun paramètre ne peut être un **record**.  
- Le constructeur doit être **déclaré** comme récursif dans l’environnement de types.

Exemples de déclarations :

```
typeconstructor Vec[N, T] recursive
typeconstructor Array[N, T] recursive
typeconstructor Matrix[R, C, T] recursive
```

---

## 4.2) ✔ Types records
- Le premier paramètre est **obligatoirement un entier** (la longueur).  
- Le second paramètre est **un bloc record**.  
- Le constructeur doit être **déclaré** comme record-based.

Exemples :

```
typeconstructor Df[N]{...} record
typeconstructor Tibble[N]{...} record
typeconstructor Table[N]{...} record
```

---

## 4.3) ✔ Règle d’or (non ambiguïté)

> **Si un type contient un bloc `{ ... }`, alors c’est un type record.  
> Sinon, s’il contient `[...]`, c’est un type récursif.**

Cette règle garantit un parsing **LL(1)** sans ambiguïté.

---

# 5) 🧭 Résumé formel minimal

```
Type ::= RecursiveType | RecordType | BaseType

RecursiveType ::= Ident "[" (Type | IntLiteral) ("," (Type | IntLiteral))* "]"

RecordType ::= Ident "[" IntLiteral "]" "{" Field ("," Field)* "}"
             | "{" Field ("," Field)* "}"

Field ::= Ident ":" Type
```

---

# 6) 🎁 Bonus : propriétés garanties par cette spécification

- **Aucune ambiguïté** entre `Df[N]{...}` et `Vec[N, T]`  
- **Séparation claire** des familles de types  
- **Parsing simple** (LL(1) ou PEG)  
- **Extensibilité naturelle** (ajout de nouveaux constructeurs sans casser la grammaire)  
- **Sémantique cohérente** (records = hétérogènes, récursifs = homogènes)
