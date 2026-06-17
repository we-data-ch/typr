# 📘 **TypR RFC — Spread Operator for Records & Lists**

> Statut : **implémenté**. `Lang::List` porte un champ `spreads: Vec<Lang>` en plus de
> `value: Vec<ArgumentValue>` (`components/language/mod.rs`). Parsing : `record_spread_field` /
> `record_field` dans `processes/parsing/elements.rs`. Typing : bras `Lang::List` dans
> `processes/type_checking/mod.rs` (+ `merge_record_fields_override`). Transpilation : bras
> `Lang::List` (spread non vide) dans `processes/transpiling/mod.rs`, qui émet exactement les
> appels `spread(...)` décrits en §6-§7. Runtime : `spread()` ajouté à
> `crates/typr-cli/configs/src/std.R`. Tests : parsing (`elements.rs`), typing/transpilation
> (`type_checking/mod.rs`, section "Record Literal Spread Tests"), `lab/spread2.ty`.
>
> Implémenté selon la règle formelle de §3.1/§4/§7 : `override` = **tous** les champs explicites
> du literal (peu importe leur position textuelle), toujours prioritaires ; `base` = fusion
> séquentielle de tous les spreads dans l'ordre d'apparition. L'exemple de §6.4 (`{ a = 1, ...x, b
> = 2 }` → `spread(list(a=1), spread(x, list(b=2)))`) illustre une sémantique positionnelle qui
> contredit cette règle (il laisserait un champ de `x` écraser le `a = 1` explicite) ; il n'a pas
> été implémenté tel quel — `{ a = 1, ...x, b = 2 }` transpile en `spread(x, list(a = 1, b = 2))`,
> conformément à la règle formelle.
>
> **Extension — constructeurs nominaux** : le `...` est aussi accepté dans
> `TypeName:{ ...source, field = val }` (`Lang::ConstructorCall.spreads: Vec<Lang>`, distinct du
> champ `spread` qui porte le `..` statique de RFC-TR-033). Typing : structurel — `source` peut
> avoir **plus** de champs que `TypeName` (contrairement à `..` qui exige un alias identique) ;
> seule la couverture des champs déclarés est vérifiée. Transpilation : merge runtime via
> `spread(...)`, puis sélection exacte des champs du type cible avec `merged[c("f1","f2")]` avant
> l'appel via `do.call(TypeName, ...)` — les champs en trop sur `source` sont donc abandonnés (le
> `Person` résultant ne porte que les champs déclarés par `Person`, validé par `validate_Person`),
> à l'inverse du spread sur record literal anonyme qui préserve tout. Tests :
> `test_constructor_call_runtime_spread_parsing` (`elements.rs`),
> `test_constructor_runtime_spread_*` (`type_checking/mod.rs`).

## 1. 🎯 **Objectif**

Cette spécification définit :

- un opérateur de **spreading** pour les records TypR  
- un mécanisme de **fusion shallow** compatible avec le row‑polymorphism  
- une stratégie de **transpilation vers R** utilisant une fonction standard `spread()`  
- une sémantique déterministe et indépendante du nombre de champs connus statiquement  

Le but est de permettre d’écrire en TypR :

```typ
let x = { a = 1, b = 2 }
let y = { ...x, b = 10, c = 3 }
```

…et d’obtenir en R :

```r
spread(x, list(b = 10, c = 3))
```

---

# 2. 🧩 **Syntaxe TypR**

## 2.1. Spread dans un record literal

```
RecordLiteral ::= "{" FieldList? "}"
FieldList ::= Field ("," Field)*
Field ::= SpreadField | Assignment
SpreadField ::= "..." Expression
Assignment ::= Identifier "=" Expression
```

Exemples valides :

```typ
{ ...x }
{ ...x, a = 1 }
{ a = 1, ...x }
{ ...x, ...y, z = 3 }
```

---

# 3. 🧠 **Sémantique**

## 3.1. Principe général

Un record literal contenant un ou plusieurs spreads est évalué comme :

```
spread(base, override)
```

où :

- `base` est la fusion séquentielle de tous les spreads dans l’ordre d’apparition  
- `override` est la liste des champs explicitement définis dans le literal  

### Exemple

```typ
{ ...x, ...y, a = 1, b = 2 }
```

Transpile en :

```r
spread(spread(x, y), list(a = 1, b = 2))
```

---

# 4. 🧬 **Règles de typage (row‑polymorphism)**

## 4.1. Spread d’un record

Si :

```
x : { a: T1, b: T2 | ρ }
```

alors :

```
{ ...x } : { a: T1, b: T2 | ρ }
```

## 4.2. Spread + override

Pour :

```typ
{ ...x, a = e1, c = e2 }
```

Le type résultant est :

```
{ a: typeof(e1), b: T2, c: typeof(e2) | ρ }
```

avec :

- `a` écrasé par l’override  
- `b` conservé depuis `x`  
- `c` ajouté  
- `ρ` préservé (champs inconnus)  

## 4.3. Spread multiple

Les spreads sont fusionnés séquentiellement :

```
{ ...x, ...y, ...z, a = 1 }
```

Type :

```
merge(merge(merge(type(x), type(y)), type(z)), { a: int })
```

---

# 5. 🛠️ **Backend R — Fonction standard `spread()`**

La standard library TypR doit fournir :

```r
spread <- function(base, override) {
  out <- base
  for (nm in names(override)) {
    out[[nm]] <- override[[nm]]
  }
  out
}
```

### Propriétés

- **shallow merge**  
- **override prioritaire**  
- **préservation des champs inconnus**  
- **compatible row‑polymorphism**  
- **pas de récursion**  

---

# 6. 🔄 **Transpilation TypR → R**

## 6.1. Cas simple : un seul spread

TypR :

```typ
{ ...x, a = 1 }
```

R :

```r
spread(x, list(a = 1))
```

---

## 6.2. Spread multiple

TypR :

```typ
{ ...x, ...y, a = 1 }
```

R :

```r
spread(spread(x, y), list(a = 1))
```

---

## 6.3. Spread sans override

TypR :

```typ
{ ...x }
```

R :

```r
x
```

(optimisation : pas besoin d’appeler `spread()`)

---

## 6.4. Spread + champs avant/après

TypR :

```typ
{ a = 1, ...x, b = 2 }
```

R :

```r
spread(list(a = 1), spread(x, list(b = 2)))
```

---

# 7. 📐 **Règles de transpilation formelles**

Soit un record literal :

```
{ S1, S2, ..., Sn, A1, A2, ..., Ak }
```

où :

- `Si` sont des spreads `...Ei`
- `Aj` sont des assignments `x = ej`

Alors :

1. Construire la liste des spreads :

```
B = E1
B = spread(B, E2)
...
B = spread(B, En)
```

2. Construire la liste des overrides :

```
O = list(A1, A2, ..., Ak)
```

3. Résultat final :

```
spread(B, O)
```

Optimisations :

- si `n = 0` → `list(A1, ..., Ak)`
- si `k = 0` et `n = 1` → `E1`
- si `k = 0` et `n > 1` → `spread(spread(...), ...)`

---

# 8. 🧪 **Exemples complets**

## 8.1. Exemple 1

TypR :

```typ
let cfg = { a = 1, b = 2 }
let new = { ...cfg, b = 10 }
```

R :

```r
cfg <- list(a = 1, b = 2)
new <- spread(cfg, list(b = 10))
```

---

## 8.2. Exemple 2 — row‑polymorphism

TypR :

```typ
fun f(x: { a: int | r }) {
  { ...x, a = x.a + 1 }
}
```

R :

```r
f <- function(x) {
  spread(x, list(a = x$a + 1))
}
```

---

# 9. 📌 **Garantie de compatibilité TypR**

Cette spécification garantit :

- que TypR n’a **pas besoin de connaître tous les champs** d’un record  
- que les champs inconnus sont **préservés**  
- que le spread fonctionne même dans des fonctions **row‑polymorphiques**  
- que la sémantique est **déterministe** et **identique à JavaScript** (shallow merge)  

---

# 10. 🔧 **Extensions possibles**

- **spread_checked** : vérification de champs obligatoires  
- **deep_spread** : merge récursif optionnel  
- **omit** : suppression de champs  
- **pick** : sélection de champs  

---

# 🎁 Conclusion

Tu as maintenant une **spécification complète**, prête à être intégrée dans :

- la documentation TypR  
- la standard library  
- le transpileur  
- les tests unitaires  

Si tu veux, je peux maintenant rédiger :

- une **RFC formelle** (style Rust RFC / TypeScript spec)  
- une **implémentation complète dans le transpileur TypR**  
- une **section de documentation utilisateur**  
- une **page “Design Rationale”** expliquant les choix  

Tu veux laquelle en premier ?
