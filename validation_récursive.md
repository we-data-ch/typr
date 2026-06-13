# 🔁 **Spécification formelle : Validation récursive des champs dans TypR**

## 1. Objectif

Cette spécification étend le pipeline `T(...) → as.T(...) → validate_T(...) → validate.T(...)`
(voir `constructor_validator.md`) au cas des **champs dont le type est lui-même un type
validable** (alias record, alias primitif, variant d'union).

État actuel : pour un champ `f: Foo`, `validate_T` se contente d'un contrôle **nominal**
superficiel :

```r
if (!inherits(x[["f"]], "Foo")) stop("... field 'f' must be of class Foo")
```

Ce contrôle vérifie la **classe** de `x[["f"]]` mais **ne descend pas** dans la structure
interne du sous-objet. Un objet portant la bonne classe mais des invariants internes violés
passe la validation du parent.

But : garantir que **la validité d'un objet implique la validité récursive de tous ses
sous-objets**.

---

## 2. Principe

Le validateur interne `validate_T` doit, pour chaque champ, choisir le contrôle adapté au
**type déclaré du champ** :

| Type du champ `f` | Contrôle généré |
|-------------------|-----------------|
| primitif (`int`, `num`, `char`, `bool`) | `inherits(x[["f"]], "<classe-R>")` |
| alias primitif (`type M <- int`) | `inherits(x[["f"]], "<classe-sous-jacente>")` |
| **alias record** (`type Foo <- list{…}`) | contrôle nominal **puis** `validate_Foo(x[["f"]])` |
| **variant / union** | `validate_<Variant>(x[["f"]])` (cf. `validation_variant_d_union.md`) |
| générique / fonction / autre | présence seule |

La règle nouvelle ne concerne que les deux lignes en gras : les champs dont le type **possède
son propre validateur** sont validés **par délégation** à ce validateur.

---

## 3. Sémantique opérationnelle

### 3.1 Champ de type alias record

Pour `type T <- list{ …, f: Foo, … }` où `type Foo <- list{ … }` :

```
validate_T(x) ≜
    … (contrôle de présence des champs) …
    if !inherits(x.f, "Foo") then error("field f must be of class Foo")
    x.f ← validate_Foo(x.f)        // délégation récursive, structurelle
    …
    x
```

### Propriétés

- **Délégation au validateur _interne_** `validate_Foo`, **jamais** à `validate` (générique
  utilisateur) ni à `as.Foo`. Justification :
  - le sous-objet a déjà traversé `as.Foo → validate_Foo → validate.Foo` lors de **sa propre
    construction** ; rejouer `validate.Foo` (métier, potentiellement à effets) dans le parent
    serait redondant et surprenant ;
  - `as.Foo` **ré-annoterait** la classe : inutile, le sous-objet est déjà annoté.
- `validate_Foo` est **pur** : `x.f ← validate_Foo(x.f)` ne modifie pas structurellement `x`.
  L'affectation est conservée par symétrie avec le reste du pipeline (un futur `validate_Foo`
  enrichi pourrait normaliser le sous-objet).
- L'invariant « le validateur interne est pur et structurel » est **préservé**.

### 3.2 Contrôle nominal préalable

Le contrôle `inherits(x.f, "Foo")` est conservé **avant** la délégation, car le sous-typage de
TypR est **structurel** : un record distinct possédant les mêmes champs satisfait
`validate_Foo` mais n'est pas nominalement un `Foo`. L'ordre est donc :

```
1. présence de f
2. inherits(x.f, "Foo")     — garantie nominale
3. validate_Foo(x.f)        — garantie structurelle profonde
```

### 3.3 Terminaison

La récursion suit la **structure de la valeur**, qui est finie. Pour les types récursifs
(p. ex. un arbre `type Tree <- list{ value: int, children: Option<Tree> }`), la descente
s'arrête sur le cas de base (`.None`, `NULL`, liste vide). Aucune borne artificielle n'est
nécessaire : il n'existe pas de valeur de profondeur infinie.

> ⚠️ Un type **mutuellement récursif** (`A` contient `B`, `B` contient `A`) suit la même
> garantie tant que la valeur est finie. La génération du *code* des validateurs n'est, elle,
> jamais récursive (chaque `validate_X` est émis une seule fois pour l'alias `X`).

---

## 4. Règles d'inférence

### 4.1 Validation d'un champ délégable

```
Γ ⊢ f : Foo     Foo est un alias record     validate_Foo défini
───────────────────────────────────────────────────────────────
Γ ⊢ champ f de validate_T  ⟹  inherits(x.f, "Foo") ; validate_Foo(x.f)
```

### 4.2 Propagation de validité

```
validate_T(x) = x      ∀ champ f : A,  validate_A(x.f) = x.f
──────────────────────────────────────────────────────────────
x est récursivement valide
```

---

## 5. Pipeline complet (formel)

```
T(...)
    ↳ as.T(...)
        ↳ validate_T(...)
            ↳ pour chaque champ f : Foo  →  validate_Foo(x.f)
                ↳ pour chaque champ g : Bar  →  validate_Bar(x.f.g)
                    ↳ …
            ↳ validate.T(...)            (métier, niveau racine uniquement)
```

### Propriété fondamentale

> **Un objet de type `T` est valide si et seulement si tous ses sous-objets le sont
> structurellement, à toute profondeur.**

---

## 6. Exemple complet

### Déclaration TypR

```typr
type Address <- list { city: char, zip: int };
type Person  <- list { name: char, age: int, home: Address };
```

### Généré (extrait pertinent)

```r
validate_Address <- function(x) {
  required_fields <- c("city", "zip")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) { stop(paste0("Validation failed for type Address: missing fields: ", paste(missing_fields, collapse = ", "))) }
  if (!inherits(x[["city"]], "character")) stop("Validation failed for type Address: field 'city' must be of class character")
  if (!inherits(x[["zip"]],  "integer"))   stop("Validation failed for type Address: field 'zip' must be of class integer")
  x
}

validate_Person <- function(x) {
  required_fields <- c("age", "home", "name")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) { stop(paste0("Validation failed for type Person: missing fields: ", paste(missing_fields, collapse = ", "))) }
  if (!inherits(x[["age"]],  "integer"))   stop("Validation failed for type Person: field 'age' must be of class integer")
  if (!inherits(x[["name"]], "character")) stop("Validation failed for type Person: field 'name' must be of class character")
  # champ de type alias record → délégation récursive
  if (!inherits(x[["home"]], "Address"))   stop("Validation failed for type Person: field 'home' must be of class Address")
  x[["home"]] <- validate_Address(x[["home"]])
  x
}
```

---

## 7. Impact d'implémentation

- **Fichier** : `crates/typr-core/src/processes/transpiling/mod.rs`, bras
  `Lang::Alias` / `Type::Record`.
- **Helper existant** : `record_field_class(typ, cont)` calcule déjà la classe d'un champ.
  Ajouter un helper jumeau (ou étendre celui-ci) qui distingue **« alias record »** (→ émettre
  `inherits` + `validate_<A>`) de **« primitif / alias primitif »** (→ `inherits` seul).
- **Ordre d'émission** : conserver le bloc de présence, puis les contrôles `inherits`, puis les
  délégations `x[["f"]] <- validate_<A>(x[["f"]])`.
- **Aucune** régénération `.bin`, **aucune** modification de `std.R` : tout est généré par champ
  à la transpilation.
- **Tests** : étendre les snapshots `type_alias_record` avec un cas imbriqué ; ajouter un test
  inline `FluentParser` vérifiant la présence de `validate_Address(x[["home"]])` dans
  `validate_Person`.

---

## 8. Décisions ouvertes

1. **Faut-il garder le contrôle `inherits` nominal** en plus de la délégation, ou la délégation
   seule suffit-elle ? (Recommandé : garder les deux — nominal + structurel — cf. §3.2.)
2. **Réaffectation `x[["f"]] <- validate_…`** ou simple appel `validate_…(x[["f"]])` sans
   réaffecter ? (Recommandé : réaffecter, pour rester homogène avec un futur validateur
   normalisant, même si aujourd'hui `validate_…` est l'identité structurelle.)
3. **Champs optionnels** (`f: Option<Foo>`) : la délégation doit être gardée par un test
   `.Some/.None` avant de descendre. À spécifier conjointement avec
   `validation_variant_d_union.md`.

---

## 🧭 Conclusion

La validation récursive transforme la garantie **locale** (« les champs ont la bonne classe »)
en garantie **globale** (« l'objet est valide à toute profondeur »), en réutilisant les
validateurs internes déjà générés, sans recourir au validateur métier ni ré-annoter les
sous-objets. Elle est le complément naturel du système constructeur/annotateur/validateur.
