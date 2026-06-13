# 🔀 **Spécification formelle : Constructeurs et validateurs des variants d'union dans TypR**

## 1. Objectif

Étendre le pipeline `T(...) → as.T(...) → validate_T(...) → validate.T(...)`
(voir `constructor_validator.md`) aux **variants d'un type union**.

Aujourd'hui, pour `type Shape <- .Circle(num) | .Square(num)`, chaque variant reçoit un
constructeur **direct** qui pose la classe et ne valide pas :

```r
Circle <- function(x) {
  structure(list(x), class = c("Circle", "Shape", "list"))
}
```

But : que chaque variant bénéficie exactement du même contrat
**constructeur / annotateur / validateur interne / validateur utilisateur** que les records,
et que la **validité d'une valeur d'union** soit garantie au niveau du variant **et** au niveau
de l'union.

---

## 2. Le problème préalable : deux représentations runtime

TypR encode actuellement un tag de **deux façons incohérentes** :

| Origine | Code R généré | Classe |
|---------|---------------|--------|
| Littéral `.Circle(3.14)` (`Lang::Tag`) | `list('Circle', body = 3.14)` | `c('.Circle', 'Tag')` |
| Constructeur de variant `Circle(3.14)` | `list(3.14)` | `c('Circle', 'Shape', 'list')` |

Ces deux formes **ne sont pas interchangeables** : l'une porte le nom du tag en position 1 et
le payload sous `body`, l'autre porte le payload en position 1 sans nom de tag. Le validateur
de `Type::Tag` lit `x[[1]]` (nom) et `x[["body"]]` (payload), donc seul le **premier** encodage
est validable en l'état.

> **Prérequis non négociable** : avant d'ajouter le pipeline, les deux origines doivent
> **converger vers une représentation canonique unique**. C'est la décision centrale de cette
> spec (cf. §7).

### Représentation canonique recommandée

```
.Circle(p)  ≜  structure(list("Circle", body = p),
                         class = c("Circle", "Shape", "Tag", "list"))
```

- **Identité du tag en position 1** (`x[[1]] == "Circle"`) → le filtrage par motif (`match`)
  et le validateur existant continuent de lire `x[[1]]` / `x[["body"]]` **sans changement**.
- **Classe enrichie sans point** `c("Circle", "Shape", "Tag", "list")` → permet le dispatch S3
  de `validate` au niveau **variant** (`validate.Circle`) puis **union** (`validate.Shape`),
  et supprime le `.Circle` à point initial qui produirait des méthodes `validate..Circle`.
- Le variant **vide** `.Nothing` ⟹ `list("Nothing")` (pas de `body`).

---

## 3. Syntaxe

```typr
type Shape <- .Circle(num) | .Square(num) | .Nothing
```

génère, pour **chaque** variant `V` d'union `U` :

- un constructeur `V(...)`
- un annotateur `as.V(...)`
- un validateur interne `validate_V(...)`
- un point d'extension utilisateur `validate.V(...)` (et, au niveau union, `validate.U(...)`)

---

## 4. Sémantique opérationnelle

### 4.1 Constructeur `V(...)`

```
Circle(p)  ≜  let x = list("Circle", body = p) in as.Circle(x)
Nothing()  ≜  let x = list("Nothing")          in as.Nothing(x)
```

Comme pour les records : **construit seulement**, délègue à l'annotateur, n'ajoute ni classe
ni validation.

### 4.2 Annotateur `as.V(x)`

```
as.Circle(x) ≜
    let y = if !inherits(x, "Circle") then class(x) ← c("Circle","Shape","Tag","list")
    let z = validate_Circle(y)
    in validate(z)
```

- Pose idempotente de la classe (variant + union + `Tag` + `list`).
- Seul point qui annote et qui déclenche la validation.

### 4.3 Validateur interne `validate_V(x)`

Vérifie les invariants **structurels** du variant :

```
validate_Circle(x) ≜
    if x[[1]] ≠ "Circle"          then error("expected tag 'Circle'")
    if is.null(x.body)            then error("missing payload")
    if !inherits(x.body, "numeric") then error("payload must be of class numeric")
    x
```

- **Pur**, non redéfinissable.
- Si le payload est un **alias record / un autre variant**, déléguer à son validateur
  (`validate_<Alias>(x.body)`) — cf. `validation_récursive.md`.
- Variant vide : on vérifie seulement `x[[1]] == "Nothing"` et l'absence de payload.

### 4.4 Validateurs utilisateur `validate.V` et `validate.U`

Grâce à la classe `c("Circle", "Shape", "Tag", "list")`, le générique `validate` dispatche :

```
validate(x)  →  validate.Circle  si défini
             →  sinon validate.Shape   (niveau union)
             →  sinon validate.default = identité
```

- `validate.Circle` : règles métier propres au variant.
- `validate.Shape`  : règles métier communes à **toute** valeur de l'union.
- Pour exécuter **les deux** (variant puis union), `validate.Circle` appelle `NextMethod()`
  (S3 ne déclenche qu'une seule méthode par défaut).

Définition côté utilisateur, via le mécanisme S3 existant (dispatch sur le 1ᵉʳ paramètre) :

```typr
let validate <- fn(x: Circle): Circle { … };   // → validate.Circle
let validate <- fn(x: Shape):  Shape  { … };   // → validate.Shape
```

---

## 5. Règles d'inférence

### 5.1 Construction

```
Γ ⊢ p : num
─────────────────────────────
Γ ⊢ Circle(p) : Shape
```

### 5.2 Annotation

```
Γ ⊢ x : list("Circle", body = num)
─────────────────────────────────────
Γ ⊢ as.Circle(x) : Shape
```

### 5.3 Validité d'union

```
x : Shape    x[[1]] = "Circle"    validate_Circle(x) = x
────────────────────────────────────────────────────────
x est un Shape valide (variant Circle)
```

---

## 6. Pipeline complet (formel)

```
Circle(p)
    ↳ as.Circle(p)
        ↳ class ← c("Circle","Shape","Tag","list")   (idempotent)
        ↳ validate_Circle(...)            // tag + payload (+ récursif)
            ↳ validate.Circle(...)        // métier variant
                ↳ NextMethod → validate.Shape(...)   // métier union (optionnel)
                    ↳ résultat final
```

### Propriété fondamentale

> **Toute valeur d'un type union est garantie : (1) bien formée pour son variant,
> (2) valide structurellement en profondeur, (3) conforme aux règles métier du variant
> puis de l'union.**

---

## 7. Décisions ouvertes

1. **Représentation canonique** (décision centrale, §2) :
   - **(A, recommandée)** garder le nom du tag en `x[[1]]` + enrichir la classe
     (`c("Variant","Union","Tag","list")`). Churn minimal : `match` et le validateur
     existant restent valides ; il « suffit » d'aligner le constructeur de variant et le
     littéral `Lang::Tag` sur cette forme.
   - **(B)** porter l'identité du tag **uniquement** dans la classe et supprimer `x[[1]]`.
     Plus pur, mais oblige à réécrire le filtrage par motif (`pattern_to_condition`) et le
     validateur de tag.
2. **`validate.U` au niveau union** : l'autorise-t-on dès maintenant, ou variant seul d'abord ?
   (Dépend de la convention `NextMethod`.)
3. **Variants partagés entre unions** : un même nom de variant (`.None`) réutilisé dans
   plusieurs unions — faut-il préfixer la classe par l'union pour éviter la collision des
   méthodes `validate.None` ? À trancher.
4. **Payload multiple** (`.Point(int, int)`) vs payload record (`.Point:{x:int,y:int}`) :
   harmoniser la place du payload (`body` unique vs champs nommés) avec
   `validation_récursive.md`.

---

## 8. Impact d'implémentation

- **Fichier** : `crates/typr-core/src/processes/transpiling/mod.rs`.
  - Bras `Lang::Alias` / `Type::Operator(Union…)` (lignes ~933-996) : remplacer les
    constructeurs directs `structure(list(...), …)` par le triplet
    constructeur+annotateur+validateur, comme le bras `Type::Record`.
  - Bras `Lang::Tag` (~832) : aligner le littéral sur la représentation canonique (§2).
  - Bras `Type::Tag` du validateur (~1048-1096) : déjà proche ; réutiliser pour `validate_V`.
- **`std.R`** : le générique `validate` + `validate.default` existent déjà (ajoutés à
  l'étape 2 records) — **rien à ajouter**.
- **Cohérence `match`** : vérifier que `pattern_to_condition` reste compatible avec la
  représentation canonique retenue (option A : oui ; option B : réécriture nécessaire).
- **Tests** : snapshots union (constructeurs + `as.V` + `validate_V`), test inline d'un
  `match` sur valeur construite via le nouveau pipeline, test de dispatch
  `validate.Circle` / `validate.Shape`.

---

## 🧭 Conclusion

Unifier les variants d'union sous le même contrat que les records donne à TypR **un seul
modèle de construction/validation** pour tous les types nominaux. Le verrou n'est pas le
pipeline lui-même — déjà éprouvé sur les records — mais la **convergence des deux
représentations runtime de tags** (§2/§7.1), qui doit être tranchée en premier.
