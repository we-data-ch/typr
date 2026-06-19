# 📘 RFC‑00XX — *Named Type Embedding in TypR*

## 1. Objectif

Cette RFC introduit le mécanisme de **type embedding nommé** dans TypR.
Il permet à un type `A` d'embarquer un type `B` via un champ préfixé `embed` :

```
embed fieldName: B
```

et d'hériter automatiquement :

- des **fonctions** définies sur `B`
- avec **forwarding automatique**
- et **reconstruction fonctionnelle** du type `A`

sans héritage nominal, sans classes, sans impl blocks, et sans mutation.

L'objectif est de fournir un mécanisme de **composition comportementale** simple, déterministe et purement fonctionnel.

`embed` est un mot-clé "doux" (reconnu uniquement en tête d'un champ de record,
suivi d'un espace) — il ne réserve aucun caractère sigil (`@` reste disponible pour
d'autres constructions, voir `sigils.md`).

---

## 2. Syntaxe

### 2.1 Déclaration

```
embed fieldName: Type
```

Exemple :

```
type Player <- list {
    embed coords: Position,
    name: char
}
```

### 2.2 Restrictions syntaxiques

- `fieldName` doit être un identifiant valide.
- `Type` doit être un type structurel (voir section 4).
- Un type peut contenir plusieurs champs `embed`.
- Les champs `embed` doivent apparaître dans un record (`list { ... }`).
- Un champ littéralement nommé `embed` (sans espace avant `:`) reste un champ normal :
  `embed` n'est reconnu comme mot-clé que s'il est suivi d'au moins un espace puis
  d'un nom de champ.

---

## 3. Sémantique

### 3.1 Principe général

Pour chaque fonction :

```
let f <- fn(self: B, args...): B { ... };
```

TypR génère automatiquement :

```
let f <- fn(self: A, args...): A {
    A:{ fieldName = f(self$fieldName, args...), ...self }
};
```

où :

- `A` est le type contenant le champ embarqué
- `fieldName` est le champ préfixé `embed`
- `B` est le type embarqué

### 3.2 Forwarding

Le forwarding suit la règle :

```
A.f(self, args...) = B.f(self$fieldName, args...)
```

### 3.3 Reconstruction

La reconstruction est **fonctionnelle**, via le constructeur de `A` et le spread
d'exécution `...self` (voir `spread_operator2.md` / `spread_operator3.md`) :

- aucun champ n'est muté
- un nouveau record est construit par `A:{ ... }`
- les autres champs sont préservés via `...self`
- le champ embarqué est explicitement réécrit avec le résultat de l'appel forwardé,
  qui l'emporte sur la valeur spreadée (l'explicite gagne toujours sur le spread)

---

## 4. Types éligibles à l'embedding

Un type est éligible s'il est **structurellement un record**, c'est‑à‑dire de kind
`Record` (voir `accepts_record_kind` dans `type_arithmetic.rs`) :

- un record
- un refined type basé sur un record *(non supporté en v1, voir §12)*
- un alias résolu en record

Non éligibles :

- types numériques
- types littéraux simples
- unions (sauf si résolues en record)
- fonctions
- types opaques non structurels

---

## 5. Résolution des fonctions

### 5.1 Règle de base

Une fonction `f` définie sur `B` devient une fonction de `A` **si et seulement si** :

- `f` a un premier paramètre `self: B`
- `f` retourne un `B`
- `A` peut reconstruire un `A` à partir d'un `B`

### 5.2 Collisions

Si `A` définit déjà une fonction `f`, alors :

- **erreur de compilation**
- aucune shadowing implicite
- aucune résolution hiérarchique

### 5.3 Multiples embeddings

Si plusieurs champs `embed` fournissent une fonction `f`, alors :

- **erreur de compilation**
- l'utilisateur doit renommer ou supprimer un embedding

---

## 6. Exemple complet

### 6.1 Type embarqué

```
type Position <- list {
    x: int,
    y: int
}

let move <- fn(self: Position, dx: int, dy: int): Position {
    Position:{ x = self$x + dx, y = self$y + dy }
};
```

(Note: a type named `Character` collides with TypR's reserved cast function
`as.Character` for the builtin `char` type — use a different name, e.g. `Player`.)

### 6.2 Type composite

```
type Player <- list {
    embed coords: Position,
    name: char
}
```

### 6.3 Fonction générée automatiquement

```
let move <- fn(self: Player, dx: int, dy: int): Player {
    Player:{ coords = move(self$coords, dx, dy), ...self }
};
```

---

## 7. Interactions avec d'autres systèmes

### 7.1 Refined types

*Non supporté en v1 — les refined types (prédicats `(. > 0)`) n'existent pas encore
dans TypR. Cette section reste prospective :*

```
type Positive <- int & (. > 0)

type Health <- list {
    embed hp: Positive
}
```

Une fois les refined types implémentés, toutes les fonctions de `Positive` devraient
être forwardées vers `Health`.

### 7.2 Intersections

Embedding multiple, déclaré directement dans le même record `list { ... }` :

```
type Player <- list {
    embed coords: Position,
    embed stats: Stats
}
```

Les fonctions de `Position` et `Stats` sont combinées, sauf collisions (§5.3).

*Note : si `Player` est plutôt construit par intersection (`Position & Stats`), le
drapeau `embed` n'est aujourd'hui pas préservé par la fusion de champs
(`merge_record_fields`) — voir §12.3.*

### 7.3 Arithmétique de types

*Non supporté en v1 (voir §12.1).* Les types embarqués génériques sont prospectifs :

```
type Vec2<#N> <- list { x: #N, y: #N }

type Entity<#N> <- list {
    embed pos: Vec2<#N>
}
```

---

## 8. Règles de kinding

### 8.1 Kind de l'embedding

Si `embed fieldName: B` est embarqué dans `A`, alors :

```
kind(A) = Record
```

### 8.2 Compatibilité

L'embedding est valide si :

- `kind(B) = Record`
- `kind(A) = Record`

### 8.3 Fonctions générées

Les fonctions générées ont le même kind que les fonctions de `B`.

---

## 9. Erreurs

### 9.1 Collision de noms

```
error[E-EMBED-001]: function `move` is provided by multiple embeddings
```

### 9.2 Type non structurel

```
error[E-EMBED-002]: cannot embed non-record type `int`
```

### 9.3 Conflit avec une fonction existante

```
error[E-EMBED-003]: function `move` already defined in `Player`
```

---

## 10. Motivation

Le type embedding nommé fournit :

- un mécanisme de composition simple
- une alternative élégante aux traits et classes
- un forwarding automatique sans boilerplate
- une sémantique purement fonctionnelle
- une compatibilité partielle avec les intersections et l'arithmétique de types (voir §12)

Il renforce la philosophie de TypR :
👉 *des outils puissants, structurels, explicites, sans magie.*

---

## 11. Statut

**Implémenté (v1)** — typechecker (`processes/type_checking/embedding.rs`) et parser
(`embed_keyword` dans `processes/parsing/types.rs`).

---

## 12. Scope (v1)

L'implémentation initiale couvre §1 à §6 et §8 à §9, avec les limites suivantes :

### 12.1 Types embarqués monomorphes uniquement

`get_functions_from_type` (le mécanisme de résolution structurelle réutilisé pour
l'embedding) ne fait correspondre que des fonctions dont le premier paramètre a
**exactement** le type embarqué `B`. Une fonction générique (`fn id(x: T): T`) n'est
pas instanciée automatiquement pour `B` — seules les fonctions monomorphes définies
directement sur `B` sont forwardées. L'instanciation générique (§7.3) est reportée à
une v2.

### 12.2 Pas d'interaction avec les refined types

Les refined types (prédicats `(. > 0)`) n'existent pas dans TypR à ce jour. §7.1 est
documentée à titre prospectif uniquement.

### 12.3 Multi-embedding direct, pas via intersection

La détection de collision (§5.3) et le forwarding combiné ne fonctionnent que pour des
champs `embed` déclarés directement dans le même `list { ... }` littéral. Si deux
records portant des champs `embed` sont combinés via `&` (intersection), le drapeau
`is_embedded` n'est pas préservé par `merge_record_fields` — le champ résultant n'est
alors plus considéré comme embarqué.

### 12.4 Collision avec une fonction explicite : un seul sens détecté

Le typechecker de TypR fonctionne en une seule passe séquentielle, et un type doit
être déclaré avant toute fonction qui le référence (`AliasNotFound` sinon). Une
fonction explicite ne peut donc entrer en collision avec un embedding que si elle est
déclarée **après** l'alias qui embarque (E-EMBED-003). L'ordre inverse est
structurellement impossible et n'a donc pas besoin d'être détecté.

### 12.5 Pas de paramètres variadiques

Une fonction de `B` dont un paramètre (autre que `self`) est variadique (`...xs: T`)
n'est pas forwardée en v1 : la génération du forwarding est silencieusement ignorée
pour cette fonction (ni erreur, ni fonction générée). Le forwarding correct d'un
appel variadique est reporté à une v2.
