# RFC : Constructeur structurel `Self:{ ... }` pour TypR

## 1. Objectif

**But de la fonctionnalité**

Introduire une syntaxe de construction structurelle basée sur le type concret du **premier paramètre** d’une fonction :

```typR
Self:{ field1 = expr1, ...value }
```

où `Self` désigne **le type concret** du premier paramètre de la fonction (ou méthode), permettant :

- de reconstruire une valeur du **même type concret** sans connaître son nom,
- de rester **100 % structurel** (pas de dépendance nominale),
- de s’intégrer avec :
  - les `list { ... }`,
  - l’`embedding`,
  - le mécanisme de **copy-on-modify**.

---

## 2. Motivation

### 2.1. Problème

On considère :

```typR
type Truc <- list { truc: int };

let f <- fn(a: Truc): Truc {
    // on veut retourner une valeur du même type concret que `a`
};
```

`Truc` est un type **structurel** : « toute `list` ayant au moins `truc: int` ».

Dans le corps de `f` :

- le type concret de `a` peut être :
  - `list { truc: int }`
  - `list { truc: int, x: string }`
  - `list { truc: int, y: bool, z: float }`
  - une `list` embarquée, raffinée, etc.
- on veut **reconstruire** une valeur du **même type concret** que `a`,
- sans connaître ce type nominalement,
- sans passer explicitement un constructeur.

### 2.2. Limites des approches naïves

- **Constructeur explicite** : `fn(a: Truc, ctor: Constructor<Truc>)`  
  → verbeux, non implicite, ne se propage pas bien via l’`embedding`.

- **Pattern matching nominal** : incompatible avec un système purement structurel.

- **Introspection dynamique** : non typé, lent, casse la pureté.

### 2.3. Solution proposée

Introduire une syntaxe :

```typR
Self:{ field = expr, ...a }
```

où :

- `Self` désigne **le type concret** du premier paramètre de la fonction,
- la construction est **structurelle**,
- la sémantique repose sur :
  - un **constructeur implicite** associé à `Self`,
  - le mécanisme de **copy-on-modify**.

---

## 3. Spécification informelle

### 3.1. Syntaxe

Nouvelle forme d’expression :

```typR
Self:{ field1 = expr1, field2 = expr2, ...base }
```

où :

- `Self` est un mot-clé **réservé** dans le corps d’une fonction,
- `field_i = expr_i` sont des assignations de champs,
- `...base` est une expression de type `Self` (ou compatible structurellement).

#### 3.1.1. Contexte de validité

`Self` est **valide** uniquement dans :

- le corps d’une fonction ou méthode,
- qui possède au moins **un paramètre** (nommé ou implicite),
- pour lequel on peut déterminer un **type concret** (voir 4.1).

---

### 3.2. Sémantique intuitive

Dans une fonction :

```typR
fn(a: T, b: U): R {
    Self:{ x = 1, ...a }
}
```

- `Self` est résolu comme le **type concret** de `a`,
- `Self:{ x = 1, ...a }` signifie :
  - « construis une nouvelle valeur du même type que `a`,  
    en partant de `a`, mais avec `x` remplacé par `1`. »

---

## 4. Règles de typage

### 4.1. Résolution de `Self`

**Règle 1 : paramètre porteur de Self**

Par défaut, `Self` désigne le type du **premier paramètre** de la fonction :

```typR
fn(a: A, b: B): R {
    // Self == A
}
```

**Règle 2 : méthodes / syntaxes sugar**

Pour une méthode :

```typR
impl Character {
    move(self, dir) {
        Self:{ coords = move(self.coords, dir), ...self }
    }
}
```

- `Self` désigne le type concret de `self`.

**Règle 3 : polymorphisme structurel**

Si le premier paramètre a un type structurel :

```typR
fn(a: { x: int, y: int }): { x: int, y: int } {
    Self:{ x = a.x + 1, ...a }
}
```

- `Self` désigne **le type concret** de `a` au point d’appel,
- mais le typage statique voit `Self` comme le type structurel minimal connu.

---

### 4.2. Typage de `Self:{ ... }`

On considère :

```typR
Self:{ f1 = e1, ..., fn = en, ...base }
```

**Conditions de typage :**

1. `Self` doit être résolu (cf. 4.1).
2. `base` doit être typé comme `Self` (ou un type structurel compatible).
3. Pour chaque champ `fi` :
   - `fi` doit être un champ valide de `Self`,
   - `ei` doit être typable dans le type du champ `fi` dans `Self`.

**Type de l’expression :**

- L’expression `Self:{ ... }` a le type `Self`.

---

## 5. Désucrage et sémantique opérationnelle

### 5.1. Constructeur implicite de `Self`

On introduit le concept de **constructeur structurel implicite** :

Pour tout type `T` utilisé comme `Self`, on suppose l’existence d’une capacité :

```typR
type SelfConstructor<T> <- {
    new: fn(fields: T): T
};
```

Et un champ implicite (conceptuel) :

```typR
embed __ctor: SelfConstructor<Self>
```

> Remarque : ce champ peut être purement conceptuel / généré par le compilateur,  
> pas nécessairement visible dans le langage utilisateur.

### 5.2. Désucrage de `Self:{ ... }`

Expression source :

```typR
Self:{ f1 = e1, ..., fn = en, ...base }
```

Désucrage :

```typR
let tmpBase = base;
let fields = tmpBase with { f1 = e1, ..., fn = en }; // mise à jour structurelle
tmpBase.__ctor.new(fields)
```

où :

- `tmpBase with { ... }` est une opération de **mise à jour structurelle** (record update),
- `__ctor.new` reconstruit une valeur du type concret de `tmpBase`.

### 5.3. Intégration avec copy-on-modify

La mise à jour :

```typR
tmpBase with { f1 = e1, ..., fn = en }
```

est implémentée via **copy-on-modify** :

- les champs non modifiés restent **partagés**,
- seuls les champs modifiés sont copiés / remplacés,
- la complexité est **O(k)** où `k` = nombre de champs modifiés.

Le constructeur `__ctor.new` ne fait que « re-emballer » la structure, sans copie supplémentaire.

---

## 6. Exemples

### 6.1. Exemple simple avec record

```typR
type Point <- { x: int, y: int };

let translateX <- fn(p: Point, dx: int): Point {
    Self:{ x = p.x + dx, ...p }
};
```

- `Self` = `Point`,
- `Self:{ x = p.x + dx, ...p }` → nouvelle valeur de type `Point`,
- `y` est partagé via copy-on-modify.

---

### 6.2. Exemple avec type structurel

```typR
type HasTruc <- { truc: int };

let incrTruc <- fn(a: HasTruc): HasTruc {
    Self:{ truc = a.truc + 1, ...a }
};
```

À l’appel :

```typR
let x: list { truc: int, name: string } <- ...;

let y <- incrTruc(x);
// y a le même type concret que x : list { truc: int, name: string }
```

- statiquement, `Self` ≈ `{ truc: int }`,
- dynamiquement, `Self` est le type concret de `x`,
- `name` est préservé automatiquement.

---

### 6.3. Exemple avec `list { ... }`

```typR
type Truc <- list { truc: int };

let bump <- fn(a: Truc): Truc {
    Self:{ truc = a.truc + 1, ...a }
};
```

Si `a` est de type :

```typR
list { truc: int, extra: bool }
```

alors :

- `Self` = `list { truc: int, extra: bool }`,
- `bump(a)` retourne une `list { truc: int, extra: bool }`.

---

### 6.4. Exemple avec embedding

```typR
type Position <- { x: int, y: int };

type Character <- {
    embed coords: Position,
    name: string
};

impl Character {
    move(self, dx: int, dy: int) {
        Self:{
            coords = move(self.coords, dx, dy),
            ...self
        }
    }
}
```

- `Self` = `Character`,
- `coords` est mis à jour,
- `name` est préservé automatiquement.

---

## 7. Interactions avec les autres fonctionnalités

### 7.1. Polymorphisme et contraintes

`Self` peut être utilisé dans des fonctions génériques :

```typR
fn mapTruc<T <- { truc: int }>(a: T, f: fn(int): int): T {
    Self:{ truc = f(a.truc), ...a }
}
```

- `Self` = `T`,
- le type concret de `T` est préservé.

---

### 7.2. Intersections / traits structurels

Avec des traits structurels :

```typR
type HasTruc <- { truc: int };
type HasName <- { name: string };

fn renameAndIncr<T <- HasTruc & HasName>(a: T, newName: string): T {
    Self:{
        truc = a.truc + 1,
        name = newName,
        ...a
    }
}
```

- `Self` = `T`,
- toutes les propriétés supplémentaires de `T` sont préservées.

---

### 7.3. Refined types

Pour un type raffiné :

```typR
type PositivePoint <- { x: int, y: int } where x > 0 && y > 0;

fn normalize(p: PositivePoint): PositivePoint {
    // ... calcul ...
    Self:{ x = newX, y = newY, ...p }
}
```

- `Self` = `PositivePoint`,
- les invariants peuvent être vérifiés à la sortie (par le système de refinements).

---

## 8. Erreurs et diagnostics

### 8.1. `Self` hors contexte

```typR
let x <- Self:{ x = 1 }; // ❌
```

Erreur :  
**`Self` n’est pas défini dans ce contexte (pas de paramètre porteur de Self).**

---

### 8.2. Champ inexistant

```typR
fn f(a: { x: int }) {
    Self:{ y = 1, ...a } // ❌
}
```

Erreur :  
**Le champ `y` n’existe pas dans le type `Self`.**

---

### 8.3. Type incompatible

```typR
fn f(a: { x: int }) {
    let b: { x: string } <- ...;
    Self:{ x = b.x, ...a } // ❌
}
```

Erreur :  
**Type de `x` incompatible : attendu `int`, trouvé `string`.**

---

## 9. Implémentation suggérée

### 9.1. Phases

1. **Résolution de `Self`** pendant la phase de typage.
2. **Vérification des champs** (`fi`) et de leurs types.
3. **Désucrage** en :
   - mise à jour structurelle (`with { ... }`),
   - appel au constructeur implicite (`__ctor.new`).
4. **Génération de code** optimisée avec copy-on-modify.

### 9.2. Optimisations

- Inline de `__ctor.new` pour éviter tout appel indirect.
- Représentation interne de `Self:{ ... }` comme une primitive de mise à jour de record.
- Partage maximal des champs non modifiés.

---

## 10. Résumé

Cette RFC introduit :

- un mot-clé **`Self`** dans le corps des fonctions,
- une syntaxe de construction :

  ```typR
  Self:{ field = expr, ...base }
  ```

- qui permet de :
  - reconstruire une valeur du **même type concret** que le premier paramètre,
  - rester **structurel** et générique,
  - s’intégrer naturellement avec :
    - `list { ... }`,
    - l’`embedding`,
    - le **copy-on-modify**,
    - les traits structurels,
    - les refined types.
