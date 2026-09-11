# Spécifications de correction compilateur

> Deux trous du compilateur `typr`, identifiés le **2026-09-09** en passant les 45 blocs
> ` ```typr noplayground ` de la documentation au compilateur (0.5.10). La doc a raison, le
> compilateur a tort : chaque défaut est documenté plus bas avec un repro minimal exécutable et
> des critères d'acceptation.
>
> Implémentation de référence : **`we-data-ch/typr`**, porte d'entrée `typr case add` — un
> correctif sans cas derrière lui n'a rien qui l'empêche de régresser (voir `cases/README.md`).
> Le suivi côté doc de ces deux points : A et B dans `doc_correction.md`.

---

## A · Types singleton pour `bool` et `num`

**Source.** `docs/reference/types.md:27` (section « Literal types »). Le bloc, qui liste les
types singleton, est présentement `noplayground` parce que la moitié de ses lignes ne compilent
pas. La carte de référence `syntaxe.md:100` liste explicitement les quatre — `3`, `3.14`, `true`,
`"chat"` sont des types valides, plus précis que `int`/`num`/`bool`/`char`.

### Comportement actuel

Les littéraux entiers et chaînes reçoivent bien leur type singleton ; les booléens et les
flottants non — ils sont re-typés `bool` / `num`, d'où une *type error* sur une forme que le
parser accepte :

| Ligne | Résultat |
|---|---|
| `let x: 3 = 3;` | ✅ |
| `let name: "hello" = "hello";` | ✅ |
| `let flag: true = true;` | ❌ `Type error: type true doesn't match type bool` |
| `let x: 3.14 = 3.14;` | ❌ `Type error: type 3.14 doesn't match type num` |

### Comportement attendu

Tout littéral utilisé comme type (type singleton) doit être attribué à la valeur littérale
correspondante :

- `let flag: true = true;` doit compiler sans erreur, avec `flag : true` (singleton) ;
- `let flag: true = false;` doit être rejeté ;
- `let x: 3.14 = 3.14;` doit compiler sans erreur, avec `x : 3.14` (singleton) ;
- `let x: 3.14 = 3.0;` / `let x: 3.14 = 3.141;` doit être rejeté.

### Repro (à poser sous forme de cas `typr case add`, séparément ou réunis)

Cas positifs :

```
let flag: true = true;
let x: 3.14 = 3.14;
```

Cas négatifs (attendus en *type error*) :

```
let flag: true = false;
let x: 3.14 = 3.0;
```

### Critères d'acceptation

1. Les quatre types singleton (`3`, `3.14`, `true`, `"chat"`) se lient à leur littéral.
2. L'inférence de type après liaison rapporte le singleton, pas la base (`true` et non `bool`,
   `3.14` et non `num`).
3. Les cas négatifs échouent au *type checking* (pas à la syntaxe).
4. `npm run check:examples` passe ; le bloc de `docs/reference/types.md:27` redevient
   vérifiable — `noplayground` à retirer.

---

## B · Styles structurels : nom de paramètre de fonction ignoré dans la comparaison de types

**Source.** `docs/reference/functions.md:131` (section « Closures »). Le bloc est présentement
`noplayground` bien que le code qu'il montre soit du TypR valide. C'est le plus net des deux :
une ligne le reproduit, et il mord dès qu'on annote un retour de fonction.

### Comportement actuel

`(int) -> int` se désucre en `fn(a: int) -> int` — paramètre nommé `a` par défaut — et les types
de fonction sont comparés **nom de paramètre compris** :

```
let f: (int) -> int <- fn(a: int): int { a };   # ✅  (le nom devine le désucrage)
let f: (int) -> int <- fn(z: int): int { z };   # ❌
```

> ```
> Type error: The output type of the function don't match it's type annotation
> Expected: fn(a: int) -> int
> Found:    fn(z: int) -> int
> ```

### Comportement attendu

Un type de fonction est **structurel** : sa valeur ne dépend pas du nom des paramètres.
`fn(z: int) -> int` doit être accepté partout où `fn(a: int) -> int` l'est, et
`let f: (int) -> int <- fn(z: int): int { z };` doit compiler sans erreur.

Le nom du paramètre est une commodité d'écriture côté implémentation, pas une information de
type. En l'état, toute annotation de retour de fonction — closure, callback — n'est acceptée que
si l'implémenteur devine le nom choisi par le désucrage.

### Repro

Cas positif :

```
let f: (int) -> int <- fn(z: int): int { z };
```

Également concerné (le même bug en plus long), le bloc complet de la doc :

```
let make_adder <- fn(n: int): (int) -> int {
    fn(x: int): int { x + n }
};

let add5 <- make_adder(5);
add5(3);   # 8
```

Cas négatifs (à conserver : le typage structurel doit rester strict sur les **types**, pas sur
les noms) :

```
let f: (int) -> int <- fn(a: char): int { 1 };          # ❌ type de paramètre différent
let f: (int) -> int <- fn(a: int): char { "x" };        # ❌ type de retour différent
let f: (int, bool) -> int <- fn(a: int): int { a };     # ❌ arité différente
```

### Critères d'acceptation

1. La comparaison de deux types de fonction ignore le nom des paramètres.
2. La comparaison reste stricte sur le type et l'arité des paramètres et sur le type de retour.
3. `npm run check:examples` passe ; le bloc de `docs/reference/functions.md:131` redevient
   vérifiable **sans le toucher** — `noplayground` à retirer.

---

## Rappel de méthode

Pour rejouer un repro localement sans dépôt du compilateur :

```bash
cd "$(mktemp -d)"
printf 'let flag: true = true;\n' > t.ty
typr check t.ty
```

Le premier `typr check` d'un répertoire y écrit `context.json` et `std.ty` (préchargement de la
bibliothèque standard) — c'est normal, et c'est pour ça qu'on travaille dans un répertoire
jetable.

Après correction du compilateur, côté doc :

```bash
npm run check:examples                    # les 180 blocs
npm run check:examples -- --noplayground  # ce qui reste exclu
```