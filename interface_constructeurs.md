# RFC — Constructeurs d’interface en tant que validateurs structurels  
**Version**: 1.0  
**Statut**: Draft  
**Auteur**: Fabrice & Copilot  
**Domaine**: TypR — Système de types, interfaces, compilation  
**Objectif**: Définir la sémantique, la syntaxe et le rôle des constructeurs d’interface dans TypR.

---

## 1. Motivation  
TypR repose sur un **typage structurel**, une **composition par alias**, et une **inférence comportementale**.  
Les interfaces définissent des **contrats comportementaux** (fonctions requises), mais ne sont pas des classes et ne doivent pas introduire de nominalisme.

Pour que :

- les **alias** puissent combiner des interfaces,  
- les **types dérivés** puissent être validés,  
- les **DSL fluents** puissent s’appuyer sur des comportements,  
- les **pipelines typés** puissent vérifier les capacités d’un type,  

il faut un mécanisme permettant de **valider qu’un type satisfait une interface**.

Ce mécanisme est le **constructeur d’interface**, qui n’instancie rien :  
> **il valide structurellement qu’un type implémente les fonctions requises.**

---

## 2. Définition  
### 2.1. Interface  
Une interface est un type structurel contenant des **signatures de fonctions** :

```
interface Movable {
    move(self: Self, dx: number, dy: number) -> Self
}
```

### 2.2. Interface construite à la volée  
TypR permet de construire une interface dynamiquement :

```
type Movable <- interface {
    move: (self: Self, dx: number, dy: number) -> Self
};
```

Les deux formes sont strictement équivalentes.

---

## 3. Constructeur d’interface  
### 3.1. Constructeur implicite  
Pour chaque interface `I`, TypR génère automatiquement un constructeur :

```
fn I(x) {
    assert has_fn(x, "f1")
    assert has_fn(x, "f2")
    ...
    return x
}
```

où `f1`, `f2`, … sont les fonctions requises par l’interface.

### 3.2. Nature du constructeur  
Le constructeur d’interface :

- **ne crée pas d’instance**  
- **ne normalise pas les champs**  
- **ne transforme pas la valeur**  
- **ne fait que valider** le contrat comportemental  
- **échoue au transpilateur** si le contrat n’est pas respecté  

C’est un **validateur structurel**, pas un constructeur au sens OO.

---

## 4. Sémantique de validation  
### 4.1. Règle de validation  
Pour un type `T` et une interface `I`, la validation est :

```
I(T)  // appel implicite du constructeur
```

Le transpilateur vérifie :

- que `T` possède toutes les fonctions requises par `I`  
- que les signatures sont compatibles (contravariance de `self`, covariance du retour)  
- que les types des paramètres sont compatibles structurellement  

### 4.2. Erreur de compilation  
Si une fonction requise est absente ou incompatible :

```
error: Type T does not implement interface I
missing function: move(self, dx, dy)
```

---

## 5. Alias et composition  
### 5.1. Alias basé sur une interface  
```
type MovablePoint <- Movable & {
    x: number
    y: number
};
```

Lors de la compilation :

```
Movable({ x, y })
```

→ Le transpilateur vérifie que `{x, y}` possède `move`.  
→ Sinon : erreur.

### 5.2. Intersection d’interfaces  
```
type DrawableMovable <- Movable & Drawable;
```

Le constructeur implicite devient :

```
fn DrawableMovable(x) {
    Movable(x)
    Drawable(x)
    return x
}
```

---

## 6. Interfaces génériques  
Les interfaces peuvent être génériques :

```
interface Container<T> {
    push(self: Self, value: T) -> Self
}
```

Le constructeur valide :

- la présence de `push`  
- la compatibilité du paramètre `value: T`  
- la compatibilité du retour `Self`

---

## 7. Interfaces construites à la volée  
### 7.1. Syntaxe  
```
type Movable <- interface {
    move: (self: Self, dx: number, dy: number) -> Self
};
```

### 7.2. Constructeur généré  
```
fn Movable(x) {
    assert has_fn(x, "move")
    return x
}
```

### 7.3. Cas d’usage  
- DSL fluents  
- APIs d’animation  
- comportements dynamiques  
- types dérivés  
- pipelines typés  

---

## 8. Règles de typage  
### 8.1. Compatibilité des signatures  
Une fonction `f` satisfait une signature d’interface si :

- les paramètres sont **contravariants**  
- le retour est **covariant**  
- `self` est compatible avec `Self`  

### 8.2. Self  
`Self` est résolu comme le type concret passé au constructeur.

---

## 9. Erreurs et diagnostics  
### 9.1. Fonction manquante  
```
error: Type T does not implement interface Movable
missing function: move(self, dx, dy)
```

### 9.2. Signature incompatible  
```
error: Function move has incompatible signature
expected: (self, dx: number, dy: number) -> Self
found:    (self, dx: string, dy: number) -> Self
```

---

## 10. Exemples complets  
### 10.1. Interface + alias

```
interface Printable {
    print(self) -> string
}

type LabeledPoint <- Printable & {
    x: number
    y: number
    label: string
}
```

Validation :

```
Printable({ x, y, label })
```

→ Erreur si `print` n’existe pas.

---

## 11. Résumé  
> Les constructeurs d’interface dans TypR sont des **validateurs structurels** générés automatiquement.  
> Ils vérifient que les types satisfont les contrats comportementaux, et échouent à la compilation si ce n’est pas le cas.  
> Ils permettent l’aliasing, la composition, les interfaces dynamiques, les DSL fluents, et les pipelines typés — tout en restant fidèles au typage structurel de TypR.
