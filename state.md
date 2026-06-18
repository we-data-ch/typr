# 📘 **RFC‑000X : Système de valeurs partagées — `State<T>`**

## 1. **Résumé**
Cette RFC propose l’introduction d’un type générique `State<T>` dans TypR, permettant de représenter des valeurs partagées dans un langage purement fonctionnel, sans recourir aux environnements mutables de R ni à un modèle orienté‑objet par référence.

`State<T>` est un **conteneur immuable**, typé, versionné, et manipulé via des transformations pures.  
Il constitue la base d’un futur système de réactivité, de dérivation de contexte, ou de gestion d’état structurée.

---

# 2. **Motivation**

TypR est :

- purement fonctionnel  
- typé  
- orienté valeurs (philosophie S3)  
- sans mutation implicite  
- sans environnement dynamique  

Cependant, certains usages nécessitent une **valeur partagée**, par exemple :

- un compteur  
- un contexte d’exécution  
- un état d’animation  
- un état d’application (UI, pipeline, etc.)  
- un cache local  
- un accumulateur  

Les environnements R ne sont pas adaptés :  
mutables, dynamiques, non typés, non traçables.

Les objets R6 ne le sont pas non plus :  
référentiels, mutables, orientés méthodes.

La solution doit être :

- **pure**  
- **typée**  
- **explicite**  
- **composable**  
- **testable**  
- **interopérable** avec un runtime JS minimal  

D’où la proposition : `State<T>`.

---

# 3. **Objectifs**

### 3.1. Objectifs principaux
- Introduire un conteneur générique immuable pour représenter un état.
- Permettre des transformations pures et versionnées.
- Faciliter la composition et la dérivation d’états.
- Préparer un futur système de réactivité ou de signaux.

### 3.2. Non‑objectifs
- Introduire la mutabilité implicite.
- Reproduire les environnements R.
- Introduire un modèle orienté‑objet par référence.
- Ajouter un système de classes ou de méthodes.

---

# 4. **Spécification**

## 4.1. Définition du type
```r
type State<T> = {
  value: T,
  version: Int,
}
```

### Propriétés
- `value` : la valeur encapsulée, de type `T`.
- `version` : entier strictement croissant à chaque transformation.

### Invariants
- `State<T>` est **immuable**.
- `version` augmente à chaque transformation.
- `value` peut être n’importe quel type valide de TypR.

---

## 4.2. Constructeur générique

### Syntaxe
```r
state = <T>(value: T) -> State<T>
```

### Sémantique
- Crée un nouvel état avec `version = 0`.
- Ne réalise aucune mutation.

### Exemple
```r
s = state(10)          # State<Int>
t = state([1,2,3])     # State<List<Int>>
u = state(df{ ... })   # State<DataFrame<...>>
```

---

## 4.3. Mise à jour fonctionnelle

### Fonction `set`
```r
set = <T>(s: State<T>, newValue: T) -> State<T>
```

### Sémantique
- Retourne un nouvel état.
- `version` est incrémenté de 1.

### Exemple
```r
s2 = set(s, 11)
```

---

## 4.4. Transformation pure

### Fonction `map`
```r
map = <A,B>(s: State<A>, f: A -> B) -> State<B>
```

### Sémantique
- Applique `f` à `s.value`.
- Retourne un nouvel état avec `version = s.version + 1`.

### Exemple
```r
s2 = map(s, x -> x * 2)
```

---

## 4.5. Composition d’états

### États imbriqués
`State<T>` étant un conteneur pur, les états peuvent être imbriqués :

```r
nested = state(state(10))
```

### Fonction `flatten`
```r
flatten = <T>(s: State<State<T>>) -> State<T>
```

### Sémantique
- Extrait la valeur interne.
- Conserve la version externe.

---

## 4.6. Dérivation d’état (optionnel mais recommandé)

### Fonction `derive`
```r
derive = <T>(s: State<T>, f: T -> T) -> State<T>
```

### Exemple
```r
local = derive(s, x -> x + 10)
```

---

# 5. **Modèle d’exécution**

### 5.1. Pureté
Toutes les opérations sur `State<T>` sont pures.

### 5.2. Représentation runtime
En JavaScript :

```js
{ value: ..., version: ... }
```

### 5.3. Optimisations possibles
- Comparaison structurelle ou par version.
- Mémorisation des transformations.
- Intégration future avec un système de signaux.

---

# 6. **Cas d’usage**

### 6.1. Contexte d’exécution
```r
ctx = state(Context(...))
```

### 6.2. Animation
```r
frame = state(0)
frame = map(frame, f -> f + 1)
```

### 6.3. Pipeline data
```r
cache = state({})
```

### 6.4. UI réactive (futur)
```r
count = state(0)
count = set(count, count.value + 1)
```

---

# 7. **Comparaison avec alternatives**

| Modèle | Pureté | Typage | Mutabilité | Élégance | Compatibilité TypR |
|--------|--------|--------|------------|----------|---------------------|
| Environnements R | ❌ | ❌ | ✔️ | ❌ | ❌ |
| R6 | ❌ | ❌ | ✔️ | ❌ | ❌ |
| State monad (Haskell) | ✔️ | ✔️ | ❌ | ⚠️ complexe | ⚠️ trop abstrait |
| React‑like state | ✔️ | ✔️ | ❌ | ✔️ | ✔️ |
| **State<T> (proposé)** | **✔️** | **✔️** | **❌** | **✔️** | **✔️** |

---

# 8. **Évolutions futures**

- Système de **signals** (SolidJS‑like)
- Système de **lenses** pour accéder à des sous‑structures
- Stores globaux optionnels
- Intégration avec le runtime JS pour animations
- Dérivation automatique de dépendances (Typst‑like)

---

# 9. **Conclusion**

`State<T>` est :

- minimal  
- typé  
- fonctionnel  
- élégant  
- composable  
- compatible avec l’ADN de TypR  

Il fournit une base solide pour un système d’état moderne, sans sacrifier la pureté ni introduire de mutabilité implicite.
