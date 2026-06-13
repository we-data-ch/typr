# 🎯 Takeaway  
La syntaxe de mutation implicite TypR repose sur un **postfixe `!;`** appliqué à une expression pipeline ou UFC.  
Elle transforme :

```
expr!; 
```

en :

```
lhs <- expr
```

où `lhs` est la variable de tête du pipeline ou de l’UFC.

---

# 📘 Spécification syntaxique de la mutation implicite TypR  
*(Version formelle, style langage)*

---

# 1. 🎭 Objectif de la fonctionnalité  
La syntaxe de mutation implicite permet d’écrire :

```
obj.method()!; 
```

comme si l’objet était modifié “in place”, tout en restant purement fonctionnel.

Elle est strictement **syntaxique** :  
aucune mutation réelle n’est effectuée dans le langage interne.

---

# 2. 🧩 Syntaxe formelle

## 2.1. Ajout au lexique  
On introduit un nouveau token postfixe :

```
MUTATE_POSTFIX ::= '!;'
```

Il s’agit d’un opérateur **postfixe**, de plus haute priorité que `;` mais plus basse que les appels de fonction.

---

## 2.2. Grammaire (EBNF)

### Expressions
```
ExprMutating ::= ExprPipeline MUTATE_POSTFIX
               | ExprUFC MUTATE_POSTFIX
```

### Pipelines
```
ExprPipeline ::= ExprPrimary
               | ExprPipeline '|>' ExprCall
```

### Uniform Function Call
```
ExprUFC ::= ExprPrimary '.' IDENT '(' ArgList? ')'
```

### Règle d’intégration
`ExprMutating` est une forme d’expression complète, mais **ne peut apparaître que comme statement**.

```
Statement ::= ExprMutating
            | Expr
            | Assignment
            | ...
```

---

# 3. 🎯 Détermination du LHS (variable cible)

La mutation implicite nécessite d’extraire la variable “source” du pipeline.

## 3.1. Règle générale  
Soit :

```
E!; 
```

On définit :

```
lhs = head(E)
rhs = E
```

La transformation devient :

```
lhs <- rhs
```

---

# 4. 🔍 Définition formelle de `head(E)`

### 4.1. Pipeline  
```
head(x |> f() |> g()) = x
```

### 4.2. Uniform Function Call  
```
head(obj.method()) = obj
```

### 4.3. Cas interdits  
Si `head(E)` n’est pas une *l-value* valide :

- littéral (`3!;`)
- expression complexe (`(x+1)!;`)
- appel de fonction (`f(x)!;`)
- indexation (`x[1]!;` sauf si tu veux l’autoriser)

→ **Erreur de compilation : `Mutation target is not assignable`**

---

# 5. 🔧 Transformation AST → AST (avant génération R)

### 5.1. Forme d’entrée  
```
Mutate(
    expr = Pipeline(
        head = Var("x"),
        steps = [Call("f"), Call("g")]
    )
)
```

### 5.2. Forme de sortie  
```
Assign(
    lhs = Var("x"),
    rhs = Pipeline(
        head = Var("x"),
        steps = [Call("f"), Call("g")]
    )
)
```

---

# 6. 🛑 Contraintes syntaxiques

## 6.1. `!;` doit terminer une expression  
Valide :

```
x |> f()!; 
```

Invalide :

```
x |> f()!; |> g()
```

## 6.2. `!;` ne peut pas être combiné avec `=` ou `<-`  
Interdit :

```
y <- x |> f()!; 
```

## 6.3. `!;` ne peut pas apparaître dans une expression imbriquée  
Interdit :

```
h(x |> f()!;)
```

---

# 7. 🧪 Exemples validés par la grammaire

### 7.1. Pipeline simple  
```
x |> normalize()!; 
```

→

```
x <- x |> normalize()
```

### 7.2. UFC  
```
obj.translate(dx=1, dy=2)!;
```

→

```
obj <- obj |> translate(dx=1, dy=2)
```

### 7.3. Pipeline + UFC  
```
shape.scale(2) |> rotate(90)!;
```

→

```
shape <- shape |> scale(2) |> rotate(90)
```

---

# 8. 🧱 Cas limites

### 8.1. Mutation sur champ  
Si tu veux l’autoriser :

```
obj.field.method()!; 
```

→

```
obj <- obj |> (x -> x$field |> method())
```

Sinon → erreur.

### 8.2. Mutation sur vecteur immuable  
Tu peux décider :

- soit erreur (`Cannot mutate immutable type`)
- soit autoriser mais avertir

---

# 9. 🧭 Résumé formel  
> **`E!;` est une forme postfixe qui transforme une expression pipeline ou UFC en une assignation implicite sur la variable de tête.**

---

Si tu veux, je peux maintenant produire :

- **la règle de typage complète**  
- **la sémantique formelle**  
- **la transformation complète AST → R**  

Tu veux laquelle en premier ?
