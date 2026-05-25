Objectif de la spécification

Spécifier formellement, pour TypR :

- la fusion génériques / interfaces  
- la règle d’inférence des paramètres de type  
- la distinction universel (paramètre) / existentiel (retour)  
- le désucrage vers une forme noyau avec forall / exists et interfaces structurelles.

---

1. Syntaxe de surface (simplifiée)

On suppose :

`ebnf
Type      ::= Id                    // int, String, A, Incrementable
           | "interface" "{" Methods "}"
           | Type "&" Type          // intersection
           | ...                    // autres formes

Method    ::= Id ":" Type

FunDecl   ::= "fn" Id "(" Params ")" ":" Type "{" Body "}"

Param     ::= Id ":" Type
`

Exemples de surface :

`typR
type Incrementable <- interface {
    incr: (self: Self) -> Self
};

fn double(i: Incrementable): Incrementable { ... }

fn f(a: A, b: B): A { ... }
`

---

2. Forme noyau : types universels et existentiels

On introduit une forme noyau explicite :

2.1. Types noyau

- Types \(T\) :

\[
T ::= X \mid \text{Int} \mid \dots \mid T \& T \mid \text{Iface}(R)
\]

où \(X\) est une variable de type, et \(R\) est un ensemble de méthodes (signature structurelle).

- Contrainte d’interface :

\[
C ::= \text{Iface}(R)
\]

- Schémas de types :

\[
\sigma ::= \forall X1:C1, \dots, Xn:Cn.\ T
\]

- Types existentiels :

\[
\exists X:C.\ T
\]

---

3. Environnement de types

- \(\Gamma\) : environnement des types nommés (ex. Incrementable)  
- \(\Delta\) : environnement des variables de type (paramètres génériques)

On suppose :

`text
Γ(Incrementable) = Iface({ incr: Self -> Self })
`

---

4. Règle d’inférence des paramètres génériques

4.1. Types libres → paramètres génériques

Dans une signature de fonction de surface :

`typR
fn f(p1: T1, ..., pn: Tn): Tr { ... }
`

on collecte l’ensemble des identifiants de type libres :

\[
\text{FreeTypes}(T1, \dots, Tn, Tr) = \{ X1, \dots, X_k \}
\]

où chaque \(X_i\) n’est ni :

- un type primitif (int, string, …)  
- un type défini dans \(\Gamma\) (Incrementable, Vector, …)

Pour chaque \(X_i\), on introduit un paramètre de type :

\[
X_i : \text{Iface}(\emptyset)
\]

c’est‑à‑dire une interface vide.

4.2. Désucrage générique

La signature de surface :

`typR
fn f(a: A, b: B): A
`

devient en noyau :

\[
\forall A:\text{Iface}(\emptyset), B:\text{Iface}(\emptyset).\ A \times B \to A
\]

ou en pseudo‑TypR noyau :

`typR
fn fA: interface {}, B: interface {}: A
`

---

5. Interfaces nommées et structurelles

5.1. Définition d’interface

Surface :

`typR
type Incrementable <- interface {
    incr: (self: Self) -> Self
};
`

Noyau :

\[
\Gamma(\text{Incrementable}) = \text{Iface}(\{ \text{incr}: \text{Self} \to \text{Self} \})
\]

5.2. Utilisation comme contrainte

Surface :

`typR
fn double(i: Incrementable): Incrementable { ... }
`

Désucrage (voir section 6) :

`typR
fn doubleA: Incrementable: A
`

Noyau :

\[
\forall A:\text{Iface}(\{ \text{incr}: \text{Self} \to \text{Self} \}).\ A \to A
\]

---

6. Règle clé : paramètre vs retour

6.1. Interface en paramètre → universel (générique contraint)

Règle informelle :

> Si un type d’interface I apparaît dans un paramètre, alors il introduit un paramètre de type universel \(A\) avec contrainte \(A : I\).

Formellement, pour une fonction de surface :

`typR
fn double(i: Incrementable): Incrementable { ... }
`

1. On détecte que Incrementable est une interface dans \(\Gamma\).  
2. On introduit un paramètre de type \(A\) avec contrainte :

\[
A : \Gamma(\text{Incrementable})
\]

3. On remplace le type du paramètre et du retour par \(A\).

Désucrage :

`typR
fn doubleA: Incrementable: A { ... }
`

Type noyau :

\[
\forall A:\text{Iface}(\{ \text{incr}: \text{Self} \to \text{Self} \}).\ A \to A
\]

6.2. Interface uniquement en retour → existentiel

Règle informelle :

> Si une interface I apparaît uniquement en retour, sans être liée à un paramètre, alors le type de retour est un existentiel.

Surface :

`typR
fn makeCounter(): Incrementable { ... }
`

Désucrage noyau :

\[
\exists A:\text{Iface}(\{ \text{incr}: \text{Self} \to \text{Self} \}).\ A
\]

ou en pseudo‑TypR :

`typR
fn makeCounter(): some A: Incrementable
`

---

7. Typage de l’exemple Incrementable / double

7.1. Interface

`typR
type Incrementable <- interface {
    incr: (self: Self) -> Self
};
`

\[
\Gamma(\text{Incrementable}) = \text{Iface}(\{ \text{incr}: \text{Self} \to \text{Self} \})
\]

7.2. Implémentation pour int

`typR
let incr <- fn(self: int): int {
    self + 1
};
`

Règle d’inférence d’interface (informelle) :

> Si une fonction fn(self: T): T est enregistrée comme implémentation de incr, et que la signature de incr dans Incrementable est (Self) -> Self, alors T est un sous‑type structurel de Incrementable.

On enregistre :

\[
\text{int} \leq \text{Incrementable}
\]

7.3. Définition de double

Surface :

`typR
let double <- fn(i: Incrementable): Incrementable {
    i.incr().incr()
};
`

Désucrage :

`typR
fn doubleA: Incrementable: A {
    i.incr().incr()
}
`

Type noyau :

\[
\forall A:\text{Iface}(\{ \text{incr}: \text{Self} \to \text{Self} \}).\ A \to A
\]

7.4. Application double(3)

- 3 : int
- int \leq Incrementable
- on choisit \(A = \text{int}\)

Donc :

\[
\text{double}(3) : \text{int}
\]

Conclusion : le type de retour est bien le type réel, pas Incrementable.

---

8. Résumé des règles de désucrage

1. Types libres dans une signature  
   → deviennent des paramètres de type universels avec contrainte interface {}.

2. Interface en paramètre  
   `typR
   fn f(x: I): I
   `
   →  
   `typR
   fn fA: I: A
   `

3. Interface uniquement en retour  
   `typR
   fn f(): I
   `
   →  
   `typR
   fn f(): some A: I
   `

4. Interface nommée  
   `typR
   type I <- interface { ... }
   `
   →  
   \(\Gamma(I) = \text{Iface}(\{ ... \})\)

---

Si tu veux, on peut maintenant :

- raffiner ça en règles de typage naturelles (style \(\Gamma \vdash e : T\)),  
- ou écrire la traduction vers un IR avec dictionnaires (style typeclasses / traits) pour ton backend R/JS.
