1. Syntaxe

1.1. Déclaration de fonction

Grammaire (extrait)

`text
FunctionDecl ::= "fn" Ident "(" ParamList? ")" "->" Type Block

ParamList    ::= Param ("," Param)*
Param        ::= Ident ":" Type
               | "..." Ident ":" Type
`

- Paramètre variadique :

`text
"..." x : T
`

- Exemples :

`text
fn sum(...xs: Number) -> Number { ... }

fn paste0(...args: Printable) -> String { ... }
`

> Contrainte syntaxique :  
> Il ne peut y avoir au plus qu’un seul paramètre variadique, et il doit être en dernière position.

---

2. Domaine des types autorisés pour T

Définition :

Soit Type l’ensemble des types valides de TypR.  
On autorise :

- Types primitifs : Number, String, Bool, etc.  
- Unions : A | B | ...  
- Records : { x: Number, y: Number }  
- Interfaces : interface I { ... }  
- Alias : type T = ...  
- Types paramétriques : Vector<T>, Option<T>, etc.

> Règle :  
> Si T ∈ Type, alors ...T est un type de paramètre valide.

---

3. Sémantique statique de ...T

On considère une déclaration :

`text
fn f(...x: T) -> U { body }
`

et un appel :

`text
f(a₁, a₂, ..., aₙ)
`

3.1. Environnement de typage

- Γ : environnement de typage (associe identifiants → types)
- Γ ⊢ e : T : “dans Γ, l’expression e a le type T”

3.2. Sous-typage

On suppose une relation de sous-typage :

\[
S <: T
\]

définie pour :

- Unions :

\[
A <: A \mid B
\quad\text{et}\quad
B <: A \mid B
\]

- Records (structural) :

Si

\[
R1 = \{ fi : Ti \}{i \in I},\quad
R2 = \{ fj : Uj \}{j \in J}
\]

alors

\[
R1 <: R2
\quad\text{ssi}\quad
J \subseteq I \ \land\ \forall j \in J,\ Tj <: Uj
\]

- Interfaces (structural) :

Si I est une interface avec méthodes \(\{ mk : Tk \}_{k \in K} \), alors

\[
S <: I
\quad\text{ssi}\quad
S\ \text{fournit toutes les méthodes}\ m_k\ \text{avec types compatibles}
\]

(compatibilité = sous-typage des types de fonctions).

---

4. Règles de typage pour les fonctions variadiques

4.1. Typage d’une déclaration variadique

Règle (T-Fun-Vararg)

Soit :

`text
fn f(...x: T) -> U { body }
`

On introduit dans le corps :

- soit x comme séquence de valeurs de type T (abstraction de haut niveau)
- soit, plus concrètement, x : Array<T> si tu modélises ça comme un vecteur interne.

Formellement :

\[
\frac{
  Γ, x : Seq(T) ⊢ body : U
}{
  Γ ⊢ fn\ f(...x:T) -> U\ \{ body \} : ( ...T ) -> U
}
\]

où Seq(T) est un type interne représentant une séquence d’arguments de type T (non exposé à l’utilisateur si tu veux garder la syntaxe simple).

> Remarque : côté surface language, tu peux considérer que x est un pseudo‑paramètre représentant la collection des arguments variadiques.

---

4.2. Typage d’un appel à une fonction variadique

Soit :

`text
fn f(...x: T) -> U { ... }
`

et un appel :

`text
f(a₁, a₂, ..., aₙ)
`

Règle (T-Call-Vararg)

\[
\frac{
  Γ ⊢ f : (...T) -> U
  \quad
  ∀ i \in \{1..n\},\ Γ ⊢ ai : Si \ \land\ S_i <: T
}{
  Γ ⊢ f(a1, ..., an) : U
}
\]

Autrement dit :

- chaque argument aᵢ doit avoir un type Sᵢ  
- et ce type doit être sous‑type de T.

---

4.3. Cas des unions

Si T est une union :

\[
T = A \mid B \mid ...
\]

et aᵢ : Sᵢ, alors on exige :

\[
S_i <: T
\]

ce qui est vrai si :

\[
Si <: A \quad\text{ou}\quad Si <: B \quad\text{ou}\ ...
\]

---

5. Compatibilité avec la vectorisation implicite

Tu veux que ce modèle reste compatible avec la vectorisation de TypR.

On distingue deux niveaux :

- Niveau “paramètre variadique” : ...T  
- Niveau “valeur vectorisée” : Vector<T> ou équivalent

5.1. Règle conceptuelle

> Règle (V-Compat) :  
> Un argument de type Vector<T> passé à un paramètre ...U est traité comme un seul argument, pas comme un “spread” automatique, et doit vérifier :
> \[
> Vector<T> <: U
> \]

La vectorisation implicite de TypR se fait dans le corps de la fonction, pas au niveau de l’appel variadique.

Exemple :

`typR
fn paste0(...args: Printable) -> String {
    // à l’intérieur, tu peux décider de vectoriser
}
`

Appel :

`typR
paste0("a", ["b", "c"])
`

- "a" : String <: Printable
- ["b","c"] : Vector<String> <: Printable si tu définis Printable pour accepter aussi des vecteurs, ou pas, selon ton design.

La vectorisation automatique (par exemple produire ["ab","ac"]) est une règle séparée, au niveau de la sémantique d’exécution, pas du typage de ....

---

6. Traduction vers R (sémantique dynamique)

6.1. Schéma général

Pour :

`typR
fn f(...x: T) -> U {
    body
}
`

tu génères en R :

`r
f_tr <- function(...) {
    # éventuellement : xs <- list(...)
    # puis traduction de body en utilisant xs
}
`

- Les vérifications de type sont faites à la compilation par TypR.  
- R reçoit une fonction classique à ellipsis ....

6.2. Correspondance formelle

Soit :

\[
\llbracket \cdot \rrbracket
\]

la traduction TypR → R.

Alors :

\[
\llbracket fn\ f(...x:T) -> U\ \{ body \} \rrbracket
=
f\_tr \leftarrow function(...) \{ \llbracket body \rrbracket \}
\]

et pour un appel :

\[
\llbracket f(a1, ..., an) \rrbracket
=
f\tr(\llbracket a1 \rrbracket, ..., \llbracket a_n \rrbracket)
\]

---

Si tu veux, on peut maintenant :

- préciser la forme exacte de Seq(T) dans ton AST,  
- écrire les règles d’inférence complètes (avec les autres formes d’appels),  
- ou intégrer ça dans ta spécification globale de TypR (chapitre “Fonctions”).
