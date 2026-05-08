Vue d’ensemble

use est une directive de résolution de noms qui permet d’introduire dans le scope courant des symboles publics (@pub) provenant d’un module.  
Elle ne crée pas de nouveaux symboles dans le module, elle ne fait que lier des noms dans le scope courant vers des membres publics existants.

---

Syntaxe formelle

Forme générale

`ebnf
UseDirective  ::= "use" UsePath ";"

UsePath       ::= ModulePath "::" UseSelector
                | ModulePath

ModulePath    ::= Ident ( "::" Ident )*

UseSelector   ::= "*" 
                | "{" UseItemList "}"

UseItemList   ::= UseItem ( "," UseItem )* [ "," ]

UseItem       ::= Ident [ "as" Ident ]
`

Remarques

- use M::*; : import global de tous les symboles publics de M.
- use M::{a, b as c}; : import sélectif, avec renommage optionnel.
- use M; (sans ::) : n’importe rien dans le scope courant, mais peut être réservé pour d’autres usages (par exemple, forcer la résolution/chargement du module). Tu peux décider de l’interdire pour l’instant si tu veux rester strict.

---

Domaine d’application

Scope

- Portée lexicale :  
  Une directive use s’applique au scope lexical dans lequel elle apparaît (fichier, bloc, module interne, etc.).
- Ordre :  
  Toutes les directives use d’un même scope sont traitées avant la résolution des références dans ce scope.

Visibilité

- use ne rend visibles que les symboles marqués @pub dans le module cible.
- Les symboles non publics restent inaccessibles, même via use.

---

Sémantique

1. Résolution du module

Pour une directive :

`r
use A::B::C::{x, y as z};
`

1. Résoudre A::B::C comme un module existant dans l’environnement de compilation.
2. Si le module n’existe pas → erreur de compilation : UnknownModule.

2. Sélection des symboles

- Cas * :

  `r
  use M::*;
  `

  1. Collecter l’ensemble \( P \) des symboles publics du module M.
  2. Pour chaque symbole \( s \in P \), proposer une liaison dans le scope courant sous le même nom s.

- Cas { ... } :

  `r
  use M::{a, b as c};
  `

  Pour chaque UseItem :

  - a :
    - Vérifier que a est un symbole public de M.
    - Créer une liaison dans le scope courant sous le nom a.
  - b as c :
    - Vérifier que b est un symbole public de M.
    - Créer une liaison dans le scope courant sous le nom c.

3. Nature de la liaison

Pour chaque symbole importé :

- La liaison dans le scope courant est un alias vers le membre du module, pas une copie conceptuelle.
- En pratique (dans ta transpilation R), cela se traduit typiquement par :

  `r
  a <- M$a
  c <- M$b
  `

- Si ton langage supporte la mutabilité sur ces symboles, la sémantique doit être définie clairement :
  - soit alias vers la valeur (mutation via le module seulement),
  - soit copie de la valeur (mutation locale indépendante).
  
  Tu peux spécifier, par défaut, que use importe des références en lecture seule (API), et que la mutation des membres de module se fait uniquement via Module::name.

---

Résolution de noms

Ordre de résolution dans un scope

Lorsqu’un identifiant x est rencontré dans un scope :

1. Déclarations locales (variables, paramètres, let, etc.).
2. Imports use du scope courant :
   - symboles importés explicitement ({...}),
   - symboles importés via *.
3. Membres accessibles via qualification (Module::x).
4. Scopes englobants (si ton langage a des scopes imbriqués).

Si x est trouvé à plusieurs niveaux au même rang (par exemple, deux use qui importent x), c’est une collision.

---

Gestion des collisions

Types de collisions

1. Collision avec une déclaration locale

   `r
   let x <- 1
   use M::{x};
   `

   → Erreur de compilation : NameConflictLocal("x").

2. Collision entre deux use

   `r
   use A::{x};
   use B::{x};
   `

   → Erreur de compilation : NameConflictImport("x").

3. Collision entre * et un import explicite

   `r
   use A::*;
   use B::{x};
   `

   Si A exporte aussi x → Erreur de compilation : NameConflictImport("x").

Règle générale

- Aucune ombre silencieuse : toute collision de nom introduit par use est une erreur.
- Le développeur doit résoudre la collision via :
  - renommage :

    `r
    use A::{x as ax};
    use B::{x as bx};
    `

  - ou suppression d’un des imports.

---

Erreurs de compilation formelles

Tu peux définir un ensemble d’erreurs liées à use :

- UnknownModule(path: ModulePath)  
  Le module référencé n’existe pas.

- UnknownSymbol(module: ModulePath, name: Ident)  
  Le symbole demandé n’existe pas dans le module.

- SymbolNotPublic(module: ModulePath, name: Ident)  
  Le symbole existe mais n’est pas marqué @pub.

- NameConflictLocal(name: Ident)  
  Un symbole importé entre en conflit avec une déclaration locale.

- NameConflictImport(name: Ident)  
  Deux imports (ou plus) tentent de lier le même nom dans le scope.

- WildcardNotAllowedHere (optionnel)  
  Si tu décides d’interdire * dans certains contextes (par exemple à l’intérieur de fonctions).

---

Exemples canoniques

Import global

`r
module Math {
  @pub let pi <- 3.14159
  @pub let sin <- function(x) { ... }
  let secret <- 42
}

use Math::*;

print(pi)        # OK
print(sin(0.5))  # OK
print(secret)    # Erreur : SymbolNotPublic("Math", "secret")
`

Import sélectif avec renommage

`r
use Math::{pi as PI, sin};

print(PI)
print(sin(0.5))
`

Collision

`r
let pi <- 3

use Math::{pi};  # Erreur : NameConflictLocal("pi")
`

`r
use A::{x};
use B::{x};      # Erreur : NameConflictImport("x")
`

---

Résumé formel

- But : introduire des alias locaux vers des symboles publics de modules.
- Syntaxe clé :
  - use M::*;
  - use M::{a, b as c};
- Portée : lexicale, limitée au scope où apparaît la directive.
- Visibilité : uniquement les symboles @pub.
- Collisions : toujours des erreurs, jamais de shadowing implicite.
- Sémantique : alias vers les membres du module, sans casser l’encapsulation.

Si tu veux, on peut maintenant écrire la section “Modules & Imports” de la spec de ton langage en intégrant module, @pub, Module::name et use dans un tout cohérent.
