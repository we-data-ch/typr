# Use case
UI wrapper

# Titre
Il faut créer un type system converter:

Pour pouvoir utiliser le dispatch de R, il va faloir convertir le type system de TypR.
J'aurai dû m'en douter, mais le type system pour le dispatch de R est plus faible que celui de TypR.
En effet le system de type de R est uniquement nominal. 
Je doit donc faire une transformation au cas par cas pour un projet en donnant des types "concret".

Par exemple dans TypR un même record peut avoir plusieurs définitions de type équivalentes:

{x: int, y: int} et {y: int, x: int} sont exactement similaire dans TypR.

Mais si je fais une transformation en un type nominal, même si j'essaie de conserver les informations importantes on tombera sur deux types nominalement différent:

Rec_x_int_y_int sera différent de Rec_y_int_x_int.

Nous avons aussi les mêmes problème de sous typage quand on parle du row polymorphism, des tag-unions et des interface.

C'est pourquoi on peut récupérer le context de typage après le type checking et conserver uniquement les types effectivement déclarés dans le projet ainsi que leur relation de sous-typage et équivalence relatives.

Tout les types jugés "équivalents" (par définition ou par aliasing) vont avoir un même type qui servira de représentant.
Tout les sous types entreront dans une hiérarchie. Les types enfants prendront d'avantages des types parents.


{} => tout record et tuple

{@L: T} => Tout record (sauf record vide) mais catch le premier label avec son type
{+@L: +T} => Tout record (sauf record vide) mais on catch chaque couple
+{@L: T} => La même chose qu'en haut
{+@L: T} => Tout record (sauf record vide) mais on catch que les labels
{@L: +T} => Tout record (sauf record vide) mais on catch que les types

{#N: T} => Tout tuple mais catch le premier couple
{T} => Tout tuple mais catch le premier couple
{+#N: +T} => Tout tuple mais catch le premier couple
{+T} => Tout tuple mais catch que les types
{+#N: T} => Tout tuple mais catch les labels

# Match to if

match res {
	None => true,
	Some(t) => false
}

alors on doit mettre:

if (res[[1]] == None) { true }
else if (res[[1]] == Some) { false }


# Structural to nominal
## Sous-typage

Pour optimiser le typage, il faut developper un arbre de sous typage. il faut aussi considérer les catégories de types et faire des arbres dédiés pour éviter les comparaisons inutiles.

Les alias sont converti en type natif et les opaques n'ont pas de sous-typage comme les primitives

Génériques (un seul)
Interfaces:
	- top: interface {}
	- bottom: ...
Arrays
	- top: [#M, T]
	- bottom: ... 
Records:
	- top: {}, {@L: V}
	- bottom: ...
Union:
	- top: ...
	- bottom: ...

J'utiliserai un arbre binair. Pour un type interface, on regardera seulement sur l'arbre interface. Pour un type, on regardera dans l'arbre de sa catégorie puis dans l'arbre d'interface. 
