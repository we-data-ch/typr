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


