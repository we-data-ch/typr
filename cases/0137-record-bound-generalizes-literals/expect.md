# caller's literal field types are widened on the way out

La fonction a pu écrire n'importe quel `int` dans `a` : le résultat est `list{a: int, b: bool}`, jamais `list{a: 1, b: true}`.

Voir `rfcs/0000-record-bound-generics.md`.
