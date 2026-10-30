# an exact return type is a closed record

Un record littéral satisfait `exact list{a: int}` ; le résultat est fermé, donc `r$b` est refusé (alors que `r$a` passe).

Voir `rfcs/0000-record-bound-generics.md`.
