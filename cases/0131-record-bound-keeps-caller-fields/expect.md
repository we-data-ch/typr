# a record parameter tied to the return keeps the caller's extra fields

Un `fn(x: list{a: int}): list{a: int}` renvoie le record de l'appelant : `p$name` est un `char`.

Voir `rfcs/0000-record-bound-generics.md`.
