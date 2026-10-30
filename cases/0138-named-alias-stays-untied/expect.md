# a named alias is not tied; Alias@A opts in

`fn(p: Point): Point { Point:{...} }` reste valide ; `Point@A` demande de rendre le record de l'appelant.

Voir `rfcs/0000-record-bound-generics.md`.
