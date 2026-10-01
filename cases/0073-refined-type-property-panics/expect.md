# refined-type-property-panics

## Ce qui devrait se passer

`int & (> 0)` est un type raffiné (`refined_types.md`, `refined_types_plan.md` Phase 2) : la base
`int` intersectée avec la propriété de valeur `(> 0)`. `let x: int & (> 0) <- 3;` doit passer
`typr check`, sans panic.

## Anomalies

`observed.txt` : `exit=101`. Deux causes empilées dans le parseur de types :

1. `>` est lu comme un `Op` par `index_algebra`, et `compute_operators` finissait sur
   `_ => panic!()` (`parsing/types.rs`). Corrigé : la fonction est maintenant faillible et
   `index_chain` échoue proprement, ce qui laisse `alt` essayer autre chose.
2. Rien ne sait encore lire `(> 0)` comme propriété : `ltype` obtient une suite de jetons
   invalide et `PriorityTokens::run_helper` lance `panic_any(TypeError::WrongExpression)`
   (`operation_priority.rs`). C'est le canal général des erreurs de syntaxe de type
   (`let x: int & ;` panique de la même façon) ; le cas se ferme quand la Phase 2 ajoute le
   parseur `refinement_property`.
