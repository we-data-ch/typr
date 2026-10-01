# union-subtype-always-true

## Ce qui devrait se passer

`U1 <: U2` n'est vrai que si chaque membre de `U1` tient dans un membre de `U2` (même tag, charge
utile sous-type). Sur le repro, `Shape -> Unrelated` (tags disjoints), `Shape -> WrongPayload`
(mêmes tags, charge `char` au lieu de `int`) et `Shape -> Partial` (un tag de `Shape` absent) sont
rejetés ; l'élargissement `Shape -> Wider` et l'identité `Shape -> Shape` restent acceptés.

## Anomalies

`observed.txt` : aucune erreur. `is_subtype_raw` (`components/type/mod.rs`) avait un arm
`(Union, Union) => true //TODO: Fix this` placé avant les deux arms génériques (« tous les membres
gauches sous-types de la droite », « sous-type d'au moins un membre droit »), qui masquait la bonne
règle. Un `Shape` était donc utilisable là où un union sans aucun tag commun était attendu, et le
`match` du receveur pouvait alors rencontrer un tag qu'il ne connaît pas.
