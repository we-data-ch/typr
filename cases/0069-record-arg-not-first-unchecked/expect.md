# record-arg-not-first-unchecked

## Ce qui devrait se passer

Un argument de type record en position 2 ou plus doit être vérifié contre le type du
paramètre, comme l'est le premier argument (`take(q)` avec `q: Qq` pour `p: Pt` est rejeté)
et comme le sont les arguments scalaires. Les trois appels du repro sont invalides :
un record d'un autre type (`Qq` pour `Pt`), un record dont le champ `x` a le mauvais type
(`Rr { x: char }` pour `Pt { x: int }`), et le même en 3ᵉ position.

## Anomalies

`observed.txt` : le checker termine sans aucune erreur. Cause : `get_gen_type`
(`type_checking/mod.rs`, arm `Record/Record`) renvoie toujours `Some(..)` — c'est un
collecteur de liaisons de génériques utilisé comme porte d'entrée par
`Context::get_unification_map`, qui ne rejette donc jamais deux records. Pour le premier
argument, `filter_by_first_param` fait un vrai contrôle de sous-typage à part, ce qui masque
le trou. (Les arguments de type *fonction* ne sont en revanche pas concernés : un premier
diagnostic les disait non vérifiés, mais c'était un artefact du nom `apply`, qui collisionne
avec l'`apply` de la std.)

Deuxième cause, visible sur les records anonymes (`:{ z = 1 }` passé pour un `Pt`) : le dernier
arm de `get_gen_type` teste `is_subtype(.., &Context::empty())`. Avec un contexte vide, l'alias
`Pt` ne se résout pas et se réduit à `Any` (cf. S4 de `audit_type_checking.md`), dont tout type
est sous-type. Le correctif renvoie `None` pour un alias transparent en paramètre, ce qui laisse
`match_types_to_generic` retenter sur les types réduits, avec le vrai contexte.
