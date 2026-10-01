# generic-record-binding-nondeterministic

## Ce qui devrait se passer

`first(p: list { a: T, b: U, c: V }): T` appelé avec `:{ a = 1, b = "s", c = true }` doit
lier `T` au type du champ `a` (donc `int`) à chaque exécution : `let y: int <- first(r)` passe
toujours.

## Anomalies

`observed.txt` montre `type int doesn't match type true` / `... "t"` : `T` a été lié au type
d'un autre champ (`c` ou `b`). Le résultat change d'une exécution à l'autre (un appel isolé
échoue environ 2 fois sur 3) : l'arm `Record/Record` de `get_gen_type`
(`type_checking/mod.rs`) fait un `zip` sur deux `HashSet<ArgumentType>` et apparie donc les
champs dans un ordre arbitraire. Le repro contient 8 appels indépendants pour que la
probabilité qu'ils passent tous par hasard soit négligeable tant que le bug existe.
