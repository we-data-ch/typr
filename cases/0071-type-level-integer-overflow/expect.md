# type-level-integer-overflow

## Ce qui devrait se passer

Une dimension ou un entier singleton de type hors i32 (`[99999999999, int]`) est signalé comme
erreur de syntaxe. Il ne doit ni faire paniquer le compilateur ni être réinterprété en silence.

## Anomalies

`observed.txt` : `exit=101`, panic `ParseIntError` à `parsing/types.rs:159`
(`simple_index`). Le chemin voisin `integer_literal` (`types.rs:~557`) est pire : il fait
`parse().unwrap_or(0)`, donc `type Big <- 99999999999` devient en silence le singleton `0`.
`indexation.rs:42/50` (`parse_integer`, `parse_positive_integer`) ont le même `unwrap()`.
