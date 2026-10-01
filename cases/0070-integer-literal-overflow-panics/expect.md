# integer-literal-overflow-panics

## Ce qui devrait se passer

Un littéral entier hors de la plage i32 (`2147483648`, `-2147483649`, `99999999999`) est un
double en R : il doit se typer `num`, pas faire tomber le compilateur. `99999999999.5` (déjà un
`num`) et `2147483647` (le plus grand `int`) doivent continuer à passer.

## Anomalies

`observed.txt` : `exit=101`, panic `ParseIntError { kind: PosOverflow }` à
`parsing/elements.rs:117` (`symbol.parse::<i32>().unwrap()` dans `integer_impl`). Même
`99999999999.5` panique, car `integer` est tenté avant `number` sur certains chemins du parseur.
Le panic emporte aussi le LSP.
