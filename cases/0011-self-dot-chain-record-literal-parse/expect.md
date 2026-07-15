# self-dot-chain-record-literal-parse

## Ce qui devrait se passer

Dans `TypR/scene.ty`, le champ `objects` du littéral d'enregistrement `Scene:{ ... }` a pour
valeur `self.objects.extend(object)` — une chaîne d'accès point (`self.objects`) suivie d'un
appel UFCS (`.extend(object)`) — et le littéral se termine par un spread `...self`. Le
parser devrait reconnaître l'intégralité de `self.objects.extend(object)` comme la valeur du
champ `objects`, exactement comme il le ferait hors d'un littéral d'enregistrement ou en
l'absence du spread `...self` qui suit.

## Anomalies

`observed.txt` montre que `typr check` rapporte :

```
Type error: Unknown element `elf.objects.extend(object),` in `TypR/scene.ty` at `65`
```

Un seul caractère de `self` (le `s`) est consommé comme valeur du champ `objects` ; le reste,
`elf.objects.extend(object),`, n'est pas reconnu du tout et remonte comme un élément inconnu.
L'utilisateur a confirmé en session que le symptôme est bien limité à `self` en tête de
l'expression — la même expression sans le `self` (ou avec un autre identifiant) parse
correctement. La cause probable est une interaction entre le combinator qui reconnaît le champ
`objects: <expr>` d'un littéral record et le `...self` de fin de littéral, ou un parser
spécifique au mot `self` (voir `\TypeName:{ ...self }` / `Self:{...}` desugaring) qui ne
consomme qu'un préfixe trop court avant de rendre la main.

Les autres erreurs listées dans `observed.txt` (mismatch de type sur `objects: [Object]`,
`@`<Scene> non défini, `new_scene` non défini) sont des conséquences en cascade de cet échec de
parsing initial sur `TypR/main.ty`/`TypR/scene.ty` et ne font pas partie de ce cas — seul le
message `Unknown element` sur `elf.objects.extend(object),` est dans le périmètre ici.
