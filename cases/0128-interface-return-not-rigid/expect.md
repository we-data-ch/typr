# a body returning a concrete value is accepted for an interface (rigid) return type

`fn(a: Lovable): Lovable { tom }` et `fn(a: Lovable@A): Lovable@A { tom }` (avec `tom: Cat`) doivent
être rejetés : le type de retour est celui que l'appelant a passé (`Dog` peut-être), pas « un
`Lovable` quelconque ». Le corps ne peut renvoyer que le paramètre (le rigide).

Cause : `is_subtype_raw` (`components/type/mod.rs`) avait un bras `(_, Generic) => true` qui
acceptait aussi les rigides. Un rigide n'accepte plus que lui-même et `Any`. Ce bras masquait un
second défaut : `typing_container` (`type_checking/mod.rs`) imprimait puis re-parsait le type des
éléments, et `[a]` avec `a: T` (rigide `__rigid_0`) devenait `Variable(".RIGID_0")`. Le type
d'élément est maintenant repris tel quel.

Prérequis de `rfcs/0000-record-bound-generics.md`.
