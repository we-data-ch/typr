Le problème vient de la ligne 9-15 où `incr.Point` est défini.

```r
`incr.Point` <- (function(p) { ... } |> Point()) |> Generic()
```

Le pipe `|> Point()` sur la ligne 15 transforme la fonction en un objet liste `Point` (car `Point(fn)` crée `list(x = fn, y = NULL)` avec la classe `"Point"`). Donc `incr.Point` est une **liste**, pas une fonction.

Quand `UseMethod('incr', x)` cherche `incr.Point` pour le dispatcher S3, il trouve un objet liste non-appelable → **"attempt to apply non-function"**.

**Correction :** retirer `|> Point()` pour que la fonction reste une fonction :

```r
`incr.Point` <- (function(p) {
Point(x = {
p$x + 1L |> Integer()
}, y = {
p$y + 1L |> Integer()
})
} |> Generic())
```
