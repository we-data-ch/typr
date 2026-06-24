# Erreurs identifiées dans le projet R (framr)

## 1. Enregistrement S3 erroné (`animation.R:45`)
```r
registerS3method("animate", "%T", animate.Generic)
```
La classe enregistrée est `"%T"` au lieu de `"Generic"`. Aucune méthode `animate` ne sera trouvée par dispatch S3.

## 2. Circle n'hérite pas de `"Generic"` (`circle.R:19`)
```r
class(x) <- c("Circle", "Position", "Record1", "list")
```
La classe `"Generic"` n'est pas dans la chaîne d'héritage. Même après correction du point 1, `animate.Generic` ne serait jamais dispatché sur un objet `Circle`.

## 3. Variables non définies dans `animate_move.Record4` (`animation.R:40-42`)
```r
`animate_move.Record4` <- (function(a, direction) {
  validate_Animator(Self(position = move(position[['a']], direction), .spread = a))
}) |> as.Generic()
```
- `Self` : constructeur inexistant
- `position` : variable non définie dans ce scope
- `position[['a']]` : utilise `'a'` comme nom de champ littéral au lieu du paramètre `a`

## 4. `c()` sur des objets typés perd les classes S3
```r
c(0L |> as.Integer(), 0L |> as.Integer())  # circle.R:37, position.R:34-40
```
`c()` est une primitive R qui supprime les attributs de classe. Les objets `Integer` perdent leur typage.

## 5. Méthode pour type inexistant (`animation.R:46-47`)
```r
registerS3method("animate_move", "AniPos", animate_move.AniPos)
```
- Le type `"AniPos"` n'est défini nulle part dans le projet
- `animate_move.AniPos` n'est jamais défini (ni dans `animation.R` ni ailleurs)
