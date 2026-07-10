# interface-alias-missing-from-module-export

## Contexte

Dans `TypR/object.ty`, une interface publique est déclarée :

```typr
@pub
type Animable <- interface {
    animate: (self: Self, animation: Animation) -> Self
};
```

et utilisée dans une intersection : `@pub type Object <- Id & Animable;`.

## Ce qui DEVRAIT se passer

Une interface (`Type::Interface`) est un validateur structurel compile-time only (cf.
CLAUDE.md "Interface Constructors") : elle ne génère **aucune** valeur R (pas de fonction
constructeur, pas de binding). Le module qui la déclare `@pub` ne doit donc **pas** essayer de
l'exporter comme s'il s'agissait d'un alias porteur d'une valeur R réelle (record, primitif,
union taguée, ...).

## Anomalies observées (→ règles expect.toml)

Le fichier généré `R/object.R` contient, dans le bloc d'export du module (juste avant la
fermeture du `local({...})`) :

```r
object$Animable <- Animable
```

alors que `Animable` n'a jamais été assigné nulle part dans le fichier — l'interface, à raison,
n'a généré ni constructeur ni validateur. Résultat : ce simple export référence une variable R
inexistante.

Conséquence concrète observée dans `observed.txt` : le chargement du package
(`devtools::document` / `pkgload::load_all`) échoue avec :

```
Error in `load_all()`:
! Failed to load 'R/object.R'
Caused by error:
! objet 'Animable' introuvable
```

C'est très probablement la cause du signalement utilisateur ("l'interface que j'ai créé ne
semble pas exister dans le code R généré") : l'interface n'existe effectivement dans aucune
valeur R (ce qui est correct et voulu), mais le code d'export de module ne le sait pas et
essaie quand même de la propager, cassant le chargement de tout le module.

## Localisation

`crates/typr-core/src/processes/transpiling/mod.rs`, arm `Lang::Module { .. }`, boucle
d'export des alias publics (~lignes 2219-2230) : matche `Lang::Alias { identifier, is_public:
true, .. }` et pousse inconditionnellement `format!("{name_str}${alias_name} <- {alias_name}")`
sans jamais regarder `target_type` — donc sans jamais exclure le cas `Type::Interface`.

## Correction attendue

Filtrer cette boucle d'export pour ne pas émettre `module$Name <- Name` quand
`target_type.is_interface()` (`Type::is_interface()`, `components/type/mod.rs:325`) — même
logique que celle déjà appliquée au cas top-level (catch-all à la ligne ~1885 de
`transpiling/mod.rs`, qui ne génère aucun code pour un alias d'interface).
