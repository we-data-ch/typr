# Contribuer à TypR

## Modèle de branches

- `main` — ce qui est publiable. Les tags de release en partent, et
  `nu publish.nu release` refuse de publier depuis autre chose.
- `develop` — intégration.
- Les contributions passent par une PR vers `develop`.

`main` devrait être protégée sur GitHub : PR obligatoire, CI verte requise.
Sans ça, la garde de `publish.nu` ne protège que celui qui l'utilise.

## Ce que la CI vérifie

| Job | Vérifie |
|---|---|
| `test` | `cargo test --workspace` — 868 tests, bloquant |
| `build-check` | compilation en `--release` |
| `cases` | rejeu du catalogue `cases/` — échoue sur toute `REGRESS` |
| `coherence` | versions alignées, aucun binaire commité |
| `editors` | l'extension VS Code compile, le package R est analysable |

`RUST_MIN_STACK=8388608` est indispensable : le typage est récursif et déborde
la pile par défaut de 2 Mo sur au moins un test. La CI le définit ; en local,
exporte-le si `cargo test` s'interrompt sur un `stack overflow`.

## Travailler sur le parseur ou le typage

Le catalogue `cases/` est un ensemble de reproductions curées, chacune avec sa
sortie R attendue. C'est à la fois la documentation d'un comportement et son
test de non-régression.

```bash
typr case list              # état du catalogue
typr case run               # rejoue tout ; sort en 1 sur REGRESS
typr case add --from <proj> # crée un cas depuis un vrai projet
typr case freeze <nnnn>     # fige la sortie actuelle comme golden
```

Une correction de bug sans cas associé n'a rien qui l'empêche de revenir.

## Travailler sur la documentation

La documentation vit dans un dépôt séparé : `we-data-ch/typr.github.io`.
Chaque PR y déclenche une construction complète du site — `onBrokenLinks` est
réglé sur `throw`, donc un lien mort fait échouer la CI avant la fusion, pas
après.

Les blocs de code TypR ne sont pas encore vérifiés contre le compilateur : un
exemple peut devenir faux sans que rien ne le signale. C'est le prochain
chantier documentation.

## Versions

Personne ne modifie une version à la main. `Cargo.toml [workspace.package]
version` est la seule source de vérité ; `nu publish.nu sync` la propage à
l'extension VS Code et au runner RStudio, et le job `coherence` échoue en cas
d'écart. Voir [RELEASING.md](RELEASING.md).

## Binaires

Aucun binaire ne doit être commité. Ceux du runner RStudio sont assemblés par la
CI au moment de construire le tarball de release. Le job `coherence` échoue si
un binaire réapparaît dans l'index.
