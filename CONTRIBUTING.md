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

## Proposer un changement de langage (RFC)

Une correction de bug se discute dans une issue. Un **changement de langage** —
une syntaxe nouvelle, une règle de typage, une autre forme de R engendré — passe
d'abord par une RFC : une proposition écrite, relue en PR, fusionnée dans
`rfcs/` si elle est acceptée. Le but n'est pas la cérémonie, c'est que le
*raisonnement* survive : six mois plus tard, « pourquoi TypR fait comme ça ? » a
une réponse écrite.

La frontière : **si la réponse à « que fait TypR ici ? » change, c'est une
RFC ; si le compilateur ne fait que rattraper une réponse déjà donnée, c'est une
issue.** Une idée encore floue va dans la catégorie
[Ideas](https://github.com/we-data-ch/typr/discussions/categories/ideas) des
Discussions — c'est souvent là qu'elle se règle, et sinon elle en sort meilleure.

Le processus complet, les étiquettes (`rfc-draft` / `rfc-accepted` /
`rfc-rejected`) et le gabarit sont dans [rfcs/README.md](rfcs/README.md). Les
notes de conception du workspace (`spécifications/`, `ai_context/*.md`) en sont
la matière première : une RFC, c'est leur face publique.

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

## Travailler sur la coloration syntaxique

Les grammaires d'éditeur sont **générées**. La source de vérité unique est le
manifeste de syntaxe, `crates/typr-core/src/components/syntax/mod.rs` : une
liste ordonnée de règles, une entrée par lexème que le parseur reconnaît
réellement.

```bash
typr syntax --json          # le manifeste
typr syntax --target tmlanguage
typr syntax --write         # régénère les grammaires sous editors/
typr syntax --check         # échoue si une grammaire commitée a dérivé
```

N'édite jamais `editors/vscode/syntaxes/typr.tmLanguage.json` à la main : le
fichier porte un en-tête qui le dit, et `cargo test -p typr-cli` le compare à
ce que le manifeste produit.

Deux tests gardent l'invariant dans les deux sens
(`components::syntax::tests`) :

- ajouter un `tag("…")` au parseur sans l'ajouter au manifeste fait échouer
  `every_parser_tag_is_in_the_manifest` ;
- mettre dans le manifeste un mot que le parseur ne connaît pas fait échouer
  `manifest_claims_no_word_the_parser_does_not_know`.

C'est le second qui manquait : les grammaires écrites à la main coloraient
`impl`, `trait`, `struct`, `enum`, `where`, `mut`, `Option`, `Result` —
copiés d'une grammaire Rust, inexistants en TypR — pendant que `opaque`,
`module`, `record`, `object`, `interface`, `typeconstructor`, `recursive`,
`embed`, `@export` et les sigils de kind n'étaient colorés nulle part.

## Travailler sur la documentation

La documentation vit dans un dépôt séparé : `we-data-ch/typr.github.io`.
Chaque PR y déclenche une construction complète du site — `onBrokenLinks` est
réglé sur `throw`, donc un lien mort fait échouer la CI avant la fusion, pas
après.

Chaque bloc ` ```typr ` du site est passé au vrai compilateur en CI
(`npm run check:examples`) : un exemple qui cesse de compiler bloque la
construction. L'oracle est la **dernière release** — la version que le lecteur a
réellement installée — et un second workflow rejoue la même chose chaque nuit
contre `develop`, en avertissement seulement. C'est le préavis « la prochaine
release va casser tel exemple » : quand il se déclenche, la PR du compilateur et
celle de la doc doivent partir ensemble.

## Versions

Personne ne modifie une version à la main. `Cargo.toml [workspace.package]
version` est la seule source de vérité ; `nu publish.nu sync` la propage à
l'extension VS Code et au runner RStudio, et le job `coherence` échoue en cas
d'écart. Voir [RELEASING.md](RELEASING.md).

## Binaires

Aucun binaire ne doit être commité. Ceux du runner RStudio sont assemblés par la
CI au moment de construire le tarball de release. Le job `coherence` échoue si
un binaire réapparaît dans l'index.
