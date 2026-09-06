## Ce que fait cette PR

<!-- Une ou deux phrases. -->

## Vérifications

- [ ] `cargo test --workspace` passe en local
- [ ] `cargo fmt --all` appliqué
- [ ] `typr case run` ne montre aucun `REGRESS`

## Si cette PR touche le parseur ou le typage

- [ ] Un cas a été ajouté dans `cases/` (`typr case add`) pour le comportement
      corrigé, ou une raison de ne pas en ajouter est donnée ci-dessous

## Si cette PR change une version

Les versions ne se modifient pas à la main : `nu publish.nu bump <niveau>` puis
`nu publish.nu sync`. Le job `coherence` échoue sinon. Voir `RELEASING.md`.
