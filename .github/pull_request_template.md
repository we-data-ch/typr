## Ce que fait cette PR

<!-- Une ou deux phrases. -->

## Vérifications

- [ ] `cargo test --workspace` passe en local
- [ ] `cargo fmt --all` appliqué
- [ ] `typr case run` ne montre aucun `REGRESS`

## Si cette PR touche le parseur ou le typage

- [ ] Un cas a été ajouté dans `cases/` (`typr case add`) pour le comportement
      corrigé, ou une raison de ne pas en ajouter est donnée ci-dessous

## Si cette PR est une RFC

- [ ] Le fichier est `rfcs/0000-<slug>.md` (le numéro est attribué à la fusion,
      c'est celui de cette PR) et suit `rfcs/0000-template.md`

Une RFC ne se relit pas comme du code : les fils de commentaires se posent sur
les phrases. Évite de force-pusher par-dessus une relecture en cours. Voir
[rfcs/README.md](../rfcs/README.md).

## Si cette PR change une version

Les versions ne se modifient pas à la main : `nu publish.nu bump <niveau>` puis
`nu publish.nu sync`. Le job `coherence` échoue sinon. Voir `RELEASING.md`.
