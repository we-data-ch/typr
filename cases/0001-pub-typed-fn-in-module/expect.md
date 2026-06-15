# @pub fn typée dans un module → R cassé + document() échoue

## Ce qui DEVRAIT se passer

`module M { @pub let f <- fn(): int { 42 }; }` doit générer, dans `R/<mod>.R`, le même schéma
que le top-level :

- `#' @method f` (tag présent)
- `` `f` <- … `` (binding au type concret, **pas** `` `f`.default ``)
- l'export du module référence un symbole réellement défini (`M$f <- f`, et `f` existe)
- `typr build` va jusqu'au bout : `document()` réussit.

## Anomalies originales (R fautif, voir observed.txt)

1. `` `g`.default `` au lieu de `` `g` `` — branche `Type::Any | Type::Generic` prise à tort.
2. `#' @method` manquant sur la branche `.default`.
3. `leaf$g <- g` référence un `g` non défini (seul `` `g`.default `` existait) → erreur runtime.

## Localisation

`crates/typr-core/src/processes/transpiling/mod.rs`, branche `Lang::Let` (~ligne 869) et
branche `Lang::Module` (~ligne 1420-1445). Détail complet dans l'ancien `bug.md`.

## Statut

Corrigé dans le build courant (juin 2026) : le R généré est désormais conforme. Conservé comme
cas de non-régression (golden sur `R/leaf.R`).
