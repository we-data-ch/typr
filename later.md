# Cleanup Backlog

## Critique — risques de panic en prod

### `get_related_functions()` — todo!() appelé depuis prod
- **Fichier**: `crates/typr-core/src/components/context/vartype.rs:460-462`
- **Appelant**: `components/context/mod.rs:723-724`
- Implémenter ou supprimer la fonction et son site d'appel.

### 12 `todo!()` dans du code non-test
Branches non implémentées qui paniquent à l'exécution si atteintes :

| Fichier | Ligne |
|---------|-------|
| `components/language/var.rs` | 452 |
| `components/language/var_function.rs` | 19 |
| `components/language/operators.rs` | 292 |
| `components/context/mod.rs` | 753 |
| `processes/parsing/mod.rs` | 152, 495, 590, 1060 |
| `processes/parsing/elements.rs` | 1065 |
| `processes/parsing/types.rs` | 852, 861 |
| `components/type/mod.rs` | 202 |

Pour chacun : implémenter le bras ou supprimer le chemin mort.

### `execute_r_with_path` — fonction morte doublonnée
- **Fichier**: `crates/typr-cli/src/io.rs:34-57`
- `execute_r_with_path2` (lignes 59-84) est la seule utilisée (appelée depuis `repl.rs:487`).
- Supprimer `execute_r_with_path` ou fusionner les deux.

---

## Haut — qualité du code

### `#![allow(dead_code)]` globaux à supprimer
Ces suppressions fichier-entier masquent du vrai code mort. Les retirer et corriger les warnings réels :

| Fichier |
|---------|
| `processes/parsing/mod.rs` |
| `components/type/mod.rs` |
| `crates/typr-cli/src/io.rs` |
| `crates/typr-cli/src/engine.rs` |
| `components/language/var.rs` |
| `components/context/vartype.rs` |
| `processes/type_checking/mod.rs` |

### `typing()` — fonction monolithique de 2 847 lignes
- **Fichier**: `crates/typr-core/src/processes/type_checking/mod.rs:849-3696`
- 92 bras de match dans un seul bloc — difficile à lire, tester, maintenir.
- Extraire en helpers : `typing_literal()`, `typing_operator()`, `typing_function_app()`, `typing_control_flow()`, etc.

### `push_alias` / `push_alias2` — nommage confus
- **Fichier**: `crates/typr-core/src/components/context/vartype.rs:358-379`
- Renommer en `push_alias_from_string` / `push_alias_from_var`, ou unifier.

### `load_typed_r/js` — DRY violation
- **Fichier**: `crates/typr-core/src/components/context/vartype.rs:520-545`
- 4 fonctions de chargement quasi-identiques. Extraire un helper paramétré.

### TODOs incomplets laissés dans le code
| Fichier | Ligne | Contenu |
|---------|-------|---------|
| `components/context/mod.rs` | 803 | "TODO: Differentiate between pushing variable and…" |
| `processes/type_checking/mod.rs` | 2131 | "TODO add js subcontext" |
| `processes/transpiling/mod.rs` | 1312 | "TODO get js context from memory" |
| `components/type/mod.rs` | 226 | "TODO: Fix this" |

---

## Moyen — robustesse & organisation

### `unwrap()` dans du code CLI non-test
Remplacer par propagation d'erreur (`?`) ou gestion explicite :

| Fichier | Lignes | Contexte |
|---------|--------|---------|
| `crates/typr-cli/src/engine.rs` | 27-28 | Création/écriture de fichier |
| `crates/typr-cli/src/project.rs` | 366, 369, 413, 416, 427-428, 435, 444 | Opérations fichier |
| `crates/typr-cli/src/repl.rs` | 465 | Résultat parser |
| `crates/typr-cli/src/standard_library.rs` | 88 | `find('@')` sur une ligne de signature |
| `crates/typr-core/src/components/context/vartype.rs` | 144-145 | Opérations de chargement |

### 130+ fonctions de test inline dans `type_checking/mod.rs`
- Les déplacer dans un bloc `#[cfg(test)] mod tests { }` en fin de fichier, ou dans des fichiers de test séparés.

---

## Low — WASM stubs

### Pattern `#[cfg(feature = "wasm")]` répété ~16 fois
- **Fichiers concernés** : `vartype.rs:490-561`, `processes/type_checking/mod.rs:146-186`
- Même paire de fonctions redéfinie pour chaque cible. Candidat à une abstraction trait (`FileIO` avec implémentation fichier vs mémoire).
