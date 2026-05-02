# Debugging TypR Errors

## 1. Identifier l'étape fautive

Les trois étapes du pipeline sont dans `typr-core` :
- **Parsing** → `nom` + `nom_locate` → produit un `Lang`
- **Type-checking** → `TypeChecker` → produit `TypeError` / `SyntaxError`
- **Transpilation** → aussi dans `TypeChecker`

## 2. Outils principaux

**`FluentParser`** (`crates/typr-core/src/utils/fluent_parser.rs`) — principal outil de debug/test interactif :

```rust
let fp = FluentParser::new()
    .push("let x: Integer <- 5;")
    .parse_type_next()   // type-check l'expression
    .push("x")
    .parse_next();       // parse seulement

fp.get_last_type();      // récupère le type inféré
fp.get_last_lang();      // récupère le nœud AST
```

**Builders** (`crates/typr-core/src/utils/builder.rs`) — pour construire des types attendus dans les assertions :
```rust
assert_eq!(fp.get_last_type(), integer_type());
```

## 3. Workflow selon l'erreur

**Erreur de parsing** — écrire un test inline avec `"mon expression".parse::<Lang>()` et inspecter le `Lang` produit.

**Erreur de typage** — utiliser `FluentParser` + `parse_type_next()`, puis comparer avec le type attendu.

**Erreur de transpilation** — regarder dans `TypeChecker` (`crates/typr-core/src/processes/type_checking/type_checker.rs`), isoler l'expression dans un test et inspecter la sortie transpilée.

## 4. Lancer les tests ciblés

```bash
# Tester un module spécifique
cargo test -p typr-core type_checking::

# Tester par nom (substring)
cargo test -p typr-core mon_test

# Voir la sortie complète (pas de capture)
cargo test -p typr-core mon_test -- --nocapture
```

## 5. Tests laboratoire avec `lab/`

Le dossier `lab/` à la racine du projet est prévu pour les fichiers de test ad hoc. Il est préférable de faire ses expériences ici plutôt que dans les tests Rust, pour tester rapidement des extraits de code TypR sans polluer le projet.

```bash
# Créer un fichier de test dans lab/
# lab/test.typr

# Lancer le type-checker dessus
typr check lab/test.typr
```

Cela génère un fichier `context.json` dans le répertoire courant.

## 6. Utiliser `context.json` pour inspecter le contexte de typage

La commande `typr check [filename]` génère un fichier `context.json` qui contient la sérialisation complète du contexte de typage après analyse. Ce fichier est la radiographie du type-checker sur ton code.

### Structure de `context.json`

```json
{
  "typing_context": {
    "variables": [...],   // variables définies par l'utilisateur : [(nom, type), ...]
    "aliases":   [...],   // alias de types
    "std":       [...]    // fonctions/variables de la stdlib
  },
  "subtypes": {           // graphe de sous-typage
    "memory": [...],
    "root": {...},
    "subtype_cache": {...}
  },
  "config": {
    "environment": "Project",
    "file_type": "...",
    "target_language": "R"
  }
}
```

### Extraire des informations avec `jq`

```bash
# Lister toutes les variables utilisateur avec leur type
jq '.typing_context.variables' context.json

# Filtrer une variable spécifique
jq '.typing_context.variables[] | select(.[0] == "ma_var")' context.json

# Lister uniquement les noms des variables
jq '[.typing_context.variables[][0]]' context.json

# Lister les fonctions de la stdlib
jq '.typing_context.std' context.json

# Inspecter le graphe de sous-typage
jq '.subtypes.memory' context.json
```

### Extraire des informations avec Nushell

```nu
# Charger le contexte
let ctx = (open context.json)

# Lister les variables utilisateur
$ctx.typing_context.variables

# Filtrer une variable par nom
$ctx.typing_context.variables | where $it.0 == "ma_var"

# Voir tous les noms de variables
$ctx.typing_context.variables | each { |it| $it.0 }

# Voir les types de la stdlib
$ctx.typing_context.std | select 0 1

# Compter le nombre de variables
$ctx.typing_context.variables | length
```

## 7. Hiérarchie d'erreurs

- `TypRError` → wraps `TypeError` ou `SyntaxError`
- `ErrorCollector` accumule plusieurs erreurs (pas fail-fast)
- `TypeError` : 13 variantes dans `crates/typr-core/src/components/error_message/type_error.rs`
- `SyntaxError` : variantes dans `crates/typr-core/src/components/error_message/syntax_error.rs`
