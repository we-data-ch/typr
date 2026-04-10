# Ajouter une nouvelle erreur de syntaxe (`SyntaxError`)

## Différences avec `TypeError`

`SyntaxError` suit exactement le même mécanisme que `TypeError`, avec deux particularités :

1. **`HelpData` stocké directement** dans les variants (au lieu de `Type`/`Var`/`Lang`), car les erreurs de syntaxe surviennent dans le parser, avant que les types soient inférés.
2. **Variant `WithNode(Box<Lang>, Box<SyntaxError>)`** : wrapper qui attache un nœud AST à une erreur pour conserver le contexte de parsing. Il délègue toujours aux méthodes de l'erreur interne — ne pas l'oublier dans les matchs exhaustifs.

---

## Étape 1 — Ajouter le variant dans `syntax_error.rs`

```rust
// crates/typr-core/src/components/error_message/syntax_error.rs

pub enum SyntaxError {
    // ... variants existants ...
    MonNouvelleErreur(HelpData),
    WithNode(Box<Lang>, Box<SyntaxError>),  // toujours présent, ne pas toucher
}
```

---

## Étape 2 — Implémenter `get_help_data()`

```rust
pub fn get_help_data(&self) -> Option<HelpData> {
    match self {
        // ... bras existants ...
        SyntaxError::MonNouvelleErreur(h) => Some(h.clone()),
        SyntaxError::WithNode(_, inner) => inner.get_help_data(),  // délégation
    }
}
```

---

## Étape 3 — Implémenter `simple_message()`

```rust
pub fn simple_message(&self) -> String {
    match self {
        // ... bras existants ...
        SyntaxError::MonNouvelleErreur(_) => {
            "Description courte de l'erreur".to_string()
        }
        SyntaxError::WithNode(_, inner) => inner.simple_message(),  // délégation
    }
}
```

---

## Étape 4 — Implémenter `display()` dans `impl ErrorMsg for SyntaxError`

Même pattern que `TypeError` avec `SingleBuilder` (les erreurs de syntaxe ont généralement une seule position) :

```rust
SyntaxError::MonNouvelleErreur(help_data) => {
    let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
    SingleBuilder::new(file_name, text)
        .pos((help_data.get_offset(), 0))
        .text("Description de l'erreur de syntaxe")
        .pos_text("Here")
        .help("Suggestion de correction (optionnel)")
        .build()
}
// WithNode est déjà géré :
// SyntaxError::WithNode(_, inner) => return inner.display(),
```

La valeur de retour de `display()` est `format!("{:?}", msg)`.

---

## Étape 5 — Construire et émettre l'erreur dans le parser

Dans `crates/typr-core/src/processes/parsing/` :

```rust
// Cas simple : erreur seule
Lang::SyntaxErr(Box::new(SyntaxError::MonNouvelleErreur(span.into())))

// Cas WithNode : attacher l'AST partiel analysé avant l'erreur
Lang::SyntaxErr(Box::new(SyntaxError::WithNode(
    Box::new(parsed_so_far),
    Box::new(SyntaxError::MonNouvelleErreur(span.into())),
)))
```

`HelpData` se construit depuis un `LocatedSpan` (via `impl From<LocatedSpan<...>> for HelpData`) ou depuis un `Lang` (via `impl From<Lang> for HelpData` — utilise la position du premier nœud) :

```rust
let h: HelpData = span.into();       // depuis le parser nom
let h: HelpData = some_lang.into();  // depuis un nœud AST existant
```

---

## Différences structurelles avec `TypeError`

| Aspect                    | `TypeError`                            | `SyntaxError`                         |
|---------------------------|----------------------------------------|---------------------------------------|
| Données des variants      | `Type`, `Var`, `Lang` (Locatable)      | `HelpData` directement                |
| Serde                     | Non                                    | `Serialize`, `Deserialize`            |
| Variant spécial           | Aucun                                  | `WithNode` (contexte AST)             |
| Lieu de construction      | `processes/type_checking/`             | `processes/parsing/`                  |
| Obtention de `HelpData`   | `.get_help_data()` sur Type/Var/Lang   | `.clone()` direct                     |

---

## Checklist

- [ ] Nouveau variant ajouté dans `enum SyntaxError`
- [ ] Bras ajouté dans `get_help_data()`
- [ ] Bras ajouté dans `simple_message()`
- [ ] Bras ajouté dans `display()` (dans `impl ErrorMsg for SyntaxError`)
- [ ] Erreur construite dans le parser concerné (avec ou sans `WithNode`)
- [ ] `cargo build` passe (exhaustivité des matchs vérifiée par le compilateur)
- [ ] `cargo test -p typr-core` passe
