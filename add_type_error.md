# Ajouter une nouvelle erreur de typage (`TypeError`)

## Vue d'ensemble du mécanisme

Les erreurs de typage suivent un pipeline en 8 étapes :

```
Définition (enum)
    → Construction (type_checking/)
    → Accumulation (TypeContext)
    → Propagation (fonctions de typage)
    → HelpData (localisation dans le source)
    → simple_message() (LSP / messages rapides)
    → display() (rendu formaté avec miette)
    → TypRError (wrapper unifié)
```

---

## Étape 1 — Ajouter le variant dans `type_error.rs`

```rust
// crates/typr-core/src/components/error_message/type_error.rs

pub enum TypeError {
    // ... variants existants ...
    MonNouvelleErreur(Type, Type),          // deux positions (DoubleBuilder)
    // ou
    MonNouvelleErreur(HelpData),            // une position (SingleBuilder)
    // ou
    MonNouvelleErreur(Var),                 // via Locatable (get_help_data() sur Var)
}
```

**Règle générale :** stocker la donnée la plus riche disponible.
- `Type` / `Var` / `Lang` → préférer ces types car ils portent leurs propres `HelpData` via le trait `Locatable`.
- `HelpData` directement → quand on n'a pas d'objet typé (ex. : position brute du parser).

---

## Étape 2 — Implémenter `get_help_data()`

Ajouter un bras dans le `match` de `get_help_data()` :

```rust
pub fn get_help_data(&self) -> Option<HelpData> {
    match self {
        // ... bras existants ...
        TypeError::MonNouvelleErreur(t1, _) => Some(t1.get_help_data()),
        // Si HelpData directement :
        TypeError::MonNouvelleErreur(h) => Some(h.clone()),
    }
}
```

---

## Étape 3 — Implémenter `simple_message()`

Message texte sans accès fichier, utilisé par le LSP :

```rust
pub fn simple_message(&self) -> String {
    match self {
        // ... bras existants ...
        TypeError::MonNouvelleErreur(t1, t2) => {
            format!("Mon erreur : {} vs {}", t1.pretty(), t2.pretty())
        }
    }
}
```

---

## Étape 4 — Implémenter `display()` dans `impl ErrorMsg for TypeError`

### Erreur à une seule position → `SingleBuilder`

```rust
TypeError::MonNouvelleErreur(help_data) => {
    let (file_name, text) = help_data.get_file_data().unwrap_or_else(default_file_data);
    SingleBuilder::new(file_name, text)
        .pos((help_data.get_offset(), 0))
        .text("Description de l'erreur")
        .pos_text("Indication à la position")
        .help("Suggestion de correction (optionnel)")
        .build()
}
```

### Erreur à deux positions → `DoubleBuilder`

```rust
TypeError::MonNouvelleErreur(t1, t2) => {
    let help_data1 = t1.get_help_data();
    let help_data2 = t2.get_help_data();
    let (file_name1, text1) = help_data1.get_file_data().unwrap_or_else(default_file_data);
    let (file_name2, text2) = help_data2.get_file_data().unwrap_or_else(default_file_data);
    DoubleBuilder::new(file_name1, text1, file_name2, text2)
        .pos1((help_data1.get_offset(), 0))
        .pos2((help_data2.get_offset(), 1))
        .text(format!("type {} ne correspond pas à {}", t1.pretty(), t2.pretty()))
        .pos_text1(format!("Attendu : {}", t1.pretty()))
        .pos_text2(format!("Reçu : {}", t2.pretty()))
        .help("Suggestion (optionnel)")
        .build()
}
```

La valeur de retour de `display()` est `format!("{:?}", msg)` où `msg: Result<()>`.

---

## Étape 5 — Construire et émettre l'erreur dans le type-checker

Dans `crates/typr-core/src/processes/type_checking/` :

```rust
// Vérification échoue
if condition_erreur {
    errors.push(TypRError::Type(TypeError::MonNouvelleErreur(
        type_attendu.clone(),
        type_recu.clone(),
    )));
    // On continue l'analyse pour accumuler d'autres erreurs
}
```

### Flux d'accumulation standard

```rust
// Dans une fonction de typage :
fn typing(context: Context, expr: Lang) -> TypeContext {
    let mut errors: Vec<TypRError> = vec![];

    // ... analyse ...
    if some_check_fails {
        errors.push(TypRError::Type(TypeError::MonNouvelleErreur(...)));
    }

    TypeContext::new(inferred_type, expr, context)
        .with_errors(errors)
}
```

---

## Référence rapide : builders et types utiles

| Besoin                      | Builder         | Méthodes clés                                     |
|-----------------------------|-----------------|---------------------------------------------------|
| Une position dans le source | `SingleBuilder` | `.pos()`, `.text()`, `.pos_text()`, `.help()`     |
| Deux positions              | `DoubleBuilder` | `.pos1()`, `.pos2()`, `.pos_text1/2()`, `.help()` |

| Type portant `HelpData`  | Méthode                          |
|--------------------------|----------------------------------|
| `Type`                   | `.get_help_data()`, `.get_file_name_and_text()` |
| `Var`                    | `.get_help_data()`, `.get_file_name_and_text()` |
| `Lang`                   | `.get_help_data()`               |
| `HelpData`               | `.get_offset()`, `.get_file_data()` |

---

## Checklist

- [ ] Nouveau variant ajouté dans `enum TypeError`
- [ ] Bras ajouté dans `get_help_data()`
- [ ] Bras ajouté dans `simple_message()`
- [ ] Bras ajouté dans `display()` (dans `impl ErrorMsg for TypeError`)
- [ ] Erreur construite et pushée dans le type-checker concerné
- [ ] `cargo build` passe (exhaustivité des matchs vérifiée par le compilateur)
- [ ] `cargo test -p typr-core` passe
