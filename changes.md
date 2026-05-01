# TypR - Améliorations pour le Debugging et la Maintenance

## Métriques Actuelles

- **typr-core** : ~7500 lignes de code
- **type_checking/mod.rs** : 2126 lignes (fichier monolithique)
- **291 cas** de `.unwrap()`, `panic!()`, `expect()` dans le code de production

---

## Phase 1 : Améliorer le Debugging (Immédiate)

### Problème : Utilisation Excessive de `unwrap()/panic!()`

**Fichiers les plus touchés :**
- `parsing/types.rs` : ~60 panic! (pire fichier)
- `type_checking/mod.rs` : ~40 unwrap/panic
- `type/mod.rs` : ~10 panic!
- `function_application.rs` : ~10 unwrap

**Action** : Remplacer les `panic!()` et `.unwrap()` par `TypRError` ou `Result<T, TypRError>`

**Exemple de transformation :**
```rust
// AVANT (panick à mort)
let field = fields.iter().next().unwrap().clone();

// APRÈS (avec contexte)
let field = fields.iter().next()
    .ok_or_else(|| TypRError::type_error(
        TypeError::FieldNotFound { 
            field: name.clone(), 
            expected_fields: fields.iter().map(|f| f.name.clone()).collect()
        }
    ))?;
```

### Problème : Messages d'Erreur de Qualité Inégale

**Typos à corriger :**
- `type_error.rs:31,184` : "recieved" → "received"
- `mod.rs:34` : "Unknonw" → "Unknown"

**Display format à améliorer :**
- `type_error.rs:353` : `format!("{:?}", msg)` produit du debug Rust brut
- → Utiliser `msg.display()` directement

**Action** : Créer une méthode `TypRError::with_context()` qui inclut :
- Pile d'appel (via `std::backtrace::Backtrace`)
- Type attendu vs type obtenu
- Position dans le source

---

## Phase 2 : Supprimer les Allow Globaux (Court terme)

### Problème : Directive `#![allow()]` Globale

**Emplacement :** `type_checking/mod.rs:1-7`

```rust
#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
```

**Impact** : Cache ~100+ warnings, empêche la détection de :
- Code mort (`dead_code`)
- Variables inutilisées (`unused_variables`)
- Imports inutiles (`unused_imports`)

**Plan progressif :**
1. Supprimer `unused_imports` → corriger chaque import
2. Supprimer `unused_variables` → nettoyer les variables ou ajouter `_`
3. Supprimer `dead_code` → comprendre si le code est nécessaire
4. Garder `unreachable_code` si justifié

---

## Phase 3 : Restructuration (Moyen terme)

### 3.1 Découper `type_checking/mod.rs` (2126 lignes)

**Structure proposée :**
```
type_checking/
├── mod.rs              (200 lignes - exports + TypingResult)
├── typing_engine.rs    (400 lignes - eval() principal)
├── match_handlers/     (NOUVEAU DOSSIER)
│   ├── literal.rs      (Number, Integer, Bool, Char, Null, NA, Empty)
│   ├── control_flow.rs (If, Match, WhileLoop, ForLoop)
│   ├── collections.rs  (Array, Vector, List, DataFrame, Tuple)
│   ├── functions.rs    (Function, Lambda, FunctionApp)
│   └── bindings.rs     (Let, Assign, Variable, Signature)
├── tests/
```

**Attention** : Les ~1000 lignes de tests inline devront être migrées.

### 3.2 Découper `language/mod.rs`

**Problème identifié** : ~50 variantes matchées 5+ fois pour :
- `get_help_data()`
- `simple_print()`
- `to_js()`
- `FromStr`
- Serialisation

**Solution** : Implémentations par groupe dans des fichiers séparés

**Structure proposée :**
```
language/
├── mod.rs              (200 lignes - exports)
├── literal.rs          (Number, Integer, Bool, Char, Null, NA, Empty)
├── control_flow.rs     (If, Match, WhileLoop, ForLoop)
├── data.rs             (Array, List, DataFrame, Tuple, Vector)
├── functions.rs        (Function, Lambda, FunctionApp, Return)
├── bindings.rs         (Let, Assign, Variable)
└── impls/
    ├── help_data.rs
    ├── display.rs
    └── transpile.rs
```

### 3.3 Extraire la Logique de Parsing

**Problème** : Duplication dans `elements.rs` et `types.rs`

**Solution** :

```
parsing/
├── mod.rs              (entry points)
├── combinators/
│   ├── literals.rs
│   ├── strings.rs     (double_quotes/single_quotes unifiés)
│   └── types.rs
└── precedence/
```

---

## Phase 4 : Améliorer la Gestion des Erreurs

### 4.1 Hiérarchie d'Erreurs Actuelle

```
TypRError (top-level enum)
├── TypeError (13 variants)
└── SyntaxError (4 variants)
```

### 4.2 Améliorations Recommandées

| Action | Priorité |\n|--------|----------|
| Ajouter variant pour "unexpected token" | Haute |
| Ajouter variant pour "type inference failed" | Haute |
| Unifier format "expected X, got Y" | Moyenne |
| Ajouter tests de rendu pour chaque variant | Moyenne |

### 4.3 Supprimer les Panic dans les Erreurs

**Problème** : `type_error.rs` utilise `panic!()` quand les données de fichier sont manquantes

```rust
// AVANT
panic!("Failed to read file: {}", source);

// APRÈS
Err(TypRError::syntax_error(SyntaxError::FileNotFound { 
    file_name: file_name.to_string() 
}))
```

---

## Phase 5 : Réduire la Duplication de Code

### 5.1 Parsing - `double_quotes` / `single_quotes`

**Emplacement** : `parsing/elements.rs:156-204`

**Problème** : Code quasi-identique

**Solution** : Créer une fonction通用 :

```rust
fn quoted_string<Quote: Copy>(input: Span, quote: Quote) -> ... 
where Quote: Fn(Span) -> bool;
```

### 5.2 Type Checking - Array/Vector/Sequence

**Emplacement** : `type_checking/mod.rs:1281-1434`

**Problème** : ~150 lignes quasi-identiques pour les 3 types

**Solution** : Extraire une fonction :

```rust
fn typing_container(
    context: &Context, 
    lang: &Lang, 
    container_type: VecType
) -> TypingResult;
```

---

## Liste des Fichiers à Modifier (Par Priorité)

### Haute Priorité (Debugging)

| Fichier | Action | Lignes |
|---------|--------|--------|
| `parsing/types.rs` | Supprimer ~60 panic! | 1100 |
| `type_checking/mod.rs` | Supprimer ~40 unwrap/panic | 2126 |
| `type/mod.rs` | Supprimer ~10 panic! | 1600 |
| `components/error_message/*.rs` | Corriger typos, améliorer messages | 400 |

### Moyenne Priorité (Restructuration)

| Fichier | Action | Lignes |
|---------|--------|--------|
| `type_checking/mod.rs` | Découper en modules | 2126 |
| `language/mod.rs` | Découper par groupe | ~1500 |
| `parsing/mod.rs` | Séparer entry/combinators | 900 |

### Basse Priorité (Refactor)

| Fichier | Action | Lignes |
|---------|--------|--------|
| Supprimer `#![allow()]` | Progressif, fichier par fichier | - |
| Extraire tests inline | Vers dossier tests/ | ~1000 |

---

## Questions en Attente de Réponse

1. **Type hints** : Certain `impl` block pourraient bénéficier de types de retour explicites — OK ?

2. **API publique** : Convertir `.unwrap()` en `Option`/`Result` pour plus de sécurité ?

3. **Tests inline** : Les déplacer dans un dossier `tests/` séparé ?

4. **WASM** : Vérifier compatibilité après changements majeur ?

---

## Métriques Cibles (Après Améliorations)

| Métrique | Actuel | Cible |
|---------|-------|-------|
| `.unwrap()` / `panic!()` | 291 | < 20 |
| Lignes par fichier max | 2126 | < 600 |
| `#![allow(...)]` | 5 fichiers | 0 |
| Code dupliqué (Array/Vector/Sequence) | ~150 lignes | 0 |