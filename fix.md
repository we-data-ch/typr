# Analyse des faux positifs potentiels dans TypR

Endroits où du code TypR correct pourrait être rejeté à tort ou provoquer un comportement incorrect.

---

## Stage 1 — Type-checking

### [FORT] Bras `_` qui retourne `Any` pour 9 variantes `Lang` non traitées
**Fichier** : `crates/typr-core/src/processes/type_checking/mod.rs:2042`

Le match principal de `typing()` finit par `_ => builder::any_type()` pour les variantes :
`Comment`, `Exp`, `GenFunc`, `Import`, `KeyValue`, `ModuleImport`, `Test`, `Use`, `VecFunctionApp`.

Ces constructions sont acceptées silencieusement avec le type `Any` au lieu d'être traitées
correctement — masque potentiellement des erreurs réelles en aval.

```rust
// Situation actuelle
_ => builder::any_type().with_lang(expr, context).into(),
```

---

### [FORT] `FunctionType::try_from(...).unwrap()` dans la transpilation de fonctions
**Fichier** : `crates/typr-core/src/processes/transpiling/mod.rs:442, 518, 573`

Si le type inféré n'est pas une fonction valide (dû à un bug en amont dans le type-checker),
`.unwrap()` panique plutôt que de retourner une erreur propre.

```rust
// Situation actuelle
let fn_type = FunctionType::try_from(typing(cont, self).value.clone()).unwrap();
```

---

## Stage 2 — Transpilation

### [FORT] Bras `_` qui retourne `""` pour `Import`, `Test`, `Use`
**Fichier** : `crates/typr-core/src/processes/transpiling/mod.rs:1235-1238`

Ces trois variantes tombent dans un bras par défaut qui fait un `println!` de debug et retourne
une chaîne vide — du code R silencieusement invalide est généré sans aucune erreur remontée.

```rust
// Situation actuelle
_ => {
    println!("This language structure won't transpile: {:?}", self);
    ("".to_string(), cont.clone())
}
```

**Variantes affectées** :
- `use Module::*;` → transpilé à chaîne vide
- `test { ... }` → transpilé à chaîne vide
- `import Type` → transpilé à chaîne vide

---

### [FORT] `ArrayIndexing` retourne `""` pour les types non-`Vec`
**Fichier** : `crates/typr-core/src/processes/transpiling/mod.rs:612`

Si le type inféré pour un indexage de tableau n'est pas `Type::Vec`, la transpilation retourne
une chaîne vide sans émettre d'erreur.

```rust
// Situation actuelle
let res = match typ {
    Type::Vec(_, _, _, _) => format!("{}[[{}]]", exp_str, val_str),
    _ => "".to_string(),  // code R silencieusement perdu
};
```

---

### [MOYEN] `ArrayType::try_from(...).unwrap()`
**Fichier** : `crates/typr-core/src/processes/transpiling/mod.rs:670, 1217`

Même pattern que le point précédent — panique si le type n'est pas ce qui est attendu,
au lieu d'une erreur explicite.

```rust
// Situation actuelle
let dimension = ArrayType::try_from(typ.clone()).unwrap().get_shape();
```

---

## Stage 3 — Parsing

### [FORT] `panic!` dans `scope()` pour les scopes vides
**Fichier** : `crates/typr-core/src/processes/parsing/elements.rs:1219`

Un scope vide `{}` provoque une panique au lieu d'une erreur de syntaxe propre ou d'une
valeur neutre.

```rust
// Situation actuelle
Ok((_s, (_, None, _))) => panic!("Error: the scope shouldn't be empty"),
```

---

### [FAIBLE] Ordre dans `variable_recognizer()` : `pascal_case_2` avant `variable_exp_2`
**Fichier** : `crates/typr-core/src/processes/parsing/elements.rs:654`

L'ordre `alt((quoted_variable, pascal_case_2, variable_exp_2))` pourrait dans certains cas
limites causer une consommation partielle incorrecte sur des noms commençant par une majuscule.

---

### [FAIBLE] Consommation du whitespace dans `argument_val()`
**Fichier** : `crates/typr-core/src/processes/parsing/elements.rs:315`

L'utilisation de `opt(terminated(tag(","), multispace0))` pourrait créer des conflits dans
certains cas limites avec des virgules sans espace (`key=value,next`).

---

## Résumé

| # | Fichier | Ligne | Stage | Confiance |
|---|---------|-------|-------|-----------|
| 1 | `type_checking/mod.rs` | 2042 | Type-checking | FORT |
| 2 | `transpiling/mod.rs` | 442, 518, 573 | Transpilation | FORT |
| 3 | `transpiling/mod.rs` | 1235-1238 | Transpilation | FORT |
| 4 | `transpiling/mod.rs` | 612 | Transpilation | FORT |
| 5 | `parsing/elements.rs` | 1219 | Parsing | FORT |
| 6 | `transpiling/mod.rs` | 670, 1217 | Transpilation | MOYEN |
| 7 | `parsing/elements.rs` | 654 | Parsing | FAIBLE |
| 8 | `parsing/elements.rs` | 315 | Parsing | FAIBLE |

## Recommandations par priorité

1. **Immédiat** — Traiter explicitement les 9 variantes `Lang` manquantes dans `typing()` plutôt que le bras `_` par défaut
2. **Immédiat** — Remplacer le `panic!` dans `scope()` par une erreur de syntaxe propre
3. **Court terme** — Ajouter la transpilation de `Import`/`Test`/`Use` ou émettre une vraie erreur
4. **Court terme** — Remplacer les `.unwrap()` sur les conversions de types par des erreurs explicites
