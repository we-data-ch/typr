# Ajout de validateurs de types dans TypR

Ce document explique comment le système de validation `as!` a été implémenté et comment l'étendre
pour d'autres types.

## Vue d'ensemble

L'opérateur `as!` est un cast validant : `expr as! TypeName` compile vers `.validate_TypeName(expr)`.
La fonction `.validate_TypeName` est générée automatiquement à la transpilation pour chaque type
qui supporte la validation.

```typr
type Person <- list { name: char, age: int };
let raw <- list { name = "Alice", age = 30 };
let person <- raw as! Person;   // → .validate_Person(raw)
```

```r
# Généré automatiquement :
Person <- function(name, age) {
  structure(list(name = name, age = age), class = c("Person", "list"))
}
.validate_Person <- function(x) {
  required_fields <- c("name", "age")
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    stop(paste0("Validation failed for type Person: missing fields: ",
                paste(missing_fields, collapse = ", ")))
  }
  Person(name = x[["name"]], age = x[["age"]])
}
```

---

## Les 5 points de contact dans le code

### 1. L'opérateur — `components/language/operators.rs`

L'opérateur `Op::AsExcl` est ajouté à l'enum `Op` et configuré avec :

- **Binding power 4** (même que `|>`, `.`, `$`) dans `get_binding_power()` — `x + y as! T`
  parse comme `x + (y as! T)`.
- **`combine()` spécial** : au lieu de créer `Lang::Operator`, crée `Lang::ValidatingCast` en
  extrayant le nom du type depuis la partie droite (`Lang::Variable { name }`).

```rust
// operators.rs — get_binding_power()
Op::AsExcl(_) => 4,

// operators.rs — combine()
pub fn combine(self, left: Lang, right: Lang) -> Lang {
    if let Op::AsExcl(_) = self {
        let type_name = match &right {
            Lang::Variable { name, .. } => name.clone(),
            _ => "Unknown".to_string(),
        };
        return Lang::ValidatingCast {
            expression: Box::new(left.clone()),
            type_name,
            help_data: left.get_help_data(),
        };
    }
    // ... cas normal
}
```

### 2. Le nœud AST — `components/language/mod.rs`

Un nouveau variant `Lang::ValidatingCast` est ajouté à l'enum `Lang` :

```rust
ValidatingCast {
    expression: Box<Lang>,
    type_name: String,
    help_data: HelpData,
},
```

Il faut aussi mettre à jour les 4 impls qui matchent exhaustivement `Lang` :
- `PartialEq` : comparer `expression` et `type_name`, ignorer `help_data`
- `get_help_data()` : retourner `help_data`
- `simple_print()` : retourner `format!("ValidatingCast({})", type_name)`
- `From<Lang> for HelpData` : retourner `help_data`

### 3. Le parser — `processes/parsing/elements.rs`

**Point critique** : `variable2` consomme les caractères alphanumériques, donc `as` serait parsé
comme une variable avant que `as!` puisse être reconnu.

La solution : ajouter un parser dédié `as_excl_operator_token` qui tente `tag("as!")` en
**premier** dans la liste `alt()` de la fonction `elements()`.

```rust
fn as_excl_operator_token(s: Span) -> IResult<Span, LangToken> {
    let res = terminated(tag("as!"), multispace0).parse(s);
    match res {
        Ok((s, tok)) => Ok((s, LangToken::Operator(Op::AsExcl(tok.into())))),
        Err(r) => Err(r),
    }
}

pub fn elements(s: Span) -> IResult<Span, Lang> {
    let res = many1(alt((
        as_excl_operator_token,   // ← DOIT être en premier
        single_element_token,
        element_operator_token,
    ))).parse(s);
    // ...
}
```

> **Règle générale** : tout opérateur dont le préfixe est un identificateur valide (lettres) doit
> être parsé via un token dédié placé avant `single_element_token` dans `elements()`.

### 4. Le type-checker — `processes/type_checking/mod.rs`

La fonction `typing()` doit gérer `Lang::ValidatingCast`. La sémantique statique : le type
résultant est toujours `Type::Alias(type_name)`, quelle que soit la valeur de l'expression.

```rust
Lang::ValidatingCast {
    expression,
    type_name,
    help_data: h,
} => {
    let expr_tc = typing(context, expression);
    let alias_type = Type::Alias(type_name.clone(), vec![], false, h.clone());
    TypeContext::new(alias_type, expr.clone(), context.clone())
        .with_errors(expr_tc.errors)
}
```

Les erreurs de l'expression intérieure sont propagées via `.with_errors(expr_tc.errors)`.

### 5. La transpilation — `processes/transpiling/mod.rs`

Deux endroits à modifier :

**A. Génération du validateur** dans le bras `Lang::Alias { type: Type::Record }` — juste
après la génération du constructeur :

```rust
let validator = format!(
    ".validate_{name} <- function(x) {{\n\
     \  required_fields <- c({fields_quoted})\n\
     \  missing_fields <- setdiff(required_fields, names(x))\n\
     \  if (length(missing_fields) > 0) {{\n\
     \    stop(paste0(\"Validation failed for type {name}: missing fields: \",\n\
     \                paste(missing_fields, collapse = \", \")))\n\
     \  }}\n\
     \  {name}({field_access})\n\
     }}"
);
(format!("{constructor}\n{validator}"), cont.clone())
```

Où `fields_quoted` est la liste des noms entre guillemets (`"name", "age"`) et `field_access`
est `name = x[["name"]], age = x[["age"]]`.

**B. Transpilation de l'expression** dans un nouveau bras `Lang::ValidatingCast` :

```rust
Lang::ValidatingCast { expression, type_name, .. } => {
    let expr_r = expression.to_r(cont).0;
    (format!(".validate_{}({})", type_name, expr_r), cont.clone())
}
```

---

## Étendre la validation à d'autres types

### Types union (`type Shape <- .Circle(num) | .Square(num)`)

Le validateur devrait vérifier que la valeur a la bonne classe S3 :

```r
.validate_Shape <- function(x) {
  if (!inherits(x, "Shape")) {
    stop(paste0("Validation failed: expected Shape, got class: ", paste(class(x), collapse = "/")))
  }
  x
}
```

**Où ajouter** : dans le bras `Type::Operator(_, _, _, _)` de `Lang::Alias` dans
`processes/transpiling/mod.rs` (vers la ligne 914), après la génération des constructeurs
de variants.

### Types primitifs (`type Meters <- int`)

Un validateur pour un alias primitif pourrait vérifier le type R sous-jacent :

```r
.validate_Meters <- function(x) {
  if (!is.integer(x)) stop("Validation failed: expected integer for Meters")
  x
}
```

**Où ajouter** : dans le bras `_ =>` (catch-all) de `Lang::Alias` dans
`processes/transpiling/mod.rs`. Aujourd'hui ce bras retourne `("".to_string(), cont.clone())`.

### Interfaces

Les interfaces n'ont pas de représentation R directe — un validateur pourrait vérifier que
les méthodes requises existent sur l'objet (via `exists()` ou `hasMethod()`).

---

## Tests à ajouter

Pour chaque nouveau validateur, ajouter dans `processes/transpiling/mod.rs` :

```rust
#[test]
fn test_alias_MYTYPE_generates_validator() {
    let r_code = FluentParser::new()
        .check_transpiling("type MyType <- ...");
    let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
    assert!(r_str.contains(".validate_MyType <- function(x)"), "got: {}", r_str);
}

#[test]
fn test_validating_cast_mytype() {
    let r_code = FluentParser::new()
        .push("type MyType <- ...;")
        .run()
        .check_transpiling("expr as! MyType");
    let r_str = r_code.iter().cloned().collect::<Vec<_>>().join("\n");
    assert!(r_str.contains(".validate_MyType(expr)"), "got: {}", r_str);
}
```

Et mettre à jour le snapshot dans `tests/snapshots/` si le type concerné fait partie des
snapshot tests (`cargo insta accept`).
