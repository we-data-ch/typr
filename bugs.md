# Bugs et fragilités relevés — session du 2026-11-05

> Notes personnelles, complément de `lacunes.md` et `decisions.md`.
> Méthode : lecture de code + `typr check` via le serveur MCP
> (`Environment::Wasm`), dépôt `develop`, version `0.6.2`.
>
> Statuts : `reproduit` (snippet exécuté), `confirmé par lecture`,
> `latent` (contrat violé mais chemin non démontré).
> Sévérités : `élevée` (soundness ou contrat), `moyenne` (résultat faux ou
> refus injustifié), `faible` (gêne, maintenance).

## Corrections appliquées (2026-11-05)

B-01 à B-09 traités, non commités. Tests : `crates/typr-core/tests/subtyping_soundness.rs`
(B-01/02/03) et `doc_attach::tests` (B-09). Notes de mise en œuvre :

- **B-02/B-03** : l'arm fonction de `is_subtype_raw` utilise maintenant
  `is_subtype_raw` (paramètres contravariants, retour covariant) au lieu de
  `strict_subtype`, qui ne reste que pour ses propres fonctions imbriquées
  (paramètres inversés aussi).
- **B-05** : `PartialEq` compare les noms des rigides (rigide ≠ générique
  souple) ; le `Hash` est inchangé car les champs de record en dépendent.
- **B-06** : `type_contains_generic` couvre les quatre familles
  (`same_generic_key`) et est branché sur l'arm `KindedGen`.
- **B-07** : seulement documenté (ce n'est pas un ordre, ne pas trier avec).
- **B-09** : l'apostrophe est retirée dans `doc_attach` (le R généré garde `#'`).
- Test instable préexistant, sans lien : `registry_validate::formals_diff_flags_a_missing_export_and_an_arity_mismatch`.

## Résumé

| ID | Titre | Sévérité | Statut |
|----|-------|----------|--------|
| B-01 | Le sous-typage des tuples ignore l'arité | élevée | reproduit |
| B-02 | Paramètres de fonction comparés en covariance | élevée | reproduit |
| B-03 | `strict_subtype` trop pauvre pour les composants de fonction | moyenne | reproduit |
| B-04 | `PartialEq` de `RClass` n'est pas symétrique | moyenne | confirmé par lecture |
| B-05 | Contrat `Hash`/`Eq` violé pour les `Generic` rigides | moyenne | confirmé par lecture |
| B-06 | L'occurs check ne couvre que `Generic` | faible | latent |
| B-07 | `PartialOrd for Type` n'est pas un ordre | faible | latent |
| B-08 | `typr document` ignore `typr.lock` | moyenne | confirmé par lecture |
| B-09 | `#'` laisse une apostrophe et désactive les tags | faible | confirmé par lecture |

---

## B-01 — Le sous-typage des tuples ignore l'arité

**Sévérité.** Élevée (soundness).
**Statut.** Reproduit le 2026-11-05 via `typr check`.

**Repro.**

```typr
let f <- fn(): tuple{int, int} { :{1, 2} };
let x: tuple{int, int, int} <- f();
```

**Observé.** `ok: true`, aucun diagnostic.
**Attendu.** `T001: Type mismatch: expected tuple{int, int, int}, got
tuple{int, int}`.

Le sens inverse passe aussi :

```typr
let f <- fn(): tuple{int} { :{1} };
let x: tuple{int, int} <- f();   // accepté
```

**Cause.** L'arm tuple de `is_subtype_raw` zippe les deux listes sans comparer
leurs longueurs :

`crates/typr-core/src/components/type/mod.rs:387-390`

```rust
(Type::Tuple(types1, _), Type::Tuple(types2, _)) => types1
    .iter()
    .zip(types2.iter())
    .all(|(typ1, typ2)| typ1.is_subtype_raw(typ2, context)),
```

`zip` s'arrête au plus court : les éléments en trop sont ignorés. La même
omission existe dans `PartialOrd for Type`
(`mod.rs:1540-1544`).

**Portée.** Tout sous-typage de tuples, donc les annotations `tuple{...}`, les
retours de fonction et les arguments. Un tuple trop court est accepté partout
où un tuple plus long est attendu, et réciproquement.

**Correctif suggéré.** Ajouter `types1.len() == types2.len() &&` avant le
`zip`, dans les deux fonctions. Ajouter un test unitaire avec des tuples de
longueurs différentes **et** des types d'éléments compatibles, sinon le bug
reste invisible (les repros ci-dessus passent justement parce que les éléments
présents sont compatibles).

---

## B-02 — Paramètres de fonction comparés en covariance

**Sévérité.** Élevée (soundness).
**Statut.** Reproduit le 2026-11-05 via `typr check`.

**Repro (dangereux, accepté).**

```typr
let f <- fn(x: int): int { x };
let g: (Any) -> int <- f;      // ok: true
```

`g` promet d'accepter `Any`, mais `f` n'accepte que `int`. Un appel
`g("x")` type-checke alors qu'il échouera à l'exécution.

**Repro (sûr, refusé).**

```typr
let f <- fn(x: Any): int { 1 };
let g: (int) -> int <- f;      // T001: Type mismatch
```

C'est exactement l'inverse de la règle attendue.

**Cause.** Pour `f <: g`, un paramètre doit être **contravariant** :
`g.param <: f.param`. Le code compare `f.param <: g.param` :

`crates/typr-core/src/components/type/mod.rs:319-326` (arm fonction de
`is_subtype_raw`) et `mod.rs:1211-1218` (`strict_subtype`) :

```rust
args1.len() == args2.len()
    && args1.iter().zip(args2.iter())
        .all(|(p1, p2)| p1.get_type().strict_subtype(&p2.get_type()))
    && ret_typ1.strict_subtype(ret_typ2)
```

Les paramètres sont comparés comme le retour (covariant). Le retour, lui, est
correct.

**Portée.** Limitée par B-03 : `strict_subtype` ne connaît que `Empty`, `Any`,
l'égalité, `<: Generic` et les fonctions imbriquées. Les cas réellement
acceptés à tort sont donc ceux qui passent par ces règles — ce qui suffit pour
`Any`, `Empty` et les génériques. Ne pas corriger B-03 avant B-02 : renforcer
`strict_subtype` rendrait la covariance exploitable sur les records, unions et
raffinements.

**Correctif suggéré.** Inverser la comparaison des paramètres
(`p2.strict_subtype(&p1)`), garder le retour covariant, puis ajouter les deux
repros en tests (le premier doit échouer, le second doit passer).

---

## B-03 — `strict_subtype` trop pauvre pour les composants de fonction

**Sévérité.** Moyenne (refus injustifié, pas unsound).
**Statut.** Reproduit le 2026-11-05 via `typr check`.

**Repro.**

```typr
let f <- fn(): list{a: int, b: char} { :{a = 1, b = "x"} };
let g: () -> list{a: int} <- f;   // T001
```

**Observé.** `T001: Type mismatch: expected fn() -> list{a: int}, got
fn() -> list{a: int, b: char}`.
**Attendu.** Accepté : `list{a:int,b:char} <: list{a:int}` par largeur de
record, et le retour est covariant.

**Cause.** `strict_subtype` n'a que cinq cas :

`crates/typr-core/src/components/type/mod.rs:1200-1221`

```rust
(Type::Empty(_), _) => true,
(_, Type::Any(_)) => true,
(typ1, typ2) if typ1 == typ2 => true,
(_, Type::Generic(_, _)) => true,
(Type::Function(..), Type::Function(..)) => ...
_ => false,
```

Il n'y a ni `Record`, ni `Union`, ni `Refined`, ni `Interface`, ni `Exact`.
Or `is_subtype_raw` utilise `strict_subtype` pour chaque paramètre et le
retour : tout ce que le sous-typage complet sait faire est perdu dès qu'on
entre dans une fonction.

**Portée.** Refus de fonctions pourtant valides ; l'utilisateur doit annoter
exactement la même forme. C'est cohérent avec le commentaire « volontairement
plus faible », mais incohérent avec le reste du système.

**Décision à prendre.** Soit assumer et documenter la limitation, soit
remplacer `strict_subtype` par `is_subtype_raw` (ou une variante réduite mais
complète) **après** avoir corrigé B-02. Les deux changements vont ensemble.

---

## B-04 — `PartialEq` de `RClass` n'est pas symétrique

**Sévérité.** Moyenne (cache de sous-typage, égalité de `Var`).
**Statut.** Confirmé par lecture.

**Cause.**

`crates/typr-core/src/components/type/mod.rs:1441`

```rust
(Type::RClass(el1, _), Type::RClass(el2, _)) =>
    el1.difference(el2).next().is_none(),
```

C'est `el1 ⊆ el2`, pas `el1 == el2`. Donc `A == B` peut être vrai et `B == A`
faux : avec `A = {a, b}` et `B = {a}`, `B == A` est vrai, `A == B` est faux.
`PartialEq` doit être symétrique.

**Impact.** `is_subtype` met en cache ses résultats dans
`HashMap<(Type, Type), bool>` (`mod.rs:239-251`). Selon l'ordre de comparaison
utilisé par la table, une requête `(B, X)` peut emprunter le résultat calculé
pour `(A, X)` si `B ⊆ A`. Le résultat n'est pas forcément valide pour `B` :
une **fausse négative** est possible (résultat `false` pour `A` réutilisé pour
un `B` qui, lui, serait accepté). Le sous-typage direct de `RClass` utilise
bien `is_subset` (`mod.rs:369`), donc le cache et la relation ne sont pas
alignés.

Le même `PartialEq` remonte dans `Var` (`crates/typr-core/src/components/language/var.rs:49-63`),
qui est `Eq` et utilisé dans des `HashSet`.

**Correctif suggéré.** Comparer les deux directions
(`el1 == el2`, ou `el1.is_subset(el2) && el2.is_subset(el1)`). Vérifier que
`Hash` reste cohérent (il hache seulement le discriminant aujourd'hui, donc
c'est bon).

---

## B-05 — Contrat `Hash`/`Eq` violé pour les `Generic` rigides

**Sévérité.** Moyenne (comportement de `HashSet`/`HashMap`).
**Statut.** Confirmé par lecture.

**Cause.** `PartialEq` rend **tous** les `Generic` égaux :

`crates/typr-core/src/components/type/mod.rs:1418`

```rust
(Type::Generic(_, _), Type::Generic(_, _)) => true,
```

Mais `Hash` fait participer le nom des rigids :

`mod.rs:1574-1580`

```rust
Type::Generic(name, _) if is_rigid_name(name) => {
    6.hash(state);
    name.hash(state);
}
Type::Generic(_, _) => 6.hash(state),
```

Deux rigids `__rigid_0` et `__rigid_1` sont donc **égaux** mais ont des hashes
**différents**, ce qui viole le contrat `k1 == k2 ⇒ hash(k1) == hash(k2)`.

**Impact.** `is_subtype` évite volontairement le cache dès qu'un rigid est
mentionné (`mod.rs:234-241`), donc le cache de sous-typage est protégé. Mais
tout autre `HashSet`/`HashMap` keyé par `Type` (ou par `Var`, dont le `Hash`
inclut `related_type`) peut contenir des doublons logiques ou rater une
entrée. `get_all_generic_functions` collecte par exemple des `(Var, Type)`
dans un `HashSet`.

**Correctif suggéré.** Choisir une seule sémantique :

- soit `PartialEq` compare les noms de rigids (et on garde le `Hash` actuel) ;
- soit `Hash` hache les rigids comme les autres génériques (et l'identité de
  rigid passe exclusivement par `same_generic_key`).

Le commentaire de `unification.rs` indique que l'égalité lâche des `Generic`
est encore utilisée par l'inférence des lambdas non typées : le changement doit
être accompagné de tests sur ces chemins.

---

## B-06 — L'occurs check ne couvre que `Generic`

**Sévérité.** Faible (latent).
**Statut.** Latent : aucun repro.

**Cause.** `type_contains_generic` parcourt la structure mais ne matche que
`Type::Generic` :

`crates/typr-core/src/processes/type_checking/unification.rs:195-219`

```rust
Type::Generic(g, _) => g == name,
...
_ => false,
```

`IndexGen`, `LabelGen` et `KindedGen` ne sont jamais détectés. De plus, le check
n'est branché que dans l'arm `Generic` de `unification_helper`
(`unification.rs:254-265`).

**Pourquoi c'est probablement bénin aujourd'hui.** Les arms `IndexGen` et
`LabelGen` ne lient qu'à `Integer` / `Char` concrets, donc une
auto-référence `#I ↦ [.., #I]` ne semble pas constructible par ces chemins.

**Risque résiduel.** L'arm `KindedGen` accepte **permissivement** tout type dont
`type_kind` vaut `None` (Vec, Tuple, Function, Alias…). En principe,
`%R ↦ [#N, %R]` pourrait donc être accepté et faire boucler la substitution.
À démontrer par un test ciblé avant de le classer en bug confirmé.

**Correctif suggéré.** Étendre `type_contains_generic` à
`same_generic_key`-style pour les quatre familles, ou au minimum ajouter un
test qui tente l'auto-référence kindée.

---

## B-07 — `PartialOrd for Type` n'est pas un ordre

**Sévérité.** Faible aujourd'hui (aucun appelant externe trouvé).
**Statut.** Latent.

**Cause.** `partial_cmp` est en réalité un test de sous-typage déguisé :

`crates/typr-core/src/components/type/mod.rs:1483-1556`

- `Empty` et `<: Any` renvoient `Less` ;
- un record plus large renvoie `Less`, le plus étroit renvoie `None` ;
- `RClass` renvoie `Less` si `set1 ⊆ set2`, `None` sinon ;
- `Vec`, `Function`, `Tuple` testent seulement `partial_cmp(...).is_some()`.

Il n'y a jamais de `Greater`, et l'antisymétrie est violée : `a < b` et
`b < a` peuvent tous deux être `Less` (ou l'un `Less`, l'autre `None`). Ce
n'est pas un ordre partiel conforme.

**Impact.** Aucun appelant externe de `Type::partial_cmp` n'a été trouvé (les
usages internes sont récursifs, et les tris observés portent sur des `String`
ou des noms). Le risque est donc de maintenance : un futur `sort_by` ou une
`BTreeMap<Type, _>` produirait des résultats incohérents.

**Correctif suggéré.** Renommer en `is_subtype`-like, ou implémenter un vrai
ordre structurel (par discriminant puis champs), ou supprimer l'implémentation
si elle n'est pas utilisée.

---

## B-08 — `typr document` ignore `typr.lock`

**Sévérité.** Moyenne (incohérence entre commandes).
**Statut.** Confirmé par lecture.

**Cause.** `document_impl` construit son contexte directement et n'appelle
jamais `load_project_type_definitions` :

`crates/typr-cli/src/project.rs:2064-2095`

```rust
let context = Context::default().set_environment(Environment::Project);
...
let type_checker = TypeChecker::new(context).typing_no_panic(&lang);
```

Les autres chemins (`check`, `build`, `run`) appellent
`load_project_type_definitions` (`project.rs:829, 863, 922, 1091, 1322, 1435,
1608`).

**Impact.** `typr document` ne voit que la stdlib : les noms des paquets
épinglés retombent en `UnknownFunction` (variadique, retour `Any`). Les `.Rd`
peuvent donc afficher des types `Any`/imprécis, et `document` peut accepter un
programme que `check` refuserait. La liste des pages ne change pas (le SPG ne
contient que les items du projet) ; c'est la **précision des types** et la
**vérification** qui diffèrent.

**Correctif suggéré.** Charger les définitions comme `build`/`check` avant
`TypeChecker::new`, ou documenter explicitement que `document` est
stdlib-only. Si le comportement est voulu, le rendre visible (warning) pour
éviter le piège.

---

## B-09 — `#'` laisse une apostrophe et désactive les tags

**Sévérité.** Faible (piège de documentation).
**Statut.** Confirmé par lecture.

**Cause.** Le parser ne retire que le premier `#` :

`crates/typr-core/src/processes/parsing/mod.rs:1036-1048`

```rust
let res = (tag("#"), not_line_ending, ...).parse(s);
...
Lang::Comment { value: txt.to_string(), ... }
```

`#' Titre` produit donc `' Titre`. `doc_attach` trim puis joint les lignes, et
`parse_doc` compare `trimmed.starts_with("@param")` etc.
(`crates/typr-cli/src/rd_renderer.rs:17-56`) : une ligne `#' @param x ...`
commence par `'`, donc **aucun tag n'est reconnu** et tout finit dans la
description.

**Impact.** Un auteur habitué à roxygen obtient une doc silencieusement
dégradée : titre préfixé par `'`, tags littéraux.

**Correctif suggéré.** Soit retirer aussi l'apostrophe quand le commentaire
commence par `#'`, soit émettre un warning dédié (`WrongCommentSyntax` existe
déjà pour `//`), soit documenter clairement que TypR n'utilise pas `#'`.

---

## Limitations connues (rapportées par les notes, non revérifiées ici)

Ces points viennent de `lacunes.md` §14 ; ils n'ont pas été reproduits pendant
cette session.

- Pas de nettoyage des `.Rd` et des fragments de vignette devenus obsolètes.
- Deux nœuds de même nom non qualifié écrivent le même `man/<nom>.Rd` : le
  dernier gagne silencieusement.
- Les nœuds privés sont filtrés des `.Rd` mais **pas** des fragments de
  vignette.
- Les métadonnées `StdlibMeta` ne nourrissent jamais les `.Rd`.
- `typr document` et `typr build` peuvent diverger sur les types (voir B-08).

## Pièges connus, pas des bugs

- **Génériques distincts non liés.** L'arm d'égalité
  `t1 == t2 => Some(vec![])` de `unification_helper` passe avant l'arm
  `Generic`, et `PartialEq` rend tous les `Generic` égaux : unifier `T` avec
  `U` renvoie `Some(vec![])` **sans lier** `T` à `U`. Le commentaire du code
  dit que plusieurs chemins d'inférence s'appuient encore dessus ; c'est un
  piège connu, pas un bug confirmé (mais il rejoint B-05).
- **`types vendor` ne nourrit pas le build** : décision assumée, voir
  `decisions.md` A.5.
- **Un replay de release repart du tag** : décision assumée, voir
  `decisions.md` C.4.
- **Les tags de doc inconnus finissent dans la description** : décision
  assumée, voir `decisions.md` D.1.

## Fausses pistes vérifiées

- **`to_interface` + `todo!()`** (`mod.rs:331-336`) : le `todo!()` n'est pas
  atteignable. `to_interface` renvoie soit l'interface elle-même, soit
  `builder::interface_type(...)` pour une fonction, soit
  `builder::interface_type2(...)` pour tout le reste — toujours un
  `Type::Interface`. Pas de panic trouvé.
- **`UnificationMap::append`** (`unification_map.rs`) : aucun appelant trouvé ;
  le risque de concaténation sans revalidation est théorique.
- **`Hash for Type::Function`** n'hache que le discriminant, pas les paramètres
  ni le retour : c'est légal (deux valeurs égales hachent pareil ; des valeurs
  différentes peuvent collisionner).
- **`Hash for Type::RClass`** n'hache que le discriminant : cohérent avec le
  `PartialEq` actuel, mais celui-ci reste asymétrique (B-04).

## Priorités suggérées

1. **B-02 puis B-01** : ce sont les deux atteintes à la soundness, et elles se
   corrigent indépendamment.
2. **B-03** : à traiter avec B-02, sinon on élargit la surface de B-02.
3. **B-08** : incohérence `document`/`build` très visible pour l'utilisateur.
4. **B-04, B-05** : contrats `Eq`/`Hash`, à corriger avec des tests de
   non-régression.
5. **B-06, B-07, B-09** : latents ou cosmétiques ; documenter en attendant.
