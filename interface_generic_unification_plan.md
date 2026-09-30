# Génériques implicites `Interface@Id` — plan d'implémentation

Plan pour `interface_generic_unification2.md` (RFC), écrit après lecture de `typr-core` et sondes
contre le binaire (`target/debug/typr`, branche `develop`, état au 2026-09-30).

---

## 0. Ce qui existe déjà (et ce que les sondes révèlent)

| Élément du RFC | État actuel dans le code |
|---|---|
| Interface en paramètre → variable rigide contrainte | **Déjà en place dans le corps** : `type_checking/function.rs:326-340` crée un `Type::Generic("__rigid_N")` + `add_interface_constraint` **par paramètre**. Même mécanique dans `specialize_lambda` (`function_application.rs:1061`). |
| Élimination `e.m() : T[Self ↦ A]` | En place : `try_constrained_variable_match` (`function_application.rs:29`, FILTERING 0). |
| Instanciation au site d'appel | `try_interface_subtype_match` (`function_application.rs:624`) : table `interface_to_concrete` **indexée par l'interface réduite**, et c'est le premier argument qui gagne. |
| Retour interface sans paramètre (§7.3a) | Déjà une erreur : `TypeError::InterfaceReturnOnly` (`function.rs`, `is_interface_return_only`). |
| Sigil `@` en position de type | **Déjà pris** en préfixe : `@A` → `Type::KindedGen(Kind::Interface, "A")` (`parsing/types.rs:917`, RFC `sigils.md`). |
| Génériques explicites `T`, `U` | Fonctionnent correctement : `fn(a: T, b: U): U` appelé avec `(cat, dog)` rend `Dog`. |
| Interfaces paramétrées `interface<T>` (RFC §6.3) | **N'existent pas** (aucune occurrence dans le parseur). |

Les sondes (fichiers `*.ty` avec `Lovable`, `Cat`, `Dog`) montrent que **le corps et le site d'appel
appliquent deux sémantiques différentes** pour `fn(a: Lovable, b: Lovable)` :

- **Dans le corps, les variables sont distinctes** : `a : __rigid_0`, `b : __rigid_1`.
- **Au site d'appel, une seule variable est partagée**, et c'est le premier argument qui la fixe.

Défauts constatés, à capturer en `cases/` avant toute modification :

1. **Défaut de soundness** : `let second <- fn(a: Lovable, b: Lovable): Lovable { b }` puis
   `let r: Cat <- second(cat, dog)` **passe**, et `let r: Dog <- second(cat, dog)` est **rejeté**
   (« Received Cat »). Le retour est lié au premier argument, et `b: Dog` n'est jamais confronté
   à `a: Cat`.
2. **`Lovable@A` est silencieusement avalé** : `fn(a: Lovable@A, b: Lovable@B)` passe `typr check`,
   la signature s'affiche `fn(a: Lovable, b: Lovable) -> bool` et le R généré ne garde aucune trace
   de `@A`. Il faudra trouver quel parseur consomme `@A` sans le reporter.
3. **Méthode `(Self, Self)` sur deux rigides distincts acceptée** :
   `type Same <- interface { same: (Self, Self) -> bool }; fn(a: Same, b: Same): bool { a.same(b) }`
   passe, alors qu'avec des rigides distincts, `b : B` ne devrait pas satisfaire le paramètre `A`.
   À expliquer en phase 0 : soit `is_subtype_raw` entre deux rigides est trop permissif, soit un
   filtre de repli accepte l'appel.
4. `is_rigid_compatible` (`function.rs:244`) accepte **n'importe quel** rigide dont la borne est
   `Lovable` comme retour `Lovable`. C'est pour cela que `{ b }` et `{ a }` passent tous les deux.

---

## 1. Décisions de conception

### D1. Syntaxe : `I@Id` en suffixe, cohérent avec le sigil préfixe existant

- `Lovable@A` : variable générique `A`, bornée par `Lovable`. Le `@` suffixe ne se lit qu'**immédiatement
  après un nom d'alias**, sans espace. Il ne peut donc pas être confondu avec `@export`, `@pub` ou
  `@name: …;`, qui sont au niveau des déclarations.
- `@A` reste ce qu'il est (générique de *kind* interface, sans borne précise). Lecture unifiée :
  `@A` ≈ `interface{}@A`. Un même `A` dans `@A` et `Lovable@A` désigne **la même variable**, dont la
  borne est `Lovable`.
- `Lovable@_` (RFC §7.1) : identifiant anonyme, donc variable fraîche à chaque occurrence. C'est peu coûteux
  à ajouter, à inclure.
- `Lovable#` (variante §7.1) : **rejeté**, parce que `#` est déjà le sigil d'`IndexGen`.
- Les identifiants partagent **l'espace de noms des génériques de la signature**. `fn(a: T, b: Lovable@T)` est
  une erreur (`T` est à la fois libre et borné).

### D2. Représentation : un nouveau variant `Bounded`

```rust
// components/type/mod.rs — À LA FIN de l'enum (sérialisation .bin, cf. Risques)
Bounded(String, Box<Type>, HelpData),   // Lovable@A  →  Bounded("A", Alias("Lovable"), h)
```

On le préfère à une extension de `KindedGen`, dont la clé est un `Kind` (4 valeurs) et pas un type.
On le préfère aussi à une table de bornes à côté de la signature : la borne doit voyager avec le type
à travers `Function`, `Vec`, `Tuple` et la substitution.

`Bounded` n'existe **que dans les signatures**. Dans le corps, il devient un rigide (D4). Au site
d'appel, il est lié comme un générique (D5).

### D3. Sémantique d'un `I` nu : **option A retenue (2026-09-30) — changement de comportement assumé**

> **Décision :** un `I` nu ≡ `I@I` (une variable partagée par interface, RFC §5.3). L'option B ci-dessous est écartée ; elle est conservée pour mémoire.

| Option | `fn(a: Lovable, b: Lovable)` | `cmp(cat, dog)` | Retour `Lovable` |
|---|---|---|---|
| **A (RFC §5.3)** : `I` nu ≡ `I@I`, une variable partagée par interface | `a` et `b` du **même** type | **rejeté** (aujourd'hui accepté) | non ambigu |
| **B** : `I` nu ≡ `I@_`, variable fraîche par occurrence | types indépendants | accepté | **ambigu** s'il y a ≥ 2 paramètres `I` : erreur « ajoutez `@Id` » |

**Choix : A**, pour trois raisons :

- c'est le RFC ;
- c'est la seule option sûre pour les interfaces `(Self, Self)` (`Eq`, `Ord`, `Same`), où `a.eq(b)`
  n'a de sens que si `a` et `b` ont le même type ;
- elle aligne le corps sur ce que le site d'appel fait déjà à moitié.

Coût de A : du code qui compile aujourd'hui (`cmp(cat, dog)` avec des `Lovable` nus) sera rejeté, avec
un message qui propose `Lovable@A, Lovable@B`. Le stdlib n'est pas touché (`unique` et `sort`
n'ont qu'un paramètre interface). Il reste à mesurer l'impact sur les tests et la doc en phase 0.

### D4. Corps : un rigide **par identifiant**, pas par paramètre

La boucle de `function.rs:326` passe d'un rigide par paramètre à une table `id → rigide`. Deux
paramètres `Lovable@X` partagent `__rigid_k`. Avec D3-A, deux `Lovable` nus partagent aussi le leur.
`is_rigid_compatible` compare le rigide du corps **au rigide de l'identifiant déclaré en retour**,
et non plus seulement à la borne.

### D5. Site d'appel : `Bounded` passe par l'unification générale

On ajoute un bras à `unification_helper` (`unification.rs:239`), sur le modèle du bras `KindedGen`
déjà présent :
`(concrete, Bounded(id, bound))` lie `Generic(id) ↦ concrete` **si** `concrete ⊨ bound`
(`interface_satisfaction::check_interface_satisfaction`), et échoue sinon. Deux occurrences du
même `id` passent alors par `UnificationMap::try_new`, qui rejette déjà les liaisons contradictoires.
C'est exactement la règle « même identifiant ⇒ même type » (RFC §7.2).

`try_interface_subtype_match` ne sert plus qu'aux interfaces nues **après désucrage**. Avec D3-A,
elles deviennent des `Bounded` et la fonction peut disparaître. Égalité exigée pour un même `id` :
on reprend le comportement actuel d'un `T` répété (`fn(a: T, b: T)`), pour ne pas créer deux règles.

### D6. Hors périmètre : interfaces paramétrées `interface<T>` (RFC §6.3)

Elles ne sont pas parsées aujourd'hui. `Box@A` avec `Box <- interface<T>` relève d'un RFC distinct.
`Bounded` est conçu pour les accueillir (borne = `Alias("Box", [T])`) sans changement de représentation.

---

## 2. Phases

### Phase 0 — Filet et mesure (aucun changement de sémantique)

- `typr case add` pour les défauts 1, 2 et 3 du §0 (`expect.toml` : substrings d'erreur attendues).
- Expliquer le défaut 3 (`a.same(b)` accepté) : tracer `is_subtype_raw(Generic(r1), Generic(r0))`.
- Mesurer D3-A : forcer temporairement un rigide partagé par interface dans `function.rs`, puis lancer
  `cargo test --workspace`, `typr case run` et, côté doc, `npm run check:examples`. Lister les échecs.
  C'est cette liste qui chiffre le changement de comportement.

**Statut : FAITE (2026-09-30).**

- Cas ouverts (échouent tant que le plan n'est pas implémenté ; oracle : `Type errors found`) :
  `0086-interface-return-bound-to-first-arg` (défaut 1), `0087-interface-shared-id-different-types`
  (défaut 2 / D5), `0088-interface-self-self-distinct-ids` (défaut 3). `typr case run` : ils sont `OPEN`.
  Les 4 `REGRESS` restants (0004, 0005, 0006, 0008 : diff golden de `R/main.R`) préexistent et n'ont
  aucun rapport.
- **Cause du défaut 3** : `impl PartialEq for Type` (`components/type/mod.rs:1334`) déclare
  `(Generic(_), Generic(_)) => true` — deux génériques quelconques sont « égaux ». Or `is_subtype_raw`
  commence par `(typ1, typ2) if typ1 == typ2 => true` (`mod.rs:226`) : deux rigides distincts
  `__rigid_0` / `__rigid_1` passent donc pour sous-types l'un de l'autre. Ce n'est pas un filtre de
  repli. **Conséquence pour la phase 3** : ne pas toucher au `PartialEq` global (il sert à l'unification
  et à `UnificationMap`) ; ajouter dans `is_subtype_raw`, avant la garde d'égalité, un bras
  `(Generic(a), Generic(b))` qui compare les **noms** quand les deux sont des rigides (`__rigid_`).
  Même cause probable pour le défaut 4 (`{ a }` et `{ b }` passent tous deux).
- **Mesure D3-A** (rigide partagé par interface dans le corps, `function.rs`, expérience temporaire
  revertie) : `cargo test --workspace` **entièrement vert** (711 + 161 + … tests), `typr case run`
  inchangé (mêmes 4 REGRESS préexistants). Un scan de `typr.github.io/docs`, `blog`, `lab/`,
  `cases/*/repro` et du stdlib ne trouve **aucune** fonction à deux paramètres de même alias
  d'interface. Limite : l'expérience ne touche que le corps ; le rejet de `cmp(cat, dog)` au site
  d'appel (D5) n'est pas mesuré, mais le scan statique suggère un impact nul sur le corpus existant.
  `npm run check:examples` non lancé (aucun exemple concerné d'après le scan).

### Phase 1 — Parsing et affichage

- `parsing/types.rs` : suffixe `opt(preceded(tag("@"), alt((generic_name, tag("_")))))` après un alias
  (y compris imbriqué : `[#N, Eq@A]`, `tuple{Lovable@A}`, `(Lovable@A) -> …`). Produit `Type::Bounded`.
- Trouver et supprimer le chemin qui avale `@A` aujourd'hui (défaut 2). Un `@` suffixe mal formé doit
  donner une erreur de syntaxe, jamais être ignoré.
- `Type::Bounded` : `Hash`/`Eq`, `type_printer` (`Lovable@A`), `type_category`, `fingerprint`, et tous les
  `match` exhaustifs (le compilateur les listera).
- Manifeste de syntaxe `components/syntax/mod.rs` : déclarer le `@` suffixe de type. Puis
  `typr syntax --write` et `--check`, et les deux tests `components::syntax::tests`.

**Statut : FAITE (2026-09-30, non commitée).**

- `Type::Bounded(id, bound, h)` ajouté en **fin** d'enum ; `Hash` (tag 45), `to_category` (celle de la
  borne), `get_help_data`/`set_help_data`, `type_printer` (`format` et `verbose` : `Lovable@A`).
  Les autres `match` ont des jokers : le variant ne traverse pas encore `reduce_type`, l'unification
  ni `checked_assertions` (phases 2-5).
- Parseur (`type_alias`, `bound_suffix`) : `@` collé à l'alias, suivi de **une seule majuscule** ou de `_`.
  `Self` n'est pas un id valable (un générique est une lettre ; `Self` est réservé aux interfaces).
- **Défaut 2 (`@A` avalé)** : la cause est le résolveur de priorité des types, qui écarte un type
  résiduel. Un `@` suffixe mal formé (`Lovable @A`, `Lovable@Self`, `Lovable@Abc`) pousse
  maintenant `SyntaxError::DetachedBoundSuffix` (**S018**, + entrée `typr-mcp/src/explain.rs`).
  `Lovable@` et `Lovable@a` échouaient déjà (point-virgule manquant).
- Manifeste de syntaxe : **rien à changer**, `@` y est déjà (`live("@", "Interface", …)`) ;
  `typr syntax --check` passe.
- Tests : 5 nouveaux dans `parsing/types.rs`. `cargo test --workspace` vert ; `typr case run` : mêmes 4
  REGRESS préexistants.
- Limite : `Bounded` est parsé mais **pas encore interprété**. `fn(a: Lovable@A, b: Lovable@B)` passe
  `typr check` avec la sémantique d'avant. Le défaut 2 n'est donc corrigé qu'au niveau syntaxe ; le cas
  0087 reste OPEN jusqu'à la phase 4.

### Phase 2 — Normalisation de la signature

Un seul passage, appelé depuis `function()` et pour les signatures `@name: …;` :

- collecter `id → borne` sur les paramètres et le retour ;
- appliquer le désucrage D3 aux `I` nus : `I` devient `Bounded(I, I)` (option A) ;
  `@_` donne un id frais ;
- erreurs (RFC §7.3), une variante `TypeError` chacune, codes d'erreur stables :
  - un `id` avec deux bornes différentes (`Lovable@A`, `Printable@A`), sauf si l'une est `@A` sans borne ;
  - un `id` présent seulement dans le retour : on généralise `InterfaceReturnOnly` ;
  - un `id` qui entre en collision avec un générique libre du même nom ;

**Statut : FAITE (2026-09-30, non commitée).**

- Nouveau module `type_checking/signature_normalization.rs` : `normalize_signature(context, params, ret)`
  renvoie `{ params, ret, errors }`. Il (1) renomme chaque `I@_` en `_0`, `_1`…, (2) désucre un alias
  d'interface nu `I` en `Bounded("I", I)` (D3-A ; les `interface { … }` en ligne sont laissés tels quels),
  (3) produit les erreurs du §7.3.
- Erreurs : `ConflictingBound` **T047** (deux bornes pour un id ; comparées après `reduce_type`),
  `BoundCollidesWithGeneric` **T048** (id égal à un générique libre `T`/`#N` ; `@A` n'est pas compté, D1),
  et `InterfaceReturnOnly` **T017** réutilisée pour un id présent seulement au retour. Entrées `explain`
  T047/T048 dans `typr-mcp`.
- **Seules les erreurs sont branchées** dans `function()`. La signature désucrée est calculée mais pas
  consommée : corps et site d'appel lisent encore la signature brute, donc le comportement des programmes
  valides est inchangé (les I nus ne deviennent pas `Bounded` avant les phases 3-4).
- Limite connue : `fn(a: Lovable@A): Lovable@A { a }` est encore rejeté (`Bounded` n'est pas interprété
  dans le corps) — c'est l'objet de la phase 3.
- Tests : 6 nouveaux. `RUST_MIN_STACK=8388608 cargo test --workspace` vert, sauf
  `typr-graph --test walkthrough` (ordre `Point/x`/`Point/y` d'un `HashSet`, flaky, sans lien).

### Phase 3 — Typage du corps

- `function.rs:326` : table `id → rigide` (D4). `add_interface_constraint(rigide, borne)` une seule fois
  par id.
- `specialize_lambda` (`function_application.rs:1061`) : même traitement, et `rigid_to_concrete` indexé
  par id.
- `is_rigid_compatible` : le retour `Lovable@B` n'accepte que le rigide de `B`.
- Défaut 3 : entre deux rigides distincts, `is_subtype_raw` doit valoir `false`. Donc `a.same(b)` sera
  rejeté avec `Same@A, Same@B`, et accepté avec `Same@A, Same@A` ou avec `Same` nu (D3).

**Statut : FAITE pour `function()` (2026-09-30, non commitée).**

- `function.rs` : table `id → rigide` (un rigide par id, D3-A pour les `I` nus via la signature
  normalisée). Un paramètre `Lovable@A` est typé par sa borne ; la table sert aussi au retour : un retour
  `Lovable@B` est vérifié contre le **rigide de `B`** (`checked_ret`, aussi passé à
  `set_expected_return_type` pour les `return` anticipés). `is_rigid_compatible` : si le retour déclaré est
  un rigide, seul ce rigide passe (le défaut 4 venait de `interface <: Generic(_)`).
- Défaut 3 : bras `(Generic, Generic)` rigides dans `is_subtype_raw` (comparaison par nom). Piège :
  `Hash` ignorait le nom d'un `Generic`, donc le cache de sous-typage confondait `(r0, r1)` et `(r0, r0)` ;
  les noms de rigides entrent maintenant dans le hash (`is_rigid_name`, `type/mod.rs`).
- Tests : 5 dans `function.rs` (`test_bound_*`, `test_self_self_*`, `test_bare_interface_*`). Piège de test :
  `parse2` ne se comporte pas comme le CLI (les rejets passaient) ; le helper utilise `parse_from_string`.
- `typr case run` : 0088 passe à READY (rejeté dans le corps). 0086/0087 restent en attente de la phase 4
  (READY pour de mauvaises raisons : l'appel d'une fonction `Lovable@A` échoue tant que `Bounded` n'est pas
  unifié). `cargo test --workspace` vert ; mêmes 4 REGRESS préexistants.
- **Non fait** : `specialize_lambda` (les paramètres de lambda n'ont pas de `Bounded` déclaré ; à traiter en
  phase 4 avec `rigid_to_concrete` par id). Rigides imbriqués (`[#N, Lovable@A]`) : toujours sans rigide.
- À corriger : le message de retour affiche `Found: __RIGID_1` ; il faudrait nommer l'id (`B`).

### Phase 4 — Site d'appel

- `unification.rs` : bras `Bounded` (D5), avec un occurs check comme pour `Generic`.
- `apply_unification_to_return_type` : la substitution de `Generic(id)` couvre `Bounded(id, _)` dans le
  retour, y compris sous `Tuple`, `Vec` et `Function` (le `swap` du RFC §6.2 rend `tuple{Dog, Cat}`).
- `try_named_generic_match` / `collect_interface_bindings` : les signatures mixtes
  (`fn(f: (T) -> U, x: Lovable@A)`) et les éléments de tableau (`[#N, Eq@A]`).
- Supprimer `try_interface_subtype_match`, ou le réduire au strict nécessaire (D5).
- Messages d'erreur au site d'appel : pour « `A` lié à `Cat` puis à `Dog` », nommer l'identifiant et les
  deux arguments.

**Statut : FAITE, avec écarts au plan (2026-09-30, non commitée).**

- **Écart D5** : pas de bras `Bounded` dans `unification_helper`. `signature_normalization::instantiate_at_call`
  normalise la signature, lie chaque id à l'argument (`bind_ids` : la borne doit être satisfaite, un même id
  exige le même type par sous-typage mutuel, car le `PartialEq` de `Type` est trop lâche pour les génériques),
  puis **substitue** `Bounded(id)` par le type lié dans les paramètres et le retour. Les filtres existants
  voient ensuite une signature concrète (les `#N`, `T` restants sont à eux). Plus simple qu'un bras
  d'unification, et le retour est substitué sous `Vec`/`Tuple`/`Function` par le même `rewrite`.
- Branché au début de `apply_from_variable_inner` : signature rejetée → retirée des candidats ; une signature
  est concernée seulement si elle a un `Bounded` déclaré ou un même id ≥ 2 fois après désucrage (un `I` nu
  unique garde le chemin historique). Le message d'erreur liste les signatures *déclarées*.
- **`try_interface_subtype_match` conservé** (il sert encore au `I` nu unique, `unique`/`sort`). À réduire plus tard.
- Cas de test : `cmp(cat, dog)` avec `Lovable@A, Lovable@B` OK ; `Lovable@X` répété avec `(cat, dog)` rejeté ;
  `Lovable` nu répété avec `(cat, dog)` rejeté (D3-A) ; `id(id(cat))` OK ; retour `Lovable@B` = `Dog`, pas `Cat`.
  4 tests dans `signature_normalization.rs`. Cas 0086/0087/0088 : READY. `cargo test --workspace` vert
  (hors `typr-graph walkthrough`, flaky connu) ; mêmes 4 REGRESS préexistants.
- Piège : le cas 0086 utilisait `Dog` ⊃ `Cat` (largeur de record), donc `Dog <: Cat` et le retour `Dog` passait
  pour `Cat`. `Dog` n'a plus que `age`.
- **Non fait** : `swap` avec `tuple{…}` (syntaxe littérale non vérifiée), éléments `[#N, Eq@A]` (code prévu via
  `Vec` mais sans test), `specialize_lambda` par id, message dédié « `A` lié à `Cat` puis `Dog` » (aujourd'hui :
  `NoMatchingSignature`).

### Phase 5 — Transpilation, `--checked`, LSP

- **Le R généré ne doit pas changer** (propriété à vérifier, pas une option) : la dispatch S3
  `cmp.Lovable` / `cmp.default` utilise le nom de la borne, pas l'id.
- `transpiling/checked_assertions.rs` : un `Bounded` s'asserte comme sa borne.
- LSP : le survol affiche `Lovable@A` dans la signature et `Cat` au site d'appel.

**Statut : FAITE (2026-09-30, non commitée). LSP vérifié par lecture seulement.**

- **La propriété « R inchangé » était fausse** : `fn(a: Lovable@A): Lovable@A` produisait `idf.default` avec
  un cast `as.Generic()`, au lieu de `idf.Lovable` / `as.Lovable()` + alias `.default`. Cause : `get_class`,
  `get_class_unquoted`, `get_type_anotation` et `get_type_anotation_no_parentheses` (`context/vartype.rs`,
  `context/mod.rs`) ne connaissaient pas `Bounded`, et la branche `Lang::Let` de `transpiling/mod.rs` lisait
  le type du paramètre de dispatch brut (facettes interface/record). Correctif : `Bounded` est déballé en sa
  borne à ces cinq endroits.
- `checked_assertions.rs` : `Bounded` s'asserte comme sa borne (`checked_descriptor`).
- Vérifié : R généré **identique** (diff vide) entre `Lovable` et `Lovable@A`, avec et sans `--checked`.
  Test : `test_bounded_id_does_not_change_generated_r` (`transpiling/mod.rs`).
- LSP : aucun changement de code. Le survol passe par `type_printer`, qui affiche déjà `Lovable@A` (phase 1).
  Non testé dans le crate `typr-lsp`.
- `cargo test -p typr-core` vert ; `typr case run` : mêmes 4 REGRESS préexistants, 0086-0088 READY. Sous
  `cargo test --workspace`, `registry_validate::formals_diff_flags_…` a échoué une fois et passe seul (instable
  en parallèle, sans lien).

### Phase 6 — Documentation et RFC

- Déplacer le RFC dans `rfcs/` (numéro suivant, format `0000-template.md`), avec les décisions D1-D6.
- `typr.github.io` : page de référence sur les interfaces, `syntaxe.md` (**les deux copies**), glose
  du `@` suffixe dans `scripts/syntax-glossary.mjs` (sinon `check:syntax` casse), et exemples vérifiés
  par `typr check` (un `compile_fail` pour `cmp(cat, dog)` avec des `Lovable` nus ).
- `ai_context/interface_as_constrained_generic.md` §4.2 et §8.4 : à mettre à jour. Cette spec disait
  « paramètres distincts ⇒ variables distinctes » et proposait `where A: I`, que `I@A` remplace.


**Statut : FAITE (2026-09-30, non commitée).**

- RFC : `rfcs/0000-interface-bound-generics.md` (numéro à attribuer au merge de la PR).
- `typr.github.io` : section « `I@Id` » dans `docs/reference/interfaces.md` (4 blocs : 1 `compile_fail`
  pour `cmp(cat, dog)` avec `Lovable` nus, 3 vérifiés), et `syntaxe.md` §11. Il n'existe **qu'une** copie de
  `syntaxe.md` (`typr.github.io`) ; `typR/typr/syntaxe.md` n'existe pas, contrairement à ce que dit le CLAUDE.md.
- Glossaire `syntax-glossary.mjs` : **inchangé**, car le manifeste n'a pas bougé (`@` y est déjà) ;
  `npm run check:syntax` passe.
- `ai_context/interface_as_constrained_generic.md` : §4.2, §8.4 et la note §10 réécrits.
- Piège : `check:examples` prend le `typr` du PATH (installé, ancien). Il faut
  `TYPR_BIN=…/target/debug/typr`, sinon les blocs `@Id` échouent à tort. Avec lui : 199 blocs + 7 `compile_fail` OK.
### Suivi — points « non fait » des phases 3-4 (2026-09-30, non commité)

- **Message de retour** : `name_rigids` (`function.rs`) réécrit les rigides en `Lovable@B` (ou `Lovable` nu) : plus de `__RIGID_1`.
- **Site d'appel** : `CallInstance::Clash(IdClash)` + `TypeError::IdBoundToTwoTypes` **T049** (deux spans : « `X` est `Cat` ici / mais `Dog` ici »).
  Émis quand toutes les signatures déclarées sont rejetées par un clash d'id ; sinon `NoMatchingSignature`. Entrée `explain` T049.
- **Rigides imbriqués** : `[#N, Lovable@A]`, `tuple{Lovable@B, Lovable@A}` en paramètre *et* en retour passent par `replace_bounded_with_rigids`
  (`function.rs`). `swap` (tuple) et `first` (tableau) sont vérifiés, positif et négatif.
- **Bug trouvé** : `PartialEq for Type` n'avait pas de bras `Bounded` (`_ => false`) : un `Bounded` n'était jamais égal à lui-même, le graphe de types
  de `hoist_aliases` se dupliquait à chaque appel, et le vérificateur bouclait (deux fonctions `Lovable@A` appelées avec des lambdas sans type).
  Corrigé. Effet de bord : `get_type_anotation` trouvait un alias `FunctionN` au lieu de `as.Generic()` ; `has_bounded` compte désormais comme générique
  (R généré à nouveau identique à la forme nue).
- **Hors périmètre, préexistant** : une lambda sans annotation passée en argument (`apply(fn(c) { … }, cat)`) échoue déjà avec `T` (« fn<UnknownFunction> not defined »).
  Donc `specialize_lambda` par id n'est pas atteignable tant que ce n'est pas résolu ; lambdas *typées* avec `@A`/`@B` : OK.
- **Reste** : `try_interface_subtype_match` (I nu unique), `[#N, Eq@A]` avec une interface `(Self, Self)` non sondé.
- Tests : 4 nouveaux dans `signature_normalization.rs`. `cargo test --workspace` vert ; mêmes 4 REGRESS.

---

## 3. Tests à écrire (au fil des phases)

| Programme | Attendu |
|---|---|
| `fn(a: Lovable@A, b: Lovable@B): bool { a.love() == b.love() }`, `cmp(cat, dog)` | OK |
| `fn(a: Lovable@A, b: Lovable@B): Lovable@B { b }`, `let r: Dog <- f(cat, dog)` | OK (défaut 1 corrigé) |
| idem, `let r: Cat <- f(cat, dog)` | erreur |
| `fn(a: Lovable@A, b: Lovable@B): Lovable@A { b }` | erreur dans le corps |
| `fn(a: Lovable@X, b: Lovable@X)` appelé avec `(cat, dog)` | erreur « X lié à Cat puis Dog » |
| `swap` : `tuple{Lovable@B, Lovable@A}` | `tuple{Dog, Cat}` |
| `fn(a: Same@A, b: Same@B): bool { a.same(b) }` | erreur (défaut 3) |
| `fn(a: Same, b: Same): bool { a.same(b) }`, appel `(1, 2)` | OK (D3) |
| `fn(a: Lovable, b: Lovable)` appelé avec `(cat, dog)` | erreur + suggestion `@A/@B` (D3) |
| `Lovable@A` et `Printable@A` dans la même signature | erreur de borne conflictuelle |
| `fn(): Lovable@A` | erreur (retour seul) |
| `double(3)`, `double(double(3))`, `unique`/`sort` sur `[#N, Point]` | inchangés (régression) |
| `Lovable @A` (espace), `Lovable@` | erreur de syntaxe, pas de perte silencieuse |
| R généré de `cmp` | identique à avant |

---

## 4. Risques

- **Changement de comportement (D3)** : c'est le principal. La phase 0 le chiffre avant qu'on
  s'engage.
- **Sérialisation `.bin`** : `Bounded` va à la fin de l'enum `Type` (même contrainte que `Refined`).
- **Deux chemins d'appel** (unification générale et `try_interface_subtype_match`) : risque d'incohérence
  s'ils coexistent. D5 les fusionne.
- **Couplage inter-dépôts** : le manifeste de syntaxe et le glossaire de `typr.github.io` doivent
  changer ensemble, sinon le build de la doc casse.
