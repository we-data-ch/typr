# TypR Block Graph — spécification v2

Visualiseur interactif du code TypR sous forme de **graphe de blocs hiérarchique**, pensé à la
fois comme outil pédagogique (comprendre un programme et les concepts du langage) et comme outil
de revue de code. Disponible en CLI (`typr graph`) et dans le playground.

Cette version remplace `visualization_graph.md` (v1). Elle garde de la v1 l'idée d'un graphe
sémantique typé et de projections, mais l'adapte à l'architecture réelle de `typr-core` et au
modèle « tout est un bloc » décidé en discussion.

> Pas de RFC nécessaire : l'outil ne change pas le sens du langage (voir `rfcs/README.md`).

---

## 0. Décisions prises

| # | Sujet | Décision |
|---|---|---|
| A | Priorité | Graphe d'architecture/revue d'abord ; chaque relation peut porter une **justification** (`evidence`) pour servir la pédagogie ensuite. |
| B | Granularité | **Les expressions sont des blocs.** `7`, `12 + 3`, une fonction, un module, un type : tout est un bloc. Les déclarations (`let`, `type`, `module`, paramètres) ne créent pas de bloc : elles **nomment** un bloc. |
| C | Précision | Résolution des noms **par nom** (comme le LSP) pour commencer, avec un niveau de confiance explicite sur chaque référence. Un **enregistreur de types** minimal dans `typing()` fournit le type de chaque bloc. Une vraie passe de résolution de noms est un chantier séparé. |
| D | Spans | `HelpData` gagne un **offset de fin** (prérequis, profite aussi au LSP). |
| E | Rendu | **React Flow (xyflow) + ELK.js** dans le playground. Navigation par « entrée » dans les blocs, fil d'Ariane, historique dans l'URL. Navigation clavier prévue. |
| G | Docs | Un bloc ` ```typr graph ` de la documentation ouvre le playground sur la vue graphe. |
| — | Crate | Nouvelle crate `crates/typr-graph`, WASM-safe, dépendant de `typr-core`. |
| Q1 | Frontière | Toute dépendance qui franchit la frontière d'un bloc apparaît comme **port d'entrée implicite** (capture). Le bloc expose honnêtement tout ce dont il dépend : localité du raisonnement et composabilité rendues visibles. |
| Q2 | Appel ≠ contenance | Une fonction est son propre bloc, défini une seule fois ; un appel ne la contient pas, il la **référence** par une relation. Deux gestes distincts : *entrer* et *aller à la définition*. |
| Q3 | Mutation/boucles | Les boucles sont des blocs à **ports d'état** (entrée/sortie par variable mutée). |
| Q4 | Types | Un type est un bloc ; son interface ce sont ses **méthodes** (fonctions qui le prennent en 1ʳᵉ position). D'autres positions (n-ième paramètre, retour) pourront s'ajouter. |

Les points non tranchés sont listés en §13 et seront discutés **quand le besoin se présente**, pas avant.

---

## 1. Objectifs et non-objectifs

**Objectifs**

1. Représenter un programme TypR comme une hiérarchie de blocs explorable de l'intérieur.
2. Rendre visibles les frontières : ce qu'un bloc consomme (entrées), expose (sorties), cache.
3. Rendre visibles les relations du système de types : type d'un bloc, méthodes d'un type,
   satisfaction d'interface avec sa justification, sous-typage.
4. Servir la revue de code : dépendances d'un bloc, puis (plus tard) différence entre deux versions.

**Non-objectifs (pour l'instant)**

- Éditer le programme depuis le graphe (le graphe est une vue, pas un éditeur).
- Remplacer l'AST ou introduire un HIR : le graphe est dérivé de l'AST + du contexte.
- Analyse historique git, métriques de couplage : reportées (§12, étape 7+).

---

## 2. Écart avec la v1 et contraintes de la base de code

| Hypothèse v1 | Réalité de `typr-core` | Conséquence |
|---|---|---|
| Pipeline AST → résolution → HIR → typage | `Lang` est l'AST, typé directement ; pas de HIR | Le builder part de `Lang` + `Context` final + enregistreur de types |
| Noms résolus vers des IDs | Résolution par chaîne dans `VarType` (masquage par ordre, surcharge par type du 1ᵉʳ argument) | Références résolues par nom, avec `confidence` (§5.3) |
| Types par expression disponibles | `TypeChecker` ne garde qu'un type par instruction de haut niveau ; `typing()` (`processes/type_checking/mod.rs`) est l'unique point d'entrée récursif | Enregistreur dans `typing()` (§7.2) |
| `Person implements Printable` déclaré | Interfaces **structurelles**, sans `impl` (`interface_satisfaction.rs`) | `Satisfies` est **calculé**, avec justification |
| Types = entités | `Type` est une valeur structurelle portant un `HelpData` | Canonisation (position retirée) pour l'identité |
| Identité = `NodeIndex` | Instable d'une compilation à l'autre | Identité = **clé sémantique** (§6) ; les index restent internes |
| — | La stdlib est préchargée dans le contexte (milliers d'entrées) | Seules les entrées **référencées** apparaissent, comme blocs externes repliés |
| — | Hiérarchie de sous-typage déjà calculée : `Context.subtypes: Graph<Type>` (`context/graph.rs`) | Réutilisée pour la relation `Subtype` |
| — | `HelpData` = `offset` + `file_name` seulement | Ajout d'un offset de fin (décision D) |

Note : le parseur ne désucre ni `|>` ni `.` (ils restent des `Lang::Operator`), donc le graphe
peut refléter la forme du source.

---

## 3. Modèle

### 3.1 Vocabulaire

- **Bloc** : unité sémantique délimitée (une expression, une fonction, un type, un module…).
  Il a une **interface** (ports) et éventuellement un **intérieur** (un sous-graphe de blocs).
- **Port** : point d'entrée ou de sortie d'un bloc, nommé et (si connu) typé.
- **Fil** (*wire*) : flot de données d'un port de sortie vers un port d'entrée, **à l'intérieur
  d'un même bloc parent**. Les fils ne traversent jamais une frontière : ce qui entre dans un bloc
  passe par un de ses ports d'entrée.
- **Relation** : arête sémantique entre deux blocs quelconques (référence vers une définition,
  typage, satisfaction d'interface…). Contrairement aux fils, les relations peuvent relier des
  blocs à des niveaux différents.
- **Nom** : une déclaration (`let`, `type`, `module`, paramètre, liaison de motif) attache un nom
  à un bloc. Un bloc anonyme reste un bloc.

### 3.2 Anatomie d'un bloc

```
Block
├── key       : BlockKey            clé sémantique stable (§6)
├── kind      : BlockKind           (§4)
├── name      : Option<String>      donné par la déclaration qui le nomme
├── span      : Option<Span>        début..fin dans le source (None pour stdlib/externe)
├── type      : Option<TypeRef>     type du bloc (enregistreur, §7.2)
├── inputs    : Vec<Port>           explicites (paramètres, opérandes, arguments)
│                                   + implicites (captures, §3.3)
├── outputs   : Vec<Port>           en général un seul (la valeur) ;
│                                   plusieurs pour Record, Module, TypeDecl (§4)
├── origin    : User | Std | RPackage(name)
└── body      : Option<Body>        intérieur explorable
      ├── children : Vec<BlockKey>
      └── wires    : Vec<Wire>      (from: PortRef, to: PortRef)

Port { name, type: Option<TypeRef>, implicit: bool, visibility: Public | Private }
PortRef { block: BlockKey, port: String }   // block = le parent lui-même pour ses propres ports
```

### 3.3 Règle de frontière (Q1)

Pour tout bloc `B` possédant un intérieur :

> **Chaque nom utilisé dans l'intérieur de `B` et défini hors de `B` devient un port d'entrée
> implicite de `B`.**

Dans l'intérieur de `B`, ce port est la source des fils vers ses utilisateurs. La définition
elle-même est reliée par une relation `Ref` (§5), jamais par un fil.

Conséquences :

- l'interface d'un bloc liste **toutes** ses dépendances : on peut raisonner sur un bloc sans
  regarder ailleurs, ce qui est la base de la revue locale et de l'enseignement de la composition ;
- les dépendances remontent naturellement : si `norm2` capture `sq`, et que `norm2` est dans un
  module `M` où `sq` est aussi défini, `sq` n'est **pas** une entrée de `M` (la capture est
  résolue à l'intérieur de `M`) ;
- au niveau du bloc racine `Program`, les entrées implicites sont exactement les dépendances
  externes (stdlib, paquets R) du programme.

Les opérateurs natifs (`+`, `*`, …) ne sont pas des captures : ils font partie du bloc `Operator`.
(Si un opérateur se résout vers une fonction utilisateur, voir §13.)

Le rendu **peut** replier ces fils (ex. afficher le nom de l'appelé en étiquette du bloc `Apply`
plutôt qu'un fil depuis le port `sq`) : c'est une option d'affichage, le modèle garde le fil.

---

## 4. Catalogue des blocs

Colonne **Étape** : quand le bloc est implémenté (voir §12). Tout variant de `Lang` non encore
couvert produit un bloc **`Opaque`** (texte source, type si connu, pas d'intérieur) : le builder
est total dès la première étape et ne panique jamais.

| Kind | Source (`Lang`) | Entrées | Sorties | Intérieur | Étape |
|---|---|---|---|---|---|
| `Program` | racine `Lines` | dépendances externes | — | instructions de haut niveau | 1 |
| `Literal` | `Number`, `Integer`, `Bool`, `Char`, `Null`, `NA`, `Empty` | — | valeur | — | 1 |
| `Operator(op)` | `Operator` (hors `.`/`\|>`), `Not` | opérandes (`lhs`, `rhs`) | valeur | les opérandes | 1 |
| `Apply` | `FunctionApp`, `VecFunctionApp`, `.`/`\|>` (UFCS) | `callee` + `arg0..n` (ou noms) | valeur | les arguments | 1 |
| `Scope` | `Scope`, corps de fonction | captures | valeur de la dernière expression | blocs nommés par `let` + expressions | 1 |
| `Function` | `Function`, `Lambda` | paramètres + captures | retour | le corps (paramètres comme sources) | 1 |
| `Record` | `List` (champs nommés), `ConstructorCall` | un port par champ (+ spreads) | valeur **et** un port par champ | les valeurs des champs | 1 |
| `Access` | accès champ (`$`, `.x`) | la valeur | le champ | — | 1 |
| `TypeDecl` | `Alias` (record, union, opaque) | — | ses **méthodes** (Q4) | les champs / membres (blocs `TypeExpr`) | 1 |
| `TypeExpr` | type en position d'annotation | — | — | sous-types (`int \| char`, `list{…}`) | 1 |
| `Interface` | `Alias` vers `interface { … }` | — | — | signatures requises | 1 |
| `If` | `If` | `cond` + captures | valeur | `cond`, `then`, `else` (chacun explorable) | 1 |
| `Tuple` | `Tuple` | un port par position | valeur + un par position | éléments | 1 |
| `Array` | `Array`, `Vector` | éléments | valeur | éléments | 1 |
| `Opaque` | tout le reste | captures si connues | valeur | — | 1 |
| `Loop` | `ForLoop`, `WhileLoop`, `Loop` | itérable/condition + **état entrant** | **état sortant** | le corps | 5 |
| `Match` | `Match` | le sujet + captures | valeur | un sous-bloc par bras (§13) | 5 |
| `Module` | `Module` | captures | **membres `@pub` seulement** | tous les membres, privés compris | 5 |
| `RCode` | `RBlock`, `ExternBlock`, `RFunction` | — | valeur | — (frontière opaque vers R) | 5 |

### 4.1 Précisions

- **Let** ne crée pas de bloc : `let a <- sq(p$x)` nomme le bloc `Apply` ; toute utilisation
  ultérieure de `a` dans le même `Scope` est un fil depuis sa sortie.
- **Assign hors boucle** (`x <- x + 1`) : les utilisations suivantes de `x` sont câblées depuis
  le nouveau bloc. Aucune étiquette de version n'est affichée : le câblage suffit.
- **Loop (Q3)** : chaque variable définie hors de la boucle et réaffectée dedans devient une
  paire de ports `état entrant` / `état sortant` du bloc `Loop`. Le reste du `Scope` englobant
  est câblé depuis l'état sortant.
- **Record et Module ont plusieurs sorties** : un record expose un port par champ ; un module
  n'expose que ses membres `@pub`. Entrer dans un module montre aussi les membres privés,
  marqués comme tels : **le secret se voit de l'intérieur, jamais sur l'interface.**
- **Access** : `p$x` est un bloc `Access(x)` câblé depuis `p`. Quand la source est un bloc
  `Record` visible au même niveau, le rendu peut câbler directement depuis le port `x` du record.
- **Types** : un bloc d'expression est relié à son type par `HasType` (§5). Un `TypeDecl`
  expose comme sorties ses méthodes, c'est-à-dire les fonctions reliées à lui par
  `TypePosition { index: 0 }`.

---

## 5. Relations

### 5.1 Catalogue

| Kind | De → vers | Sens | Étape |
|---|---|---|---|
| `Ref` | utilisation (`Apply.callee`, port implicite) → bloc défini | « dépend de la définition » | 1 |
| `HasType` | bloc d'expression → `TypeDecl`/`TypeExpr` | typage | 1 |
| `TypePosition { index }` | `Function` → `TypeDecl` | le type apparaît en paramètre `index` (`0` = méthode, Q4) ; `Return` plus tard | 1 (index 0), 7 (autres) |
| `Satisfies` | `TypeDecl` → `Interface` | satisfaction structurelle, **calculée** | 3 |
| `DeclaredAs` | `TypeDecl` → `Interface` | intersection `Record & Interface` vérifiée à la déclaration | 3 |
| `Subtype` | type → supertype | depuis `Context.subtypes` | 3 |
| `Instantiates` | `Apply` → `Function` générique | instanciation d'un générique | §13 |

Convention de sens (reprise de la v1) : l'arête part de ce qui **dépend** vers ce dont il dépend.

### 5.2 Justification (`evidence`)

Toute relation calculée peut porter une justification lisible, affichée au clic :

```json
{ "kind": "Satisfies", "from": "type:Point", "to": "type:Printable",
  "evidence": [ { "requires": "show: (Self) -> char", "provided_by": "val:show(Point)" } ] }
```

C'est le support principal de l'usage pédagogique : le graphe ne dit pas seulement *que*
`Point` satisfait `Printable`, mais *grâce à quelle fonction*.

### 5.3 Confiance des références (approche C-i)

La résolution se fait par nom. Chaque `Ref` indique :

| `confidence` | Signification |
|---|---|
| `exact` | un seul candidat visible |
| `by_name` | plusieurs définitions du nom, choisie par la règle de masquage (la plus récente) |
| `ambiguous` | surcharge : plusieurs candidats selon le type du 1ᵉʳ argument ; `candidates: [...]` listés |

Le rendu montre visuellement les références non exactes (pointillés). Ce champ disparaîtra, ou
sera toujours `exact`, quand une vraie passe de résolution existera.

---

## 6. Identité : clés sémantiques

L'identité d'un bloc est une **clé stable** d'une compilation à l'autre, ce que la revue (diff
entre versions) exige. Les index internes du graphe ne sortent jamais de la crate.

```
BlockKey := namespace ":" path
namespace := "val" | "type" | "std" | "r"      # TypR sépare les espaces de noms valeurs / alias
path      := segment ("/" segment)*
segment   := nom                                # bloc nommé
           | nom "(" type_1er_param ")"         # surcharge : show(Point)
           | "#" index                          # bloc anonyme, position parmi ses frères
           | rôle                               # then, else, cond, arg0, lhs, rhs, body…
```

Les modules qualifient avec `::` : `val:Geo::area`. Exemples :
`val:norm2`, `val:norm2/a` (le bloc nommé `a`), `val:norm2/#1` (l'expression finale),
`val:norm2/#1/rhs/arg0`, `type:Point`, `std:print`.

Les segments nommés sont stables ; les segments positionnels (`#n`) le sont moins, et le diff les
apparie en second (étape 6).

---

## 7. Construction

### 7.1 Entrées

```
source ─parse─► Lang ─typing()─► Context final
                  │                  │
                  │      TypeRecorder (span → Type)
                  ▼                  ▼
              typr-graph::build(&Lang, &Context, &TypeTable) ─► BlockGraph
```

1. Parcours de `Lang` → arbre de blocs + fils locaux (§3, §4).
2. Résolution par nom des captures, avec portée lexicale simulée pendant le parcours, et
   `Context` final pour les noms de haut niveau et la stdlib (§5.3).
3. Types des blocs depuis la `TypeTable` (§7.2) ; types des paramètres depuis leurs annotations.
4. Relations de typage (`HasType`, `TypePosition`) ; plus tard `Satisfies` et `Subtype` (étape 3).
5. Élagage de la stdlib : seules les entrées référencées sont matérialisées (`origin: Std`).

### 7.2 Enregistreur de types (prérequis dans `typr-core`)

`typing(context, expr)` est l'unique point d'entrée récursif du typage. On l'enveloppe :

```rust
// processes/type_checking/type_recorder.rs
thread_local! { static RECORDER: RefCell<Option<TypeTable>> = RefCell::new(None); }

pub fn with_recording<R>(f: impl FnOnce() -> R) -> (R, TypeTable);
// dans typing() : si RECORDER est actif, table.insert(span(expr), result.value.clone())
```

- Désactivé : coût d'une lecture de `thread_local` par appel, aucune allocation.
- Clé : `(file, start, end)`. D'où le prérequis D : sans offset de fin, `a` et `a + sq(…)`
  partagent la même clé.
- `typing()` peut être rappelée sur une même expression (essais de dispatch, unification). La
  politique retenue au départ est « dernière valeur non-`Failed` ». À valider sur les cas réels à
  l'étape 0, en journalisant les conflits.
- Le WASM est mono-thread : le `thread_local` y est sans risque.

### 7.3 Offset de fin dans `HelpData` (prérequis D)

`HelpData { offset, end, file_name }`, rempli par le parseur (`nom_locate` donne la position
après chaque combinateur). Les `HelpData` synthétiques gardent `end = offset`. Bénéficiaires
annexes : surlignage exact des diagnostics et des plages LSP.

---

## 8. Projections

Le graphe plat de la v1 est une **projection** de la hiérarchie : on aplatit l'arbre des blocs,
on remplace chaque chaîne capture → `Ref` par une arête directe, puis on filtre par prédicat sur
les kinds de relation (v1 §11).

| Projection | Contenu | Étape |
|---|---|---|
| Dépendances | blocs nommés de haut niveau + `Ref` aplaties | 2 (CLI) |
| Types | `TypeDecl`, `Interface`, `HasType`, `TypePosition`, `Satisfies`, `Subtype` | 3 |
| Couplage | dérivée (fan-in/fan-out, cycles), v1 §11.3 | 7 |

---

## 9. Format d'échange (JSON)

Carte à plat des blocs (plus simple à naviguer côté client qu'un arbre imbriqué), versionnée :

```json
{
  "format": "typr-block-graph",
  "version": 1,
  "root": "val:<program>",
  "blocks": {
    "val:norm2": {
      "kind": "Function", "name": "norm2", "origin": "User",
      "span": { "file": "main.ty", "start": 98, "end": 170 },
      "type": "(Point) -> int",
      "inputs":  [ { "name": "p",  "type": "Point", "implicit": false },
                   { "name": "sq", "type": "(int) -> int", "implicit": true } ],
      "outputs": [ { "name": "out", "type": "int" } ],
      "body": {
        "children": [ "val:norm2/a", "val:norm2/#1" ],
        "wires": [ { "from": { "block": "val:norm2",   "port": "p"   },
                     "to":   { "block": "val:norm2/a", "port": "arg0" } } ]
      }
    }
  },
  "relations": [
    { "kind": "Ref", "from": "val:norm2", "port": "sq", "to": "val:sq", "confidence": "exact" },
    { "kind": "TypePosition", "from": "val:show(Point)", "to": "type:Point", "index": 0 }
  ]
}
```

Ce JSON est un **contrat externe** (CLI, WASM, playground, docs) : tout changement incompatible
incrémente `version`. Les types sont transmis sous leur forme imprimée (`Type::pretty()`) ; une
forme structurée pourra suivre si le front en a besoin.

---

## 10. Architecture des crates

```
crates/typr-core     + HelpData.end (D) + type_recorder.rs (§7.2)
crates/typr-graph    NOUVELLE — pure logique, WASM-safe (pas d'E/S, pas de tokio)
   src/model.rs        Block, Port, Wire, Relation, BlockKey
   src/build/          parcours de Lang, portée lexicale, captures, résolution par nom
   src/typing.rs       HasType, TypePosition, (étape 3) Satisfies, Subtype
   src/project.rs      projections
   src/export/         json.rs (serde), dot.rs (un niveau à la fois)
   src/diff.rs         (étape 6)
crates/typr-cli      commande `typr graph`
crates/typr-wasm     fn semantic_graph(source) -> JSON (+ multi-fichiers plus tard)
```

Dépendances : `serde`/`serde_json` (déjà dans le workspace). `petgraph` seulement si un
algorithme le justifie (cycles, étape 7) ; il reste caché derrière `model.rs`.

---

## 11. Interface utilisateur (playground)

**Pile** : React Flow (xyflow) pour les nœuds à ports (chaque bloc est un composant React),
ELK.js pour la mise en page (`elk.portConstraints`), un niveau rendu à la fois.

```
 Programme › norm2 › #1                            [←] [→]
┌──────────────────────────────────────────────────────┐
│  p: Point ●───► Access(y) ──► sq(·) ──┐               │
│                                       ▼               │
│  a: int ●────────────────────────►  ( + ) ──► ● int   │
└──────────────────────────────────────────────────────┘
```

| Geste | Effet |
|---|---|
| clic | sélectionne le bloc, surligne sa plage dans Monaco, panneau de détails (type, ports, relations, justifications) |
| double-clic | **entre** dans le bloc (s'il a un intérieur) avec une transition de zoom animée |
| Alt+clic / « aller à la définition » | navigue vers le bloc défini (`Ref`), par exemple d'un `Apply` vers la `Function` |
| fil d'Ariane | remonte à n'importe quel ancêtre |
| Précédent/Suivant | historique du navigateur |
| clic dans Monaco | synchronisation inverse : le graphe se place sur le bloc le plus interne qui contient le curseur |

**État dans l'URL** : `?view=graph&focus=<BlockKey>` (encodé). Chaque vue a son lien profond
partageable ; Précédent et Suivant marchent sans code supplémentaire. Ce paramètre s'ajoute au
contrat `INTEGRATION.md` (à mettre à jour **dans les deux dépôts à la fois**).

**Clavier (prévu, étape 7)** — à réserver dès maintenant dans la conception des composants
(nœuds focalisables, ordre de tabulation) :

| Touche | Effet |
|---|---|
| ←/→/↑/↓ | bloc voisin, en suivant les fils (amont/aval) puis les frères |
| Entrée | entrer |
| Échap / Retour arrière | remonter d'un niveau |
| `g` | aller à la définition |
| `/` | recherche d'un bloc par nom |

**Documentation (G)** : ` ```typr graph ` sur typr.github.io ajoute un bouton « voir le graphe »
qui ouvre le playground avec `?view=graph`. La variante ` ```typr graph focus=norm2 ` ouvre
directement dans un bloc.

---

## 12. Étapes

Chaque étape est livrable seule et a des critères d'acceptation vérifiables.

**Étape 0 — Prérequis dans `typr-core`** — fait (2026-09-25)
- `HelpData.end` rempli par le parseur. `cargo test --workspace` et `typr case run` restent verts.
- `type_recorder` : `with_recording` renvoie une table non vide sur l'exemple fil rouge (§14) ;
  les conflits de réécriture sont journalisés et analysés.
- Aucune régression de temps mesurable sur `typr check` quand l'enregistreur est désactivé.

Notes d'implémentation (à connaître pour la suite) :
- Chaque fonction de parsing candidate (`fn foo(s: Span) -> IResult<Span, Lang>` ou
  `IResult<Span, Vec<Lang>>` dans `elements.rs`/`mod.rs`, ~97 sites) est scindée en `foo` (fine
  couche : appelle `foo_impl`, met `end` via `Lang::set_help_data_end`) + `foo_impl` (corps
  original inchangé). Un nouveau parseur suivra ce même patron.
- `Lang::set_help_data_end` (dans `components/language/mod.rs`) fait pendant à `get_help_data` :
  toute nouvelle variante de `Lang` doit être ajoutée aux deux matches.
- `type_recorder.rs` traite `Type::Failed` **et** `Type::UnknownFunction` comme des
  placeholders (ni conflit, ni écrasement d'une valeur ferme) — découvert en step 0 sur
  l'exemple fil rouge : le corps `Lines` d'une fonction est d'abord typé `UnknownFunction`
  pendant la résolution de sa propre signature, avant d'être retypé pour de bon.
- Les `.bin` de la stdlib (`crates/typr-core/configs/bin/*.bin`) sont sérialisés en bincode
  (positionnel, pas de champ nommé) : toute évolution de `HelpData`/`Type`/`Lang` exige de les
  régénérer avec `typr std` puis un rebuild (`include_bytes!` les fige à la compilation),
  sinon `VarType::load_r()...unwrap()` panique avec une erreur de désérialisation.
- 4 régressions `typr case run` (`0004/0005/0006/0008-was-declared-as-a-public-function`) et un
  test flaky (`registry_validate::tests::formals_diff_flags_a_missing_export_and_an_arity_mismatch`,
  fail seulement en parallèle) préexistent sur `develop` — vérifié en stashant ce travail ;
  aucun lien avec ce chantier.

**Étape 1 — Crate `typr-graph`, modèle et builder** — fait (2026-09-25)
- Blocs de l'étape 1 du §4, fils locaux, captures (§3.3), `Ref` avec `confidence`, `HasType`,
  `TypePosition{0}`.
- Totalité : tout programme qui passe `typr check` produit un graphe sans panique (test sur tous
  les `cases/` et exemples de la doc) ; variants non couverts → `Opaque`.
- Tests par instantané sur des programmes courts, dont l'exemple fil rouge.

Notes d'implémentation (à connaître pour la suite) :
- `Lang::Operator{lhs, rhs}` a ses champs inversés par rapport à l'ordre syntaxique dans tout
  `typr-core` (`Op::combine`, `to_module_helper`, les bras `dollar_access`/`dot_pipe_access` de
  `processes/type_checking/mod.rs`) : `rhs` porte l'opérande *gauche*, `lhs` l'opérande *droite*.
  `typr-graph` compense une fois, à l'unique endroit où `Lang::Operator` est déstructuré
  (`crates/typr-graph/src/build/blocks.rs`).
- `typing()` sur `Lines`/`Scope` ne renvoie dans `TypeContext.lang` que la dernière instruction
  réécrite (comme la valeur d'un bloc), pas le programme entier : le builder part donc du `Lang`
  original (`parse_from_string`), pas de `result.type_context.lang`.
- `BlockGraph.blocks` est un `BTreeMap` (pas `HashMap`) pour un JSON déterministe d'un run à
  l'autre — nécessaire pour que les instantanés `insta` soient reproductibles.
- Simplifications assumées, à revoir si un cas réel l'exige : une capture ne remonte que jusqu'à
  la frontière la *plus proche* (pas de bubbling complet à travers des fonctions imbriquées,
  §3.3) ; les fils (`wires`) ne sont enregistrés qu'entre un bloc composé et ses enfants directs,
  pas au travers de plusieurs niveaux ; `Access` reçoit un corps (même vide) pour rester
  cohérent avec les autres blocs à opérandes, contrairement à la case « Intérieur : — » du §4.

**Étape 2 — CLI**
- `typr graph <fichier> [--format json|dot] [--focus <key>] [--projection deps|types]`.
- DOT : un niveau (le bloc `--focus`) avec ses ports.

**Étape 3 — Relations de types** — fait (2026-09-25)
- `Satisfies` avec `evidence`, `DeclaredAs`, `Subtype` depuis `Context.subtypes`.
- Nouveau module `crates/typr-graph/src/build/type_relations.rs`, appelé en post-passe (après le
  parcours principal, avant `b.scope.pop()`) sur les `Lang::Alias` de haut niveau uniquement —
  ces relations comparent des déclarations entre elles, pas un nœud et ses enfants, donc ça ne
  rentre pas dans la récursion par nœud de `build_expr`.
- `Satisfies` : calculée avec le même primitif que le vérificateur de types
  (`interface_satisfaction::check_interface_satisfaction`, déjà `pub` dans `typr-core`), pas
  réimplémentée. L'`evidence` associe chaque méthode requise à son bloc fournisseur — trouvé via
  `discover_methods` (nouvel helper `pub(super)`, extrait de `build_type_decl` pour être partagé),
  ou, si la méthode est satisfaite structurellement (ex. un champ de record lu comme accesseur
  trivial) sans fonction libre correspondante, l'`evidence` pointe vers le `TypeDecl` lui-même.
- `DeclaredAs` : détectée directement sur le `target_type` **tel qu'écrit** (pas réduit) d'un
  alias — si c'est un `Type::Operator(Intersection, a, b)` dont un membre est une référence
  nommée (`Type::Alias`) qui réduit vers `Type::Interface`, ça donne `DeclaredAs` vers cette
  interface. Ne se déclenche que pour une interface *nommée* (`Point & Printable`), pas pour un
  `interface { ... }` écrit en ligne dans l'intersection (rare, non couvert).
- `Subtype` : **simplification volontaire par rapport à la lettre du spec**. Le spec dit
  « depuis `Context.subtypes` », mais `Context.subtypes` (`components/context/graph.rs`) est un
  arbre construit par petits bouts à chaque `push_var_type`/`push_types`/`hoist_aliases` sur des
  types **réduits et anonymes** (des `Type::Record`, pas des `Type::Alias` nommés) — remonter
  jusqu'à un nom d'alias depuis là demanderait une correspondance inverse peu fiable, et les
  associations `union member → alias` passent en réalité par un mécanisme séparé
  (`subtype_cache`/`cache_subtype`, jamais vu par `get_supertypes`/`get_ordered_supertypes`) donc
  un vrai parcours de l'arbre les aurait de toute façon manquées. À la place : pour chaque paire
  de types déclarés dans le fichier (hors interfaces, déjà couvertes par `Satisfies`),
  `target_type.is_subtype(&other.target_type, context)` — la même primitive publique
  (`TypeSystem::is_subtype`) que le vérificateur de types utilise partout ailleurs, appliquée
  directement aux types tels qu'écrits. Toujours « depuis » la même machinerie de sous-typage,
  mais en interrogeant des paires de blocs réels du graphe plutôt qu'en essayant de parcourir
  l'arbre interne et de faire correspondre le résultat après coup. Vérifié sur un cas structurel
  simple (`list { name: char }` / `list { name: char, age: int }` → sous-typage par largeur).
- Tests unitaires dans `type_relations.rs` (un par relation) + le fil rouge (`walkthrough.rs`,
  instantané mis à jour) exerce `Satisfies` de bout en bout, CLI comprise
  (`typr graph --projection types`). `cargo test --workspace` et `typr case run` toujours verts
  (mêmes régressions préexistantes).

**Étape 4 — Playground**
- `typr-wasm::semantic_graph`, onglet Graph, navigation et gestes du §11, URL `view`/`focus`,
  synchronisation Monaco dans les deux sens, `INTEGRATION.md` mis à jour.

**Étape 5 — Documentation et blocs restants**
- ` ```typr graph ` sur typr.github.io (dépôts mis à jour ensemble).
- `Loop` (ports d'état), `Match`, `Module` (sorties `@pub`), `RCode`.

**Étape 6 — Revue**
- `diff(G_old, G_new)` apparié par `BlockKey` : blocs ajoutés, supprimés, modifiés (type,
  interface, captures) ; `typr graph diff <rev1> <rev2>` ; vue diff dans le playground (deux
  extraits de code).

**Étape 7 — Ensuite**
- Navigation clavier, dépliage sur place, `TypePosition` pour les autres positions et le retour,
  génériques, métriques et cycles, et au besoin l'enregistrement des cibles d'appel exactes
  (approche C-ii).

---

## 13. Questions ouvertes (à traiter quand le besoin se présente)

- **F — Plusieurs fichiers** : proposition par défaut, le playground reste mono-fichier et la CLI
  gère `module`/`use`/`import` multi-fichiers. À confirmer à l'étape 5.
- **Match** : un sous-bloc par bras, dont les liaisons du motif sont les entrées ? Comment
  représenter le motif lui-même ?
- **Génériques** : un bloc `Function` générique et ses instanciations (`Instantiates`) — une
  instance est-elle un bloc à part ou une annotation de l'`Apply` ?
- **Opérateurs résolus vers une fonction utilisateur** : `Operator` avec une `Ref`, ou `Apply` ?
- **Spreads de records** (`...x`, `..x`) : ports d'entrée spéciaux ?
- **Embedding** (`embed field: Type`) : méthodes héritées affichées comme sorties du `TypeDecl`
  avec une justification « transmise par `field` » ?
- **Tests** (`Test`, `TestBlock`) : dans le graphe principal ou dans une vue séparée ?
- **Types en tant que blocs** : un `TypeExpr` anonyme partagé (`int`) est-il un seul bloc
  canonique ou un bloc par occurrence ?

---

## 14. Exemple fil rouge

Vérifié avec `typr check` :

```typr
type Printable <- interface { show: (Self) -> char };
type Point <- list { x: int, y: int };

let sq <- fn(n: int): int { n * n };

let norm2 <- fn(p: Point): int {
    let a <- sq(p$x);
    a + sq(p$y)
};

let show <- fn(p: Point): char { "Point" };

let p <- Point:{ x = 3, y = 4 };
let d <- Printable(p);
let total <- norm2(p) + 12 + 3;
```

Graphe attendu, dans les grandes lignes :

```
Program
├── type:Printable   Interface      intérieur : show: (Self) -> char
├── type:Point       TypeDecl       intérieur : x: int, y: int
│                                   sorties (méthodes) : show   ← TypePosition{0} depuis val:show(Point)
│                                   ──Satisfies──► type:Printable  evidence: show ← val:show(Point)   (étape 3)
├── val:sq           Function       entrées : n: int                    sortie : int
│                                   intérieur : Operator(*) ← n, n
├── val:norm2        Function       entrées : p: Point, sq (implicite)  sortie : int
│                                   intérieur : a = Apply(callee←sq, arg0←Access(x)←p)
│                                               #1 = Operator(+)(lhs←a, rhs←Apply(sq, Access(y)←p))
│                                   ──Ref(exact)──► val:sq
├── val:show(Point)  Function       entrées : p: Point                  sortie : char
├── val:p            Record         entrées : x←3, y←4     sorties : valeur, x, y     HasType → type:Point
├── val:d            Apply          callee : Printable (validateur à la compilation)  HasType → type:Printable
└── val:total        Operator(+)    lhs ← Operator(+)(Apply(norm2, p), 12), rhs ← 3
```
