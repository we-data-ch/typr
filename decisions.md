# Décisions de conception — notes de session

> Notes personnelles, complément de `lacunes.md`. Session du 2026-11-05,
> branche `develop`, version `0.6.2`.
>
> Objet : les décisions qui expliquent les comportements rencontrés dans le
> §17, avec leur rationale et les fichiers à relire. Les bugs et fragilités
> sont répertoriés séparément dans `bugs.md`.

## A. Définitions de types externes

### A.1 Un tier est un niveau de confiance, pas une provenance

**Décision.** `T1` / `T2` / `T3` notent la confiance d'une signature, pas son
origine. La provenance (`official`, `community`, `generated`, `local`) est un
champ distinct du manifeste.

**Pourquoi.** Deux signatures du même dépôt n'ont pas la même valeur selon
qu'elles sont écrites à la main ou inférées : une définition générée donne les
noms et l'arité, pas les types.

**Conséquence.** `T1` = signatures manuelles haute confiance ; `T2` =
manuelles/communautaires raisonnablement sûres, défaut des définitions
externes ; `T3` = générées par `typr gen-types` (tout le reste en `Any`). Dans
la stdlib, les sources `T2` sont **doc-only** : elles ne nourrissent pas le
checker.

**À lire.** `crates/typr-cli/src/standard_library.rs`, `typr-def.toml`,
`rfcs/0031-external-type-definitions.md`.

### A.2 `trust` est un plancher

**Décision.** Une entrée est chargée si `rank(entry.tier) >= rank(trust)`, avec
`T1 = 3 > T2 = 2 > T3 = 1`. Le défaut est `T2`.

**Pourquoi.** Le projet déclare le **minimum** de confiance qu'il accepte ;
tout ce qui est plus sûr passe. Ce n'est pas un plafond.

**Conséquence.** `trust = "T2"` accepte `T1` et `T2` mais dégrade `T3`. La
phrase du RFC « `T3` = trust generated code too » décrit `trust = "T3"`, pas le
défaut.

**À lire.** `crates/typr-cli/src/standard_library.rs` (`tier_rank`,
`meets_trust`).

### A.3 Une définition externe ne casse jamais un build (D2)

**Décision.** Un tier sous le seuil, un lock absent, un cache manquant, un
digest différent, un manifeste illisible ou une version R hors
`since`/`until` ne fait **jamais** échouer `check`/`build`/`run`. Les noms
concernés sont chargés en `UnknownFunction` (variadique, retour `Any`) avec un
warning.

**Pourquoi.** Une définition externe est une entrée non fiable : elle ne peut
que **réduire** la vérification, pas casser du code correct.

**Conséquence.** Un build peut réussir « moins vérifié ». `typr types update
<pkg>` répare le pin.

**À lire.** `resolve_locked_definitions`, `typr.lock`.

### A.4 `typr.lock` épingle des définitions de types, pas des dépendances R

**Décision.** Le lock enregistre, par paquet : `package`, `repository`,
`version`, `rev`, `digest` (sha256 du contenu), `tier` et `r_version_seen`. Il
est commité ; le cache `~/.cache/typr/types/<pkg>/<digest>/` ne l'est pas. Au
chargement, le digest est **recalculé** et comparé.

**Pourquoi.** Les dépendances R sont déjà déclarées dans `DESCRIPTION` ; le
lock sert la **reproductibilité de la connaissance de types**.

**Conséquence.** Un cache absent ou un digest différent ⇒ définition ignorée
(D2), jamais une erreur de build.

**À lire.** `crates/typr-cli/src/type_registry.rs`,
`crates/typr-cli/src/type_definition.rs`, `rfcs/0031`.

### A.5 `types vendor` archive, ne nourrit pas le build

**Décision.** `typr types vendor` copie les définitions **déjà dans le cache
global** vers `ty/vendor/<pkg>/` (manifeste inclus). `check`/`build` continuent
de lire `typr.lock` + le cache global, pas `ty/vendor/`.

**Pourquoi.** Une seule voie de résolution ; le vendor est une copie de
secours/archive, pas une source parallèle.

**Conséquence.** `vendor` seul ne rend pas un build indépendant du cache en
`0.6.2` ; il échoue si le cache manque ou si le digest ne correspond plus.

**À lire.** `crates/typr-cli/src/type_registry.rs` (commande `vendor`).

## B. Surfaces d'exécution

### B.1 MCP : stdio uniquement, exécution en mémoire

**Décision.** `typr mcp` est un serveur MCP **stdio** (rmcp, `transport-io`),
sans HTTP/SSE. `check`/`build` tournent avec `Environment::Wasm`.

**Pourquoi.** Réutiliser la configuration du playground (`typr-wasm`) : tout en
mémoire, sans système de fichiers, sans dossier de projet, sans cache ni
expansion de modules sur disque. C'est aussi ce qui rend le serveur robuste
pour un usage IA longue durée : un panic du parser est rattrapé par
`catch_unwind` et converti en diagnostic `S000`.

**Conséquence.** `build` renvoie le R même avec des erreurs de type, mais pas
après une erreur de syntaxe. Journalisation JSONL dans
`$XDG_DATA_HOME/typr/mcp-logs`, 50 sessions conservées, désactivable par
`TYPR_MCP_LOG=off`.

**À lire.** `crates/typr-mcp/src/lib.rs`, `crates/typr-cli/src/cli.rs`
(`Mcp`).

### B.2 Tutor : un grading partagé, un serveur MCP sans état

**Décision.** Le grading est une seule fonction, `typr_mcp::tutor::grade`,
utilisée par le terminal et le MCP : suppression des commentaires, règles
`forbids`, vrai compilateur en mémoire, `Expect::Compiles` ou
`Expect::TypeError`, règles `requires`, `passed = feedback.is_empty()`. Seule
la CLI persiste `.tutor-progress.json` (`passed`, `hints`) dans son workspace
`typr-tutor/` ; le serveur MCP ne persiste **rien**.

**Pourquoi.** Une seule définition de « réussi », et un serveur d'outils
stateless.

**Conséquence.** `typr tutor solution N` reste le seul usage des solutions.

**À lire.** `crates/typr-mcp/src/tutor.rs`, `crates/typr-cli/src/tutor.rs`,
`crates/typr-mcp/data/tutor/`.

## C. Release et distribution

### C.1 Le tag est le déclencheur unique

**Décision.** Le workflow `release.yml` part sur `push` d'un tag `vX.Y.Z`
(ou `gh workflow run release.yml -f tag=vX.Y.Z`). `nu publish.nu ship` prépare
la release (bump, commit, PR vers `main`, merge) puis **crée et pousse le
tag**.

**Pourquoi.** Une identité d'artefact unique et immuable, et un garde-fou
structurel contre la dérive entre canaux.

**Conséquence.** `publish.nu` n'est pas le déclencheur ; c'est l'outil qui
produit le tag.

**À lire.** `publish.nu` (constante `CRATES`, `ship`), `RELEASING.md`.

### C.2 `verify` compare trois canaux, pas les artefacts

**Décision.** Le job `verify` exige `tag == Cargo.toml` **et** l'égalité avec
`editors/vscode/package.json` et `editors/rstudio/DESCRIPTION`.

**Pourquoi.** Tous les canaux de distribution doivent porter la même version ;
`nu publish.nu sync` propage la version de `Cargo.toml` vers les deux autres.

**Conséquence.** Un écart fait échouer `verify` avec `::error::` ; comme tous
les autres jobs en dépendent par `needs`, **aucune** distribution ne part. Les
versions binaire/Wasm/Docker ne sont pas comparées : elles sont **dérivées** du
tag.

**À lire.** `.github/workflows/release.yml` (job `verify`).

### C.3 L'ordre crates.io est topologique

**Décision.** Six crates : `typr-core` → `typr-lsp` → `typr-mcp` →
`typr-graph` → `typr-cli` → `typr` (constante `CRATES` de `publish.nu` et
boucle du job `crates-io`). `typr-wasm` n'est **pas** publié sur crates.io.

**Pourquoi.** Chaque dépendance interne est déclarée en `path` **avec** un
numéro de version ; au `cargo publish`, cargo exige que la dépendance existe
déjà sur le registre à une version compatible.

**Conséquence.** Le job saute les crates déjà publiées (vérification `curl` sur
l'API crates.io), ce qui rend un replay idempotent.

**À lire.** `.github/workflows/release.yml` (job `crates-io`), `publish.nu`.

### C.4 Un replay repart du tag

**Décision.** Tous les jobs font `actions/checkout` avec
`ref: ${{ needs.verify.outputs.tag }}`.

**Pourquoi.** Rejouer doit reconstruire exactement l'arbre releasé, pas
`develop`.

**Conséquence.** Un correctif poussé après le tag n'est pas inclus ; déplacer
le tag est exclu (crates.io est irréversible, seul le *yank* existe). Le replay
sert aux **pannes d'environnement** (token, réseau, registre, runner) ; un bug
de code exige une nouvelle version, donc un nouveau tag. `ship --resume` ne
concerne que l'orchestration locale interrompue **avant** le tag.

**À lire.** `.github/workflows/release.yml`, `publish.nu`.

## D. Documentation

### D.1 Les commentaires de doc sont des `#`, pas des `#'`

**Décision.** Un commentaire de doc est un bloc de commentaires `#` ordinaires
immédiatement avant une déclaration. Le parser ne retire que le **premier**
`#`, donc `#' Texte` produit `' Texte` (apostrophe littérale).

**Pourquoi.** TypR n'utilise pas la convention roxygen : la doc est extraite de
son propre AST de commentaires (`Lang::Comment`), puis attachée par
`doc_attach`.

**Conséquence.** `parse_doc` ne reconnaît que quatre choses : la **première
ligne** = titre ; `@param <nom> <texte>` sur **une ligne** ; `@return <texte>`
sur une ligne ; `@examples` qui capture **verbatim jusqu'à la fin** (à mettre
en dernier). Tout le reste devient la description, jointe par des espaces :
`@title`, `@description`, `@details`, `@seealso` apparaissent donc
**littéralement**. `@pub`, `@export`, `@testable` sont des annotations de
déclaration, pas des tags de doc.

**À lire.** `crates/typr-core/src/processes/parsing/mod.rs`
(`comment_impl`), `crates/typr-core/src/processes/spg/doc_attach.rs`,
`crates/typr-cli/src/rd_renderer.rs` (`parse_doc`).

### D.2 `document` ne résout pas les définitions externes

**Décision constatée.** `document_impl` construit directement
`Context::default().set_environment(Environment::Project)` et n'appelle
**jamais** `load_project_type_definitions` : ni `typr.lock`, ni cache de types.

**Pourquoi (probable).** Historique : `document` a été écrit avant la
généralisation des définitions externes.

**Conséquence.** Le type-check de `document` ne voit que la stdlib ; les noms
externes retombent en `UnknownFunction`. Les `.Rd` peuvent donc afficher des
types `Any`/imprécis, et `document` peut accepter un programme que `check`
refuserait. Ce n'est probablement pas voulu : voir `bugs.md` **B-08**.

**À lire.** `crates/typr-cli/src/project.rs` (`document_impl`).

## E. Type system

### E.1 `None` n'est pas `Some(vec![])`

**Décision.** Dans `unification_helper`, `None` = les deux types ne sont **pas
unifiables** (échec normal, ex. `int` vs `char`) ; `Some(vec![])` = unifiables
**sans nouvelle liaison** (types concrets déjà égaux, `Any`, `T` avec `T`).
`try_unify` conserve la distinction ; `unify` l'écrase en vec vide.

**Pourquoi.** Les appelants doivent pouvoir distinguer « ça ne passe pas » de
« ça passe trivialement ». Avant, les deux cas renvoyaient un vec vide.

**Conséquence.** Choisir le bon point d'entrée selon que l'échec doit être
propagé ou non.

**À lire.** `crates/typr-core/src/processes/type_checking/unification.rs`.

### E.2 L'occurs check ne couvre que `Generic`

**Décision.** `type_contains_generic` parcourt toute la structure, mais ne
reconnaît que `Type::Generic` ; `IndexGen`, `LabelGen` et `KindedGen` ne sont
pas couverts. Le check n'est branché que dans l'arm `Generic`.

**Pourquoi.** Seuls les `Generic` peuvent, dans les chemins actuels, produire
une substitution autoréférentielle (`T ↦ [T]`) qui ferait boucler
`type_substitution`.

**Conséquence.** Lacune **latente** pour les autres variables ; voir
`bugs.md` **B-06**.

**À lire.** `unification.rs` (`type_contains_generic`,
`unification_helper`).

### E.3 `PartialEq` des génériques est volontairement lâche

**Décision.** `PartialEq` considère tous les `Generic` égaux, tous les
`IndexGen` égaux, tous les `LabelGen` égaux, et deux `KindedGen` égaux si leur
kind est égal. L'identité réelle passe par `same_generic_key` (nom) ou
`exact_equality`.

**Pourquoi.** Plusieurs chemins d'inférence (notamment l'inférence de retour
des lambdas non typées) s'appuient encore sur cette égalité lâche.

**Conséquence.** `==` n'est **pas** un test d'identité de générique. Le
sous-typage met en cache sauf si un rigid est mentionné. Voir aussi
`bugs.md` **B-05** pour la tension avec `Hash`.

**À lire.** `crates/typr-core/src/components/type/mod.rs` (`PartialEq`,
`Hash`), `unification.rs` (`same_generic_key`, `exact_equality`).

### E.4 Un raffinement est un intervalle sur une mesure

**Décision.** Pas de prédicat libre : `Measure` vaut `Value` (le scalaire) ou
`Length` (`length(x)`), et `RefinementSet` garde au plus **un intervalle par
mesure**, trié. `&` est le meet d'intervalles, une contradiction donne
l'intervalle vide, l'implication est l'inclusion.

**Pourquoi.** Cette représentation donne gratuitement `Hash`/`Eq` canoniques et
une algèbre simple, sans solveur.

**Conséquence.** `!=` ne peut pas être représenté dans la branche vraie (une
réunion de deux intervalles), mais la branche fausse le transforme en `==` par
négation.

**À lire.** `crates/typr-core/src/components/type/refinement.rs`,
`processes/type_checking/refinement_check.rs`.

### E.5 Le narrowing est syntaxique

**Décision.** Le narrowing part uniquement des conditions : sujet = variable
nue (`Value`) ou `length(x)` (`Length`) ; `&&` dans la branche vraie, `||` dans
la fausse, `!` inverse ; les faits d'une même variable sont des meets ; les
longueurs sont arrondies à l'entier. Une seule liaison visible, base réductible
à `Vec`/`Integer`/`Number`, intersection vide ⇒ pas de narrowing.

**Pourquoi.** Peu coûteux, prévisible, sans solveur ; le runtime
(`coerce_to`) vérifie le reste.

**Conséquence.** `x != 5` ne narrow que dans le `else` ; `x == 5` ne narrow que
dans le `then`.

**À lire.** `crates/typr-core/src/processes/type_checking/narrowing.rs`.

### E.6 Le sous-typage des fonctions est volontairement faible

**Décision.** L'arm fonction de `is_subtype_raw` compare l'arité, puis chaque
paramètre et le retour avec `strict_subtype`. `strict_subtype` n'accepte que
`Empty`, `Any`, l'égalité, `<: Generic` nu, et la récursion sur `Function` ;
tout le reste ⇒ `false`. Les **noms** de paramètres sont ignorés.

**Pourquoi.** Comparer les types structurellement sans faire entrer les noms de
paramètres dans l'égalité, et rester conservateur.

**Conséquence.** Un record plus large, une union, un raffinement ou une
interface satisfaite dans un paramètre/retour fait échouer le sous-typage de
fonction, alors que `is_subtype_raw` l'accepterait au niveau supérieur. Les
paramètres sont comparés de façon **covariante** (`p1 <: p2`), pas
contravariante. Voir `bugs.md` **B-02** et **B-03** : les deux sont liés, il
faut décider d'une politique avant de renforcer l'un ou l'autre.

**À lire.** `mod.rs` (`is_subtype_raw`, `strict_subtype`).

### E.7 S3 est généré, pas écrit à la main

**Décision.** Une fonction typée de premier niveau est émise comme méthode
`nom.Classe` (nom générique d'abord). `generic_functions.R` écrit le stub
`nom <- function(x, ...) UseMethod('nom', x)`. `Type::Any`, un générique nu,
une interface pure et `Foreign<T>` retombent sur `.default`, donc une méthode
`nom.default` est émise. `generic_functions.R` est trié par nom.

**Pourquoi.** Pas de dispatch manuel à maintenir ; l'ordre déterministe évite
de reshuffler entre builds ; le fallback `.default` garantit que `UseMethod`
trouve toujours quelque chose.

**Conséquence.** `Var::display_type` n'ajoute le suffixe `.Classe` que si le
nom est un **method binding** connu. `nom<Type>(...)` force `nom.Type` et
**contourne** `UseMethod`, après vérification qu'une méthode existe.

**À lire.** `crates/typr-core/src/processes/transpiling/mod.rs`,
`crates/typr-core/src/components/language/var.rs`,
`crates/typr-core/src/components/context/{mod.rs,vartype.rs,graph.rs}`,
`crates/typr-cli/src/project.rs` (`write_header`).

### E.8 `@extern` / `@importFrom` n'ont pas de stub générique

**Décision.** Ces noms sont exclus de `get_all_generic_functions`, donc
`generic_functions.R` ne leur écrit pas de `UseMethod`.

**Pourquoi.** Ils désignent une fonction R **déjà existante** (parfois
`pkg::fn`). Le stub la **masquerait** au site d'appel : l'appel tomberait sur
le stub et dispatcherait au lieu d'appeler la vraie fonction — c'est la forme
du « bug nlevels ».

**Conséquence.** Les appels sont transpilés directement vers leur nom R.

**À lire.** `components/context/mod.rs` (`get_all_generic_functions`),
`soundness_transpilation.md` (matrice d'interop Phase D).

### E.9 `Kind` ne couvre que quatre catégories structurelles

**Décision.** `%` = record, `@` = interface, `^` = string, `?` = boolean.
`Number` et `Integer` n'ont volontairement pas de `Kind`.

**Pourquoi.** Les génériques numériques passent par `IndexGen` (arithmétique
d'index/longueurs) et `type_arithmetic::accepts_number_kind` ; `KindedGen` ne
sert qu'aux catégories **structurelles**.

**Conséquence.** `type_kind` renvoie `None` pour `Number` (et tout type non
réductible), ce qui est **permissif** ; mais l'arm `KindedGen` de l'unification
rejette **explicitement** `Number` et `Integer` avant ce `None` permissif.
`@` sert aussi de séparateur de bound (`Lovable@A`).

**À lire.** `crates/typr-core/src/components/type/kind.rs`,
`unification.rs` (arm `KindedGen`).
