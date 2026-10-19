## 1. Les différents moments où ce besoin apparaît

| Période   | Source                           | Besoin exprimé / observé                                                                                                             | Exemple                                                                                                         |
| --------- | -------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------- |
| 2013      | R-devel                          | Déclarer les types des paramètres et retours pour permettre notamment la composition automatique de fonctions dans des IDE/workflows | `slice(x, pivot, inclusive) : a × numeric × logical → list` ([Seminar for Statistics][1])                       |
| 2015–2017 | Checkmate / assertions           | Les développeurs écrivent manuellement des contrats de type, taille, NA, plage                                                       | chaîne scalaire, entier positif de longueur 3, proportion dans `[0,1]` ([The R Journal][2])                     |
| 2018      | vctrs                            | Besoin de rendre les fonctions **type-stable** et **size-stable**, afin qu’on puisse prédire le résultat sans exécuter le code       | prédire le type et la taille de sortie depuis ceux des entrées ([vctrs.r-lib.org][3])                           |
| 2019      | Stack Overflow                   | Demande explicite de types pour paramètres et retours                                                                                | fonction de probabilité prenant un entier/numérique et retournant un numérique ([Stack Overflow][4])            |
| 2019      | Turcotte & Vitek                 | Déterminer quels types seraient réellement utiles dans R                                                                             | distinction scalaire/vecteur, longueur des vecteurs, structure des data frames, coercition/recyclage            |
| 2020      | étude empirique sur 412 packages | Mesurer ce que les programmes R utilisent réellement                                                                                 | beaucoup de scalaires, classes, vecteurs, matrices, data frames ; polymorphisme important ([alexi turcotte][5]) |
| 2020      | même étude                       | Le typage pourrait remplacer les contrôles de paramètres écrits à la main                                                            | `is.character(x)`, `length(x)==1`, `!is.na(x)` ([alexi turcotte][5])                                            |
| 2025      | R-devel                          | Demande à nouveau très simple : ajouter une syntaxe pour documenter les types des arguments                                          | `f <- function(a:chr, b:data.frame, c:logi) ...` ([Seminar for Statistics][6])                                  |
| 2025      | R-devel / Simon Urbanek          | Confirmation qu’un mécanisme expérimental existait déjà dans R5                                                                      | `function(a, character x, integer y=1:10)` ([Seminar for Statistics][7])                                        |
| 2026      | useR!                            | Nouveau système de typage statique avec inférence, classes, data frames, polymorphisme                                               | types ensemblistes + inférence ; exemple de fonction surchargée ([DRA event platform (Indico) (Indico)][8])     |
| 2026      | D3S Prague                       | Besoin de typer les transformations **de données**, pas seulement les variables                                                      | `group_by → summarize → ggplot`, avec schéma de data frame et contraintes de `aes()` ([d3s.mff.cuni.cz][9])     |

Il y a donc une continuité étonnamment forte sur plus de dix ans.

---

# 2. Le premier besoin : « qu'est-ce que cette fonction accepte et retourne ? »

C'est probablement le besoin le plus universel.

En 2019, quelqu'un demande très explicitement comment déclarer les types des paramètres et du retour d'une fonction R, avec l'idée suivante : « `k` doit être un integer/numeric/... et la fonction retourne un numeric ». ([Stack Overflow][4])

Et dès 2013, sur R-devel, le besoin était déjà formulé de manière plus ambitieuse : connaître les types des arguments et des retours pour pouvoir composer automatiquement les fonctions dans une sorte de workflow graphique. ([Seminar for Statistics][1])

C'est important pour TypR : **la signature de fonction est donc une motivation historique très robuste**.

Le problème réel n'est pas nécessairement :

```r
add <- function(x: double, y: double): double
```

mais plutôt quelque chose comme :

```text
read_data : String → DataFrame
clean_data : DataFrame → DataFrame
fit_model : DataFrame → Model
predict   : Model × DataFrame → Vector<double>
```

Autrement dit : **rendre visible le contrat entre les blocs du programme**.

---

# 3. Mais le vrai problème R commence avec scalaire ≠ vecteur

C'est l'un des résultats les plus intéressants de la recherche empirique.

Dans R, `5` et `c(5, 6, 7)` ont le même `typeof`: `double`. Pourtant, certaines fonctions veulent véritablement une valeur unique.

L'étude de Turcotte et al. trouve que **33,33 % des paramètres observés sont des scalaires** dans leur corpus. ([alexi turcotte][5])

Ils donnent un exemple très concret :

```r
hankel.matrix <- function(n, x) {
    if (n != trunc(n))
        stop("n is not an integer")

    if (!is.vector(x))
        stop("x is not a vector")

    if (length(x) < n)
        stop("x is too short")

    ...
}
```

Ici, le développeur sait déjà quelque chose de beaucoup plus précis que :

```text
n : numeric
x : vector
```

Il sait :

```text
n : integer scalar
x : numeric vector
length(x) >= n
```

C'est exactement le genre d'information qu'un système de types R intéressant doit être capable d'exprimer. ([alexi turcotte][5])

---

# 4. Le cas encore plus R : les longueurs de vecteurs

Le papier de 2019 prend un exemple particulièrement révélateur :

```r
norm <- function(data) {
    data / c(1, 2, 4)
}
```

Puis :

```r
norm(c(5, 4, 8))
```

fonctionne comme attendu, mais :

```r
norm(c(5, 2))
```

déclenche le **recycling** de R et produit implicitement une valeur supplémentaire. 

Les auteurs proposent alors plusieurs niveaux de typage possibles :

```text
(double, double) -> double
(integer, integer) -> integer
```

ou quelque chose de plus précis :

```text
double[2] × double[2] -> double[2]
double[n] × double[n] -> double[n]
```



C'est un point extrêmement important : **la taille n'est pas seulement une propriété de données ; dans R, elle peut changer la sémantique d'une opération.**

---

# 5. Les data frames : probablement le besoin le plus spécifiquement “R”

C'est là que la recherche devient particulièrement intéressante pour ton projet.

Le papier de 2019 dit explicitement que les data frames sont au cœur des analyses R et qu'un type de data frame suffisamment riche pourrait vérifier :

* les noms des colonnes ;
* le type de chaque colonne ;
* le nombre de lignes ;
* la compatibilité entre plusieurs data frames. 

L'étude empirique de 2020 confirme leur importance : parmi les classes les plus fréquentes dans les appels observés, les matrices représentent environ 12 %, les `data.frame` 7,5 %, les formules 2 %, les facteurs 2 % et les tibbles 2 %. Les classes représentent environ 31 % des types d'arguments observés. ([alexi turcotte][5])

Et l'étude montre un exemple beaucoup plus proche des vrais bugs R :

```r
df <- data.frame(revenue = c(100, 200, 300))

vat <- df$reveneu * 0.21
```

La faute de frappe `reveneu` produit `NULL`, puis le problème peut réapparaître beaucoup plus loin sous une forme comme un résultat numérique inattendu. Le système présenté en 2026 propose justement qu'un type de data frame connaissant la colonne `revenue` puisse **refuser `df$reveneu` immédiatement**. ([DRA event platform (Indico) (Indico)][10])

À mon avis, **c'est beaucoup plus représentatif du besoin R qu'un simple `integer` contre `character`.**

---

# 6. Le cas `factor` montre que “type” signifie aussi sémantique

Le poster 2026 donne cet exemple :

```r
scores <- factor(c("1", "2", "3"))
wt <- c(0.2, 0.3, 0.5)

weighted.mean(scores, wt)
```

R accepte l'appel jusqu'à produire :

```text
Warning: '*' not meaningful for factors
NA
```

Le problème est que `scores` est stocké comme des codes entiers, mais **sémantiquement ce n'est pas un vecteur numérique ordinaire**. Le système proposé peut donc distinguer un `int<factor>` d'un entier classique et refuser l'appel à `weighted.mean`. ([DRA event platform (Indico) (Indico)][10])

C'est une autre leçon essentielle :

> Les utilisateurs de R ne veulent pas seulement typer la représentation mémoire ; ils veulent typer **ce que les données signifient**.

C'est exactement la raison pour laquelle S3, `Date`, `POSIXct`, `factor`, matrices, tibbles, etc. sont si importants.

---

# 7. Les assertions montrent ce que les développeurs veulent vraiment exprimer

C'est probablement la donnée la plus utile pour ta question.

L'étude de 2020 regarde les `stopifnot()` et assertions réellement écrits dans les packages.

Exemple :

```r
stopifnot(
    is.character(x),
    length(x) == 1L,
    !is.na(x)
)
```

Ce n'est donc pas simplement :

```text
x : character
```

mais :

```text
x : character
    ∩ length(1)
    ∩ non-NA
```

L'étude trouve 1 995 assertions dans 153 des 412 packages analysés. Leur système pouvait **complètement remplacer 50,4 %** des assertions et **simplifier 61,3 %** d'entre elles. À l'échelle de tout CRAN, ils comptent 32,3 milliers d'assertions dans 15,9 milliers de fonctions. ([alexi turcotte][5])

Checkmate arrive exactement au même endroit, mais avec une DSL d'assertions. Par exemple :

```text
character scalar
factor de longueur ≥ 1 sans NA
integer de longueur 3 avec valeurs positives
numeric scalar dans [0,1]
numeric vector positif, fini et sans NA
```

([The R Journal][2])

Donc le besoin pratique peut être formulé ainsi :

> **« Je veux pouvoir exprimer en une seule spécification ce que je suis actuellement obligé d'écrire avec `is.*`, `length()`, `is.na()`, des comparaisons et des validations personnalisées. »**

C'est une conclusion très forte.

---

# 8. Ce que l'étude de 2020 dit sur les types réellement utilisés

Le corpus est particulièrement intéressant parce qu'il ne repose pas sur des opinions : les auteurs ont observé **25 215 fonctions dans 412 packages**. ([alexi turcotte][5])

Les catégories les plus fréquentes parmi les paramètres sont notamment :

| Catégorie                    | Part approximative |
| ---------------------------- | -----------------: |
| scalaires                    |             33,3 % |
| classes                      |             23,1 % |
| vecteurs                     |             12,4 % |
| `...`                        |              8,7 % |
| `NULL`                       |              7,3 % |
| `any`                        |              7,2 % |
| listes                       |              3,4 % |
| vecteurs pouvant contenir NA |              2,8 % |

([alexi turcotte][5])

Et **58 % des fonctions** étaient monomorphes selon leur modèle, tandis que 42 % avaient au moins un paramètre ou retour polymorphe. ([alexi turcotte][5])

Cela donne une image assez différente de « R est tellement dynamique qu'on ne peut pas le typer ».

Une très grande quantité du code possède en réalité des **contrats suffisamment réguliers pour être décrits**.

---

# 9. Le point où les travaux récents vont encore plus loin : typer les pipelines de données

Le projet de D3S est particulièrement révélateur parce qu'il ne part plus du problème abstrait « ajouter des types à R ».

Il part d'un vrai bout de code R :

```r
tdec <- summarize(
    group_by(titanic, decade = round(age / 10)),
    count = sum(survived)
)

ggplot(tdec, aes(x = decade, y = count)) +
    geom_point()
```

Et pose exactement les questions qu'un développeur humain se pose :

```text
titanic.age       : numeric ?
titanic.survived  : numeric/logical ?

group_by(...)     : quelles colonnes restent ?

summarize(...)    : quelles nouvelles colonnes sont produites ?

tdec              : possède-t-il decade et count ?

aes(...)          : x et y existent-ils ?

geom_point()      : quels champs exige-t-il ?
```

Le projet cherche précisément à typer ces transformations de **forme de data frame** et la grammaire de `ggplot2`. ([d3s.mff.cuni.cz][9])

À mon sens, c'est aujourd'hui **l'exemple le plus représentatif du “type system for R” spécifique au langage**, parce qu'il combine :

```text
types des valeurs
+
longueurs
+
classes
+
schéma de data frame
+
transformation du schéma
+
polymorphisme
+
contexte d'évaluation
```

---

# 10. Et le travail 2026 confirme cette direction

Le système présenté à useR! 2026 est maintenant fondé sur des **set-theoretic types** et supporte explicitement :

```text
vecteurs
scalaires
listes
records
classes S3
data frames
polymorphisme
fonctions génériques
```

Le poster donne par exemple :

```text
df : { id : dbl, ... }<data.frame ...>
```

et peut accepter :

```r
data.frame(id = 1L:3L)
```

mais refuser :

```r
data.frame(id = c("a", "b", "c"))
```

parce que `id` devait être numérique. ([DRA event platform (Indico) (Indico)][10])

Il introduit aussi des fonctions comme :

```text
nrow : vector | array | data.frame → integer
```

et :

```text
df$col : data.frame containing col:'a → 'a
```

C'est une manière très intéressante de formaliser une chose que les développeurs R font déjà mentalement. ([DRA event platform (Indico) (Indico)][10])

---

# 11. Le problème du “type” est donc en réalité constitué de 5 couches

Après avoir regroupé tout ça, je réduirais ce que les gens cherchent à :

```text
1. Valeur
   double, integer, character, logical...

2. Cardinalité / forme
   scalar, vector, matrix, length(n)...

3. Valeur manquante
   T vs T pouvant contenir NA
   T vs NULL

4. Structure / sémantique
   factor, Date, POSIXct, data.frame, Model...

5. Contrat relationnel
   x et y ont le même type
   output dépend du type de x
   dataframe après transformation possède telles colonnes
```

C'est remarquable parce que cela converge presque exactement avec le poster 2026 : il présente explicitement le **storage type**, la **shape** et les **S3 classes** comme des informations que R cache actuellement au point d'utilisation. ([DRA event platform (Indico) (Indico)][10])

---

# 12. Quel est alors l'exemple le plus représentatif ?

Je distinguerais deux réponses.

### Le meilleur exemple de typage “général”

Une fonction R avec un vrai contrat :

```text
function(
    x : numeric vector,
    n : positive integer scalar
) -> numeric vector
```

avec éventuellement :

```text
length(x) >= n
```

C'est représentatif parce qu'il correspond directement aux assertions que les développeurs écrivent déjà dans leurs fonctions. ([alexi turcotte][5])

### Le meilleur exemple de typage “R”

Je choisirais plutôt **un pipeline de data frame** :

```r
penguins
  |> filter(...)
  |> mutate(...)
  |> summarise(...)
  |> ggplot(aes(...))
```

avec un système capable de savoir :

```text
penguins
  : {
      species : factor,
      body_mass_g : double,
      ...
    }

filter(...) 
  : conserve le schéma

mutate(...)
  : ajoute/modifie des colonnes

summarise(...)
  : change le nombre de lignes
              + produit un nouveau schéma

ggplot(...)
  : exige des colonnes existantes
```

C'est celui qui capture le mieux ce qui est **particulier à R**, plutôt que de simplement reproduire TypeScript/Rust/Haskell.

---

# 13. Et pour TypR, je pense que l'exemple encore plus fort serait celui-ci

En combinant les résultats de toutes ces sources :

```text
type Customer = {
    id: int,
    name: chr,
    revenue: dbl
}

load_customers : path → DataFrame<Customer>

normalize :
    DataFrame<{ revenue: dbl, ... }>
    → DataFrame<{ revenue: dbl, ... }>

average_revenue :
    DataFrame<{ revenue: dbl, ... }>
    → dbl1
```

Puis :

```text
customers
    |> load_customers
    |> normalize
    |> average_revenue
```

Et le système pourrait détecter :

```text
df$reveneu
```

ou :

```text
average_revenue(df_with_revenue = character)
```

ou encore :

```text
mean(revenue)
```

lorsque `revenue` n'existe pas dans le schéma.

Cela combine **les trois motivations qui reviennent le plus souvent** :

> **contrats de fonctions + structure des données + prévention des erreurs dans les pipelines.**

Et c'est beaucoup plus proche de ce que les travaux R récents essaient effectivement de résoudre que l'image classique « R mais avec `int`, `bool`, `string` ».

Un dernier élément renforce cette interprétation : vctrs affirme explicitement que lorsqu'un développeur ne peut pas prédire le **type et la taille** des variables simplement en lisant le code, cela rend le code difficile à raisonner sur papier. Leur notion de type/size stability est justement née de ce problème de lecture du code. ([vctrs.r-lib.org][3])

### Ma synthèse

Je formulerais donc le besoin utilisateur comme :

> **Les développeurs R ne demandent pas principalement de pouvoir déclarer des types. Ils demandent de pouvoir déclarer et vérifier les invariants que R laisse actuellement implicites : le type et la cardinalité des valeurs, leur structure sémantique, le schéma des data frames et les relations entre les entrées et sorties des fonctions.**

C'est, à mon avis, une formulation beaucoup plus solide pour positionner TypR.

Les sources primaires les plus importantes que j'ai trouvées sont l'étude empirique OOPSLA 2020, le travail fondateur de 2019, le projet D3S actuel et le poster useR! 2026. ([alexi turcotte][5])

[1]: https://stat.ethz.ch/pipermail/r-devel/2013-August/067331.html?utm_source=chatgpt.com "[Rd] Type annotations for R function parameters."
[2]: https://journal.r-project.org/articles/RJ-2017-028/?utm_source=chatgpt.com "The R Journal: checkmate: Fast Argument Checks for Defensive R Programming"
[3]: https://vctrs.r-lib.org/articles/stability.html?utm_source=chatgpt.com "Type and size stability • vctrs"
[4]: https://stackoverflow.com/questions/58781871/r-programming-is-it-possible-to-declare-return-or-parameter-types-of-functions?utm_source=chatgpt.com "R programming: Is it possible to declare return or parameter types of functions? - Stack Overflow"
[5]: https://reallytg.github.io/files/papers/types-for-r-oopsla20-final.pdf "Designing Types for R, Empirically"
[6]: https://stat.ethz.ch/pipermail/r-devel/2025-September/084164.html?utm_source=chatgpt.com "[Rd] Declaring Types at Function Declaration"
[7]: https://stat.ethz.ch/pipermail/r-devel/2025-November/084223.html?utm_source=chatgpt.com "[Rd] Declaring Types at Function Declaration"
[8]: https://events.digital-research.academy/event/109/contributions/482/ "useR! 2026  (6-July 9, 2026): A Type System for the R Language · DRA event platform (Indico)"
[9]: https://d3s.mff.cuni.cz/projects/primus24/?utm_source=chatgpt.com "Type systems for data-centric programming | D3S"
[10]: https://events.digital-research.academy/event/109/contributions/482/attachments/103/321/A%20Type%20System%20for%20the%20R%20Language.pdf "A Type System for the R Language"
