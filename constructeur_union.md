Voici la fiche de spécification complète, propre, formelle, cohérente avec tout ce que nous avons défini ensemble, et parfaitement intégrable dans la documentation de TypR.  
Elle décrit la syntaxe, la sémantique statique, la sémantique runtime, la monomorphisation, la hiérarchie S3, et la transpilation des constructeurs d’unions.

Je l’ai structurée comme une vraie spec de langage — concise, rigoureuse, modulaire, et prête à être intégrée dans ton projet.

---

📘 Spécification — Constructeurs d’Unions dans TypR

1. 🎯 Objectif
Définir la syntaxe, la sémantique statique, la sémantique runtime et les règles de transpilation des constructeurs de types union dans le langage TypR.

Les constructeurs d’union doivent :

- être cohérents avec les constructeurs de records (T:{ ... })
- permettre l’autocomplétion LSP via Union.
- appeler les constructeurs natifs des variantes
- produire une hiérarchie S3 compatible avec le dispatch polymorphe
- ne pas introduire de runtime ADT (pas d’enum Rust)

---

2. 🧩 Syntaxe

2.1. Déclaration d’un type union
`
type Color <- .Red | .Blue | Rgb;
`

- .Red et .Blue sont des tags (constructeurs sans payload)
- Rgb est un alias vers un type record (constructeur avec payload)

2.2. Constructeurs d’union

2.2.1. Variantes sans payload (tags)
`
Color.Red
Color.Blue
`

2.2.2. Variantes avec payload (records ou alias)
`
Color.Rgb:{ r: 255, g: 128, b: 64 }
`

2.2.3. Règle générale
`
Union.Variant
Union.Variant:{ ... }
`

---

3. 🧠 Sémantique statique

3.1. Typage des constructeurs
Pour une union :

`
type U <- A | B | C;
`

Alors :

- U.A : U
- U.B : U
- U.C : U

Même si :

- A est un tag
- B est un record
- C est un alias

3.2. Règle de typage formelle
`
Γ ⊢ Variant : T
Variant ∈ U
———————————————
Γ ⊢ U.Variant : U
`

3.3. Pas de struct ADT
- Pas de tag interne
- Pas de payload encapsulé
- Pas de représentation uniforme

Le type union est purement statique.

---

4. 🏗️ Monomorphisation

4.1. Génération des constructeurs
Pour chaque variante V de l’union U, la monomorphisation génère ou enrichit un constructeur R :

- si V est un tag → générer un constructeur vide
- si V est un alias → enrichir le constructeur existant
- si V est un record → générer un constructeur record

4.2. Hiérarchie S3 générée
Chaque constructeur de variante doit ajouter automatiquement :

`
class = c("<Variant>", "<Union>", "list")
`

Exemples :

.Red
`
class = c("Red", "Color", "list")
`

.Blue
`
class = c("Blue", "Color", "list")
`

Rgb
`
class = c("Rgb", "Color", "list")
`

4.3. Règle formelle
Pour toute variante V ∈ U :

`
class(V(x)) = c(V, U, BaseClass(V))
`

---

5. ⚙️ Transpilation vers R

5.1. Variantes sans payload
TypR :
`
Color.Red
`

R :
`r
Red()
`

5.2. Variantes avec payload
TypR :
`
Color.Rgb:{ r: 10, g: 20, b: 30 }
`

R :
`r
Rgb(r = 10, g = 20, b = 30)
`

5.3. Structure S3 générée
`r
structure(
    list(...),
    class = c("<Variant>", "<Union>", "list")
)
`

---

6. 🧬 Sémantique runtime (R/S3)

6.1. Dispatch polymorphe
Grâce à la hiérarchie S3 :

`
class(obj) = c("Red", "Color", "list")
`

Le dispatch S3 suit :

1. method.Red
2. method.Color
3. method.list
4. method.default

6.2. Conséquences
- Color peut définir des méthodes génériques pour toutes ses variantes
- chaque variante peut surcharger individuellement
- compatibilité totale avec le modèle S3

---

7. 🧭 Règles LSP

7.1. Suggestions après Union.
Le LSP doit proposer :

- toutes les variantes de l’union
- uniquement celles ayant un constructeur

Exemple :
`
Color.
→ Red, Blue, Rgb
`

7.2. Suggestions après Union.Variant:{
Si Variant est un record :

`
Color.Rgb:{
→ r, g, b
`

---

8. 🧾 Résumé

| Aspect | Spécification |
|-------|---------------|
| Syntaxe | Union.Variant / Union.Variant:{...} |
| Typage | Toutes les variantes → type de l’union |
| Runtime | Constructeurs natifs, pas d’ADT |
| S3 | c(Variant, Union, list) |
| LSP | Suggestions sur Union. et sur les champs |
| Transpilation | Appel direct au constructeur natif |
