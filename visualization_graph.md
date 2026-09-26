Formalisation du système comme une Semantic Graph IR de TypR, située après résolution des noms et suffisamment proche du système de types pour pouvoir représenter tes relations entre expressions, types, interfaces et dépendances.

Voici une première spécification assez précise, mais encore indépendante de l'implémentation Rust.

TypR Semantic Graph IR

1. Objectif

La Semantic Graph IR (SGIR) est une représentation intermédiaire du programme TypR sous forme de graphe orienté typé.

Elle représente les entités sémantiques du programme et les relations entre ces entités.

Elle a trois objectifs :

1. fournir une représentation exploitable par le compilateur ;
2. permettre des analyses structurelles du code ;
3. fournir une représentation commune aux outils de développement.

La SGIR n'est pas une représentation syntaxique du programme. Elle représente les relations sémantiques découvertes par le compilateur.

---

2. Modèle mathématique

Un programme TypR compilé est associé à un graphe :

[
G = (V,E,\tau_V,\tau_E,A)
]

où :

- V est l'ensemble des nœuds ;
- E \subseteq V \times V est l'ensemble des arêtes orientées ;
- \tau_V : V \rightarrow NodeKind associe un genre à chaque nœud ;
- \tau_E : E \rightarrow EdgeKind associe un genre à chaque arête ;
- A est l'ensemble des attributs associés aux nœuds et aux arêtes.

Une arête :

[
e = (u,v)
]

signifie que l'entité u entretient la relation \tau_E(e) avec l'entité v.

Le graphe est orienté.

Il peut contenir des cycles.

---

3. Identité des nœuds

Chaque nœud possède un identifiant stable pendant la durée de vie du graphe :

NodeId

L'identifiant n'encode pas la nature du nœud.

Le type du nœud est obtenu par :

NodeKind

Exemple :

NodeId(42)
    kind = Function

Les identifiants peuvent être implémentés avec "petgraph::stable_graph::NodeIndex" ou un mécanisme d'identifiants stable équivalent.

---

4. Nœuds

Le type fondamental est :

enum NodeKind {
    Module,
    Function,
    Parameter,
    Variable,
    Expression,

    Type,
    Interface,

    Field,
    Constructor,

    Literal,
}

Cette liste est extensible.

Un nœud représente une entité sémantique identifiable par le compilateur.

---

4.1 Module

Un module représente une unité de compilation ou un module TypR.

Module

Exemple :

module math

---

4.2 Function

Une fonction représente une fonction déclarée ou une fonction générée par le compilateur.

Function

Une fonction peut être reliée :

Function ──HasType────► FunctionType
Function ──Contains───► Expression
Function ──Uses───────► Variable
Function ──Calls──────► Function

---

4.3 Variable

Une variable représente une définition ou référence résolue.

Une référence à une variable doit pointer vers le même nœud sémantique que sa définition lorsque celle-ci est connue.

Exemple :

let x = 10
let y = x

produit notamment :

x ──UsedBy──► y

ou, selon la convention retenue :

y ──Uses──► x

La seconde convention est recommandée.

---

5. Expressions

Une expression peut être représentée comme un nœud lorsque son identité est utile à l'analyse.

Expression

Exemple :

x + y

peut produire :

Expression(Add)
   ├── Uses ──► x
   └── Uses ──► y

Les expressions purement syntaxiques qui n'apportent aucune information utile à l'analyse peuvent être omises.

La SGIR n'a donc pas obligation de représenter chaque élément de l'AST.

---

6. Types

Les types sont des entités du graphe.

Type

Exemples :

Int
String
Vector<Int>
Person
(Self) -> Empty

Les relations entre types sont représentées par des arêtes.

Exemple :

Vector<Int>
    │
    └── ElementType ──► Int

---

7. Interfaces

Une interface TypR est représentée comme un nœud :

Interface

Exemple :

type Printable <- interface {
    print: (Self) -> Empty
}

produit conceptuellement :

Printable
    │
    └── Requires ──► print
                         │
                         └── HasType ──► (Self) -> Empty

Une implémentation produit :

Person ──Implements──► Printable

---

8. Relations

Les relations sont représentées par :

enum EdgeKind {
    Contains,

    Defines,
    Uses,

    Calls,

    HasType,

    ElementType,
    FieldType,

    Implements,
    Requires,

    Refines,

    Returns,
    ParameterType,

    Instantiates,
}

Cette liste est volontairement limitée.

Une relation doit être ajoutée uniquement lorsqu'elle possède une signification sémantique exploitable.

---

9. Direction des relations

La direction doit être définie par la sémantique de la relation.

La convention générale est :

«L'arête part de l'entité qui dépend ou possède une relation vers l'entité dont elle dépend.»

Ainsi :

Function ──Uses──► Variable
Function ──Calls──► Function
Person ──Implements──► Printable
Function ──HasType──► FunctionType
Vector ──ElementType──► Int

Cette convention permet notamment d'effectuer facilement une analyse des dépendances sortantes.

---

10. Exemple complet

Pour :

type Printable <- interface {
    print: (Self) -> Empty
}

type Person = {
    name: String
}

fn print_person(person: Person) -> Empty {
    print(person.name)
}

la SGIR peut contenir :

                 ┌──────────────┐
                 │  Printable   │
                 └──────┬───────┘
                        ▲
                    Implements
                        │
                 ┌──────┴───────┐
                 │    Person    │
                 └──────┬───────┘
                        │
                    FieldType
                        │
                        ▼
                      String


┌─────────────────┐
│ print_person    │
└───────┬─────────┘
        │
       HasType
        │
        ▼
 (Person) -> Empty
        │
       Uses
        ▼
      Person

---

11. Projections

Le graphe sémantique constitue la représentation fondamentale.

Les graphes spécialisés sont des projections du graphe.

Soit :

[
G=(V,E)
]

Une projection est définie par un prédicat :

[
P : E \rightarrow {true,false}
]

et produit :

[
G_P=(V,{e\in E\mid P(e)})
]

---

11.1 Dependency Graph

Le dependency graph sélectionne les relations représentant une dépendance.

Uses
Calls
Implements
Requires
HasType

Exemple :

Function A ──Uses──► Type B
Function A ──Calls──► Function C

---

11.2 Type Graph

Le type graph sélectionne principalement :

HasType
ElementType
FieldType
Implements
Requires
Refines
Instantiates

Il permet d'explorer la structure du système de types.

---

11.3 Coupling Graph

Le coupling graph est une analyse dérivée.

Il ne correspond pas nécessairement à une catégorie d'arêtes unique.

Le couplage peut être calculé à partir des relations :

Uses
Calls
HasType
Implements
Requires
Refines

et de leur évolution dans le temps.

Ainsi :

[
Coupling(A,B)
]

est une propriété calculée du sous-graphe reliant A et B.

---

12. Abstraction

L'abstraction n'est pas nécessairement une arête.

Elle décrit quelles propriétés d'un nœud ou d'une interface sont exposées ou cachées.

Par exemple :

Implementation
      │
      ▼
   Interface

L'interface constitue une frontière abstraite.

Une analyse peut donc déterminer :

Concrete node
      │
      │ Implements
      ▼
Abstract node

et identifier quelles propriétés peuvent varier sans modifier les consommateurs.

---

13. Construction du graphe

La SGIR est construite progressivement pendant la compilation.

Pipeline recommandé :

Source
  │
  ▼
AST
  │
  ▼
Name Resolution
  │
  ▼
HIR
  │
  ├─────────────► Type Checking
  │
  ▼
Semantic Graph Builder
  │
  ▼
SGIR

Le graphe doit être construit à partir du HIR et des informations produites par la résolution des noms et le type checking.

Il ne doit pas être construit directement depuis le texte source.

---

14. Propriété importante : référence aux entités du compilateur

Les nœuds du graphe doivent pouvoir référencer les entités internes existantes.

Exemple :

enum NodeKind {
    Function(FunctionId),
    Variable(VariableId),
    Type(TypeId),
    Interface(InterfaceId),
    Expression(ExprId),
}

Ainsi le graphe ne duplique pas les informations du compilateur.

Il fournit une couche relationnelle au-dessus de celles-ci.

---

15. API minimale

Une API minimale peut être :

struct SemanticGraph {
    graph: StableDiGraph<Node, Edge>,
}

avec :

impl SemanticGraph {
    fn add_node(&mut self, node: Node) -> NodeId;

    fn add_edge(
        &mut self,
        from: NodeId,
        to: NodeId,
        kind: EdgeKind,
    ) -> EdgeId;

    fn node(&self, id: NodeId) -> &Node;

    fn edges_from(&self, id: NodeId) -> impl Iterator<Item = Edge>;

    fn edges_to(&self, id: NodeId) -> impl Iterator<Item = Edge>;
}

Une couche de requête peut ensuite être ajoutée :

struct GraphQuery<'a> {
    graph: &'a SemanticGraph,
}

avec par exemple :

fn dependencies_of(node: NodeId) -> Vec<NodeId>;

fn dependents_of(node: NodeId) -> Vec<NodeId>;

fn implementations_of(interface: NodeId) -> Vec<NodeId>;

fn users_of(node: NodeId) -> Vec<NodeId>;

fn callers_of(function: NodeId) -> Vec<NodeId>;

---

16. Sérialisation

La SGIR peut être exportée sous une forme indépendante de "petgraph".

Format conceptuel :

{
  "nodes": [
    {
      "id": 1,
      "kind": "Function",
      "name": "print_person"
    },
    {
      "id": 2,
      "kind": "Interface",
      "name": "Printable"
    }
  ],
  "edges": [
    {
      "from": 1,
      "to": 2,
      "kind": "Uses"
    }
  ]
}

L'export doit être considéré comme une représentation externe de la SGIR et non comme son implémentation interne.

---

17. Visualisation

Le compilateur doit pouvoir produire une représentation Graphviz/DOT :

typr graph
typr graph --types
typr graph --dependencies
typr graph --coupling

Exemple :

typr graph --types --format dot

La visualisation interactive pourra être implémentée ultérieurement sans modifier le modèle sémantique.

---

18. Évolution future

La SGIR est conçue pour permettre des analyses temporelles.

Pour deux versions :

[
G_t
]

et :

[
G_{t+1}
]

on peut définir :

[
\Delta G = G_{t+1} - G_t
]

permettant d'observer :

- ajout de nœuds ;
- suppression de nœuds ;
- ajout de relations ;
- suppression de relations ;
- modification des types ;
- modification des interfaces ;
- changement de dépendances.

Cela permet ultérieurement de calculer des propriétés telles que :

coupling evolution
interface stability
dependency volatility
change propagation
architectural drift

sans modifier la représentation fondamentale du programme.

---

19. Principe architectural

La règle fondamentale de la SGIR est :

«Le graphe représente les relations ; les entités du compilateur restent la source de vérité.»

Ainsi :

AST
 │
 ├── syntaxe
 │
 ▼
HIR
 │
 ├── identité
 ├── résolution
 ├── expressions
 │
 ▼
Type System
 │
 ├── types
 ├── interfaces
 ├── contraintes
 │
 ▼
Semantic Graph
 │
 ├── relations
 ├── dépendances
 ├── structure
 └── historique futur

La SGIR devient donc une couche relationnelle du compilateur, et non un deuxième AST.

---

20. Implémentation de référence

Implémentation initiale recommandée :

[dependencies]
petgraph = "..."
slotmap = "..."
serde = { version = "...", features = ["derive"] }
serde_json = "..."

Structure :

typr-core/
└── src/
    ├── ast/
    ├── hir/
    ├── resolve/
    ├── types/
    └── graph/
        ├── mod.rs
        ├── node.rs
        ├── edge.rs
        ├── graph.rs
        ├── builder.rs
        ├── query.rs
        └── projection.rs

Le graphe interne utilise "StableDiGraph".

Les projections et analyses ne manipulent pas directement les détails de "petgraph".

Cette séparation permet de remplacer ultérieurement "petgraph" sans modifier le modèle conceptuel de TypR.Le point que je trouve particulièrement intéressant pour TypR est que la SGIR pourrait devenir une véritable IR d'analyse, au même titre que ton HIR est une IR de compilation. Ensuite, le couplage, les interfaces, les types, les dépendances et même l'évolution Git ne seraient plus des systèmes séparés : ce seraient des analyses différentes d'une même structure.
