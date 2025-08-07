use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use crate::Type;
use crate::builder;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeNode {
    pub type_info: Type,
    pub subtypes: RefCell<Vec<Rc<TypeNode>>>,
}

impl TypeNode {
    pub fn new(type_info: Type) -> Self {
        TypeNode {
            type_info,
            subtypes: RefCell::new(Vec::new()),
        }
    }

    pub fn add_subtype(&self, subtype: Rc<TypeNode>) {
        self.subtypes.borrow_mut().push(subtype);
    }

    pub fn is_leaf(&self) -> bool {
        self.subtypes.borrow().is_empty()
    }

    /// Trouve tous les nœuds feuilles (sans sous-types) accessibles depuis ce nœud
    pub fn find_leaf_nodes(&self) -> Vec<Rc<TypeNode>> {
        let mut leaves = Vec::new();
        self.find_leaf_nodes_recursive(&mut leaves);
        leaves
    }

    fn find_leaf_nodes_recursive(&self, leaves: &mut Vec<Rc<TypeNode>>) {
        let subtypes = self.subtypes.borrow();
        if subtypes.is_empty() {
            // On ne peut pas créer un Rc vers self ici, donc cette méthode
            // devrait être refactorisée pour prendre un Rc<TypeNode> en paramètre
            // Pour l'instant, on la laisse pour compatibilité
        } else {
            for subtype in subtypes.iter() {
                subtype.find_leaf_nodes_recursive(leaves);
            }
        }
    }
}

#[derive(Debug)]
pub struct InsertionInfo {
    pub parent: Type,
    pub children_to_reconnect: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Graph {
    root: Rc<TypeNode>,
    // Cache pour éviter de recréer des nœuds identiques
    node_cache: HashMap<Type, Rc<TypeNode>>,
}

impl Graph {
    /// Crée un nouveau graphe avec Generic comme nœud racine
    pub fn new() -> Self {
        let root_node = Rc::new(TypeNode::new(builder::generic_type()));
        let mut node_cache = HashMap::new();
        node_cache.insert(builder::generic_type(), root_node.clone());

        Graph {
            root: root_node,
            node_cache,
        }
    }

    /// Ajoute un type au graphe selon la logique de sous-typage
    pub fn add_type(&mut self, new_type: Type) {
        // Si le type existe déjà, on ne fait rien
        if self.node_cache.contains_key(&new_type) {
            return;
        }

        // Créer le nouveau nœud
        let new_node = Rc::new(TypeNode::new(new_type.clone()));
        self.node_cache.insert(new_type.clone(), new_node.clone());

        // Trouver les positions d'insertion appropriées avec les informations de reconnexion
        let insertion_infos = self.find_insertion_points_with_reconnection(&new_type);

        // Traiter chaque insertion
        for insertion_info in insertion_infos {
            self.add_subtype_to_parent_with_reconnection(insertion_info, new_node.clone());
        }
    }

    /// Trouve tous les nœuds où le nouveau type devrait être inséré avec informations de reconnexion
    fn find_insertion_points_with_reconnection(&self, new_type: &Type) -> Vec<InsertionInfo> {
        let mut insertion_infos = Vec::new();
        let mut visited = HashSet::new();
        
        self.find_insertion_points_with_reconnection_recursive(
            &self.root,
            new_type,
            &mut insertion_infos,
            &mut visited
        );

        insertion_infos
    }

    fn find_insertion_points_with_reconnection_recursive(
        &self,
        current: &Rc<TypeNode>,
        new_type: &Type,
        insertion_infos: &mut Vec<InsertionInfo>,
        visited: &mut HashSet<Type>
    ) {
        // Éviter les cycles
        if visited.contains(&current.type_info) {
            return;
        }
        visited.insert(current.type_info.clone());

        // Vérifier si le nœud actuel est un super-type du nouveau type
        if new_type.is_subtype(&current.type_info) {
            // Vérifier les sous-types pour voir si certains sont des sous-types du nouveau type
            let mut subtypes_of_new_type = Vec::new();
            
            for subtype in current.subtypes.borrow().iter() {
                if subtype.type_info.is_subtype(new_type) {
                    // Ce sous-type est un sous-type du nouveau type
                    subtypes_of_new_type.push(subtype.type_info.clone());
                }
            }

            // Si on a trouvé des sous-types qui sont des sous-types du nouveau type,
            // on doit insérer le nouveau type comme intermédiaire
            if !subtypes_of_new_type.is_empty() {
                insertion_infos.push(InsertionInfo {
                    parent: current.type_info.clone(),
                    children_to_reconnect: subtypes_of_new_type,
                });
                return;
            }

            // Si c'est un nœud feuille, c'est un point d'insertion simple
            if current.subtypes.borrow().is_empty() {
                insertion_infos.push(InsertionInfo {
                    parent: current.type_info.clone(),
                    children_to_reconnect: Vec::new(),
                });
            } else {
                // Sinon, continuer la recherche dans les sous-types
                let initial_size = insertion_infos.len();
                for subtype in current.subtypes.borrow().iter() {
                    self.find_insertion_points_with_reconnection_recursive(
                        subtype,
                        new_type,
                        insertion_infos,
                        visited
                    );
                }
                // Si aucun point d'insertion n'a été trouvé dans les sous-types,
                // alors le nœud actuel devient un point d'insertion
                if insertion_infos.len() == initial_size {
                    insertion_infos.push(InsertionInfo {
                        parent: current.type_info.clone(),
                        children_to_reconnect: Vec::new(),
                    });
                }
            }
        }
    }

    fn find_insertion_points_recursive(
        &self,
        current: &Rc<TypeNode>,
        new_type: &Type,
        insertion_points: &mut Vec<Type>,
        visited: &mut HashSet<Type>
    ) {
        // Éviter les cycles (bien que peu probable avec notre structure)
        if visited.contains(&current.type_info) {
            return;
        }
        visited.insert(current.type_info.clone());

        // Vérifier si le nœud actuel est un super-type du nouveau type
        if new_type.is_subtype(&current.type_info) {
            // Si c'est un nœud feuille, c'est un point d'insertion
            if current.subtypes.borrow().is_empty() {
                insertion_points.push(current.type_info.clone());
            } else {
                // Sinon, continuer la recherche dans les sous-types
                for subtype in current.subtypes.borrow().iter() {
                    self.find_insertion_points_recursive(
                        subtype,
                        new_type,
                        insertion_points,
                        visited
                    );
                }
            }
        }
        // Si ce n'est pas un super-type, on ne continue pas dans cette branche
    }

    /// Ajoute un sous-type à un parent avec reconnexion des enfants
    fn add_subtype_to_parent_with_reconnection(&mut self, insertion_info: InsertionInfo, child: Rc<TypeNode>) {
        if let Some(parent_node) = self.node_cache.get(&insertion_info.parent).cloned() {
            // Ajouter le nouveau nœud comme enfant du parent
            parent_node.add_subtype(child.clone());

            // Si il y a des enfants à reconnecter
            if !insertion_info.children_to_reconnect.is_empty() {
                // Supprimer ces enfants du parent et les ajouter au nouveau nœud
                let mut parent_subtypes = parent_node.subtypes.borrow_mut();
                
                // Collecter les nœuds à reconnecter
                let nodes_to_reconnect: Vec<Rc<TypeNode>> = parent_subtypes
                    .iter()
                    .filter(|node| insertion_info.children_to_reconnect.contains(&node.type_info))
                    .cloned()
                    .collect();
                
                // Supprimer ces nœuds de la liste des enfants du parent
                parent_subtypes.retain(|node| !insertion_info.children_to_reconnect.contains(&node.type_info));
                
                // Libérer le borrow du parent pour pouvoir emprunter le nouveau nœud
                drop(parent_subtypes);
                
                // Les ajouter comme enfants du nouveau nœud
                for node_to_reconnect in nodes_to_reconnect {
                    child.add_subtype(node_to_reconnect);
                }
                
                println!("Reconnecté {} enfants sous {}", 
                         insertion_info.children_to_reconnect.len(), 
                         child.type_info.pretty());
            }
            
            println!("Ajout de {} comme sous-type de {}", 
                     child.type_info.pretty(), insertion_info.parent.pretty());
        }
    }

    /// Reconstruit partiellement le graphe pour ajouter une nouvelle connexion
    /// Cette approche est simplifiée - une implémentation plus efficace utiliserait
    /// Rc<RefCell<TypeNode>> ou une autre stratégie
    fn rebuild_with_new_connection(&mut self, parent_type: Type, child: Rc<TypeNode>) {
        // Implémentation simplifiée qui assume que nous pouvons modifier la structure
        println!("Ajout de {:?} comme sous-type de {:?}", child.type_info, parent_type);
        
        // Dans une vraie implémentation, vous modifieriez le nœud parent ici
        // Cette approche nécessite soit RefCell, soit une restructuration du design
    }

    /// Affiche la structure du graphe
    pub fn print_structure(&self) {
        println!("Structure du graphe de types :");
        self.print_node(&self.root, 0);
    }

    fn print_node(&self, node: &Rc<TypeNode>, depth: usize) {
        let indent = "  ".repeat(depth);
        println!("{}{:?}", indent, node.type_info);
        
        for subtype in node.subtypes.borrow().iter() {
            self.print_node(subtype, depth + 1);
        }
    }

    /// Trouve tous les super-types d'un type donné
    pub fn get_supertypes(&self, target_type: &Type) -> Vec<Type> {
        let mut supertypes = Vec::new();
        self.find_supertypes_recursive(&self.root, target_type, &mut supertypes);
        supertypes
    }

    fn find_supertypes_recursive(
        &self,
        current: &Rc<TypeNode>,
        target: &Type,
        supertypes: &mut Vec<Type>
    ) {
        // Si le nœud actuel est un super-type du target
        if target.is_subtype(&current.type_info) && current.type_info != *target {
            supertypes.push(current.type_info.clone());
        }

        // Continuer la recherche dans les sous-types
        for subtype in current.subtypes.borrow().iter() {
            self.find_supertypes_recursive(subtype, target, supertypes);
        }
    }

    pub fn update(&mut self, types: &[Type]) {
        for typ in types.iter() {
            self.add_type(typ.clone())
        }
    }

}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder;
    use crate::types::ltype;

    #[test]
    fn test_add_type() {
        let subtype = ltype("{x: int, y: int}".into()).unwrap().1;
        let supertype = ltype("{x: int}".into()).unwrap().1;
        let bottom = ltype("{x: int, y: int, z: int}".into()).unwrap().1;
        let mut graph = Graph::new();
        
        graph.add_type(subtype.clone());
        graph.add_type(supertype);
        graph.add_type(bottom.clone());
        
        graph.print_structure();

        assert_eq!(vec![builder::empty_type()], graph.get_supertypes(&bottom));
    }

}
