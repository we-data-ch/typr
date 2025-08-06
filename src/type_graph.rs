use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use crate::Type;
use crate::builder;
use crate::nominal_context::TypeCategory;

// Structure pour implémenter un cache LRU simple
#[derive(Debug, Clone, PartialEq)]
struct LRUCache<K: Clone + Hash + Eq, V: Clone> {
    capacity: usize,
    cache: HashMap<K, (V, usize)>, // (valeur, timestamp)
    timestamp: usize,
}

impl<K: Clone + Hash + Eq, V: Clone> LRUCache<K, V> {
    fn new(capacity: usize) -> Self {
        Self {
            capacity,
            cache: HashMap::new(),
            timestamp: 0,
        }
    }

    fn get(&mut self, key: &K) -> Option<V> {
        if let Some((value, _)) = self.cache.get(key) {
            let value_clone = value.clone();
            self.timestamp += 1;
            // Mettre à jour le timestamp en réinsérant l'entrée
            self.cache.insert(key.clone(), (value_clone.clone(), self.timestamp));
            Some(value_clone)
        } else {
            None
        }
    }

    fn insert(&mut self, key: K, value: V) {
        self.timestamp += 1;

        // Si on dépasse la capacité, supprimer l'élément le moins récemment utilisé
        if self.cache.len() >= self.capacity && !self.contains_key(&key) {
            if let Some(oldest_key) = self.cache
                .iter()
                .min_by_key(|(_, (_, timestamp))| *timestamp)
                .map(|(k, _)| k.clone())
            {
                self.cache.remove(&oldest_key);
            }
        }

        self.cache.insert(key, (value, self.timestamp));
    }

    fn clear(&mut self) {
        self.cache.clear();
        self.timestamp = 0;
    }

    fn contains_key(&self, key: &K) -> bool {
        self.cache.contains_key(key)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeGraph {
    // Stockage des relations sous forme de HashSet pour O(1) lookup
    edges: HashSet<(Type, Type)>,
    // Listes d'adjacence pour sous-types et super-types
    subtypes: HashMap<Type, HashSet<Type>>,
    supertypes: HashMap<Type, HashSet<Type>>,
    
    // Caches pour optimiser les recherches
    supertypes_cache: LRUCache<Type, Vec<Type>>,
    subtypes_cache: LRUCache<Type, Vec<Type>>,
    
    // Version du graphe pour invalider le cache
    version: u64,
}

impl TypeGraph {
    pub fn new() -> Self {
        Self::new_with_cache_size(1000) // Taille par défaut du cache
    }

    pub fn new_with_cache_size(cache_size: usize) -> Self {
        Self {
            edges: HashSet::new(),
            subtypes: HashMap::new(),
            supertypes: HashMap::new(),
            supertypes_cache: LRUCache::new(cache_size),
            subtypes_cache: LRUCache::new(cache_size),
            version: 0,
        }
    }

    /// Invalide tous les caches et incrémente la version
    fn invalidate_caches(&mut self) {
        self.supertypes_cache.clear();
        self.subtypes_cache.clear();
        self.version = self.version.wrapping_add(1);
    }

    /// Ajoute une relation de sous-typage et applique la transitivité
    pub fn add_subtype_relation(&mut self, subtype: Type, supertype: Type) {
        // Ajouter la relation directe si elle n'existe pas
        if self.edges.insert((subtype.clone(), supertype.clone())) {
            // Invalider les caches car le graphe change
            self.invalidate_caches();
            
            // Mettre à jour les listes d'adjacence
            self.supertypes
                .entry(subtype.clone())
                .or_insert_with(HashSet::new)
                .insert(supertype.clone());
            self.subtypes
                .entry(supertype.clone())
                .or_insert_with(HashSet::new)
                .insert(subtype.clone());

            // Appliquer la transitivité
            self.apply_transitivity(&subtype, &supertype);
        }
    }

    /// Applique la règle de transitivité de manière incrémentale
    fn apply_transitivity(&mut self, new_subtype: &Type, new_supertype: &Type) {
        // Étape 1 : Connecter tous les sous-types de new_subtype à new_supertype
        let subtypes = self
            .subtypes
            .get(new_subtype)
            .cloned()
            .unwrap_or_default();
        for subtype in subtypes {
            if self.edges.insert((subtype.clone(), new_supertype.clone())) {
                self.supertypes
                    .entry(subtype.clone())
                    .or_insert_with(HashSet::new)
                    .insert(new_supertype.clone());
                self.subtypes
                    .entry(new_supertype.clone())
                    .or_insert_with(HashSet::new)
                    .insert(subtype.clone());
            }
        }

        // Étape 2 : Connecter new_subtype à tous les super-types de new_supertype
        let supertypes = self
            .supertypes
            .get(new_supertype)
            .cloned()
            .unwrap_or_default();
        for supertype in supertypes {
            if self.edges.insert((new_subtype.clone(), supertype.clone())) {
                self.supertypes
                    .entry(new_subtype.clone())
                    .or_insert_with(HashSet::new)
                    .insert(supertype.clone());
                self.subtypes
                    .entry(supertype.clone())
                    .or_insert_with(HashSet::new)
                    .insert(new_subtype.clone());
            }
        }

        // Étape 3 : Connecter les sous-types de new_subtype aux super-types de new_supertype
        let subtypes = self
            .subtypes
            .get(new_subtype)
            .cloned()
            .unwrap_or_default();
        let supertypes = self
            .supertypes
            .get(new_supertype)
            .cloned()
            .unwrap_or_default();
        for subtype in subtypes {
            for supertype in &supertypes {
                if self.edges.insert((subtype.clone(), supertype.clone())) {
                    self.supertypes
                        .entry(subtype.clone())
                        .or_insert_with(HashSet::new)
                        .insert(supertype.clone());
                    self.subtypes
                        .entry(supertype.clone())
                        .or_insert_with(HashSet::new)
                        .insert(subtype.clone());
                }
            }
        }
    }

    /// Ajoute un type individuel et vérifie les relations avec tous les types existants
    pub fn add_individual_type(&mut self, typ: Type) {
        // Invalider les caches car le graphe va changer
        self.invalidate_caches();
        
        // Ajouter le type comme sous-type de Top si ce n'est pas Top
        self.add_subtype_relation(typ.clone(), builder::generic_type());

        // Obtenir tous les types distincts existants
        let cat = typ.clone().to_category();
        let distinct_types = self.get_all_distinct_types();
        let distinct_types = distinct_types
            .iter()
            .filter(|typ2| typ2.to_category() == cat && typ2.same_shape(&typ))
            .collect::<Vec<_>>();

        // Vérifier les relations avec chaque type distinct
        for other_type in distinct_types {
            if other_type.is_subtype(&typ.clone()) {
                self.add_subtype_relation(other_type.clone(), typ.clone());
            } else if typ.is_subtype(&other_type) {
                self.add_subtype_relation(typ.clone(), other_type.clone());
            }
        }
    }

    /// Obtient tous les super-types d'un type donné (version optimisée avec cache)
    pub fn get_all_supertypes(&mut self, typ: &Type) -> Vec<Type> {
        // Vérifier d'abord le cache
        if let Some(cached_result) = self.supertypes_cache.get(typ) {
            return cached_result;
        }

        // Si pas dans le cache, calculer et mettre en cache
        let mut supertypes = Vec::new();
        let mut visited = HashSet::new();
        self.collect_supertypes(typ, &mut supertypes, &mut visited);
        
        // Mettre en cache le résultat
        self.supertypes_cache.insert(typ.clone(), supertypes.clone());
        
        supertypes
    }


    /// Obtient tous les types distincts présents dans le graphe
    pub fn get_all_distinct_types(&self) -> Vec<Type> {
        let mut types = HashSet::new();
        
        // Collecter les types à partir des clés des HashMap
        for subtype in self.subtypes.keys() {
            types.insert(subtype.clone());
        }
        for supertype in self.supertypes.keys() {
            types.insert(supertype.clone());
        }

        // Convertir le HashSet en Vec pour le retour
        types.into_iter().collect()
    }

    /// Collecte les super-types récursivement
    fn collect_supertypes(&self, typ: &Type, supertypes: &mut Vec<Type>, visited: &mut HashSet<Type>) {
        if visited.contains(typ) {
            return;
        }

        if typ.clone().to_category() == TypeCategory::Generic {
            return;
        }

        visited.insert(typ.clone());

        if let Some(supers) = self.supertypes.get(typ) {
            for sup in supers {
                supertypes.push(sup.clone());
                self.collect_supertypes(sup, supertypes, visited);
            }
        }
    }

    /// Collecte les sous-types récursivement
    pub fn update(&mut self, types: &[Type]) {
        let all_types = self.get_all_distinct_types();
        let set1: HashSet<_> = types.iter().cloned().collect();
        let set2: HashSet<_> = all_types.iter().cloned().collect();
        let difference = set1.difference(&set2).cloned().collect::<HashSet<_>>();
        difference.iter()
            .filter(|typ| typ.to_category() != TypeCategory::Rest)
            .for_each(|typ| {
            self.add_individual_type(typ.clone());
        })
    }

}

impl Default for TypeGraph {
    fn default() -> Self {
        Self::new()
    }
}
