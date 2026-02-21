//! Standard library utilities (pure functions)
//!
//! This module contains pure utility functions for working with the standard library.
//! The actual loading of standard library files is done elsewhere.

use crate::components::r#type::vector_type::VecType;
use crate::components::r#type::Type;
use std::collections::HashMap;
use std::collections::HashSet;

const BLACKLIST: [&str; 59] = [
    "test_that",
    "expect_true",
    "`+`",
    "`*`",
    "`-`",
    "`/`",
    "while",
    "repeat",
    "for",
    "if",
    "function",
    "||",
    "|",
    ">=",
    "<=",
    "<",
    ">",
    "==",
    "=",
    "+",
    "^",
    "&&",
    "&",
    "/",
    "next",
    "break",
    ".POSIXt",
    "source",
    "class",
    "union",
    "c",
    "library",
    "return",
    "list",
    "try",
    "integer",
    "character",
    "logical",
    "UseMethod",
    "length",
    "sapply",
    "inherits",
    "all",
    "lapply",
    "unlist",
    "array",
    "cat",
    "rep",
    "str",
    "oldClass",
    "stop",
    "invisible",
    "capture__output",
    "paste0",
    "unclass",
    "exists",
    "vector",
    "tags",
    "paste",
];

/// Check if a function name is not in the blacklist of reserved/special functions
pub fn not_in_blacklist(name: &str) -> bool {
    let hs = BLACKLIST.iter().cloned().collect::<HashSet<&str>>();
    !hs.contains(name)
        && !name.contains("$")
        && !name.contains("~")
        && !name.contains("||")
        && !name.contains("|")
        && !name.contains("&")
        && !name.contains("/")
        && !name.contains("@")
        && !name.contains("{")
        && !name.contains("[[")
        && !name.contains("[")
        && !name.contains("(")
        && !name.contains("!=")
        && !name.contains("!")
        && !name.contains(":::")
        && !name.contains("::")
        && !name.contains(":")
        && !name.contains("-")
        && !name.contains(".POSIXt")
        && !name.contains(".")
}

/// Validate that vectorization is consistent across arguments
pub fn validate_vectorization(
    set: HashSet<(i32, VecType, Type)>,
) -> Option<HashSet<(i32, VecType, Type)>> {
    // Check there is only one type of Vector
    if set
        .iter()
        .map(|(_, vectyp, _)| vectyp)
        .collect::<HashSet<_>>()
        .len()
        == 1
    {
        let mut number_by_type: HashMap<Type, HashSet<i32>> = HashMap::new();

        for (num, _, typ) in &set {
            number_by_type
                .entry((*typ).clone())
                .or_insert_with(HashSet::new)
                .insert(*num);
        }

        // Check if each type don't have more than two related number
        for numeros in number_by_type.values() {
            if numeros.len() > 2 {
                return None;
            }
        }

        // If there is 2 numbers, must be only 1 or n
        let mut n_option: Option<i32> = None;

        for numeros in number_by_type.values() {
            if numeros.len() == 2 {
                if !numeros.contains(&1) {
                    return None;
                }
                let n = numeros.iter().find(|&&x| x != 1).copied().unwrap();
                if let Some(n_existant) = n_option {
                    if n != n_existant {
                        return None;
                    }
                } else {
                    n_option = Some(n);
                }
            } else if numeros.len() == 1 {
                let num = *numeros.iter().next().unwrap();
                if num != 1 {
                    if let Some(n_existant) = n_option {
                        if num != n_existant {
                            return None;
                        }
                    } else {
                        n_option = Some(num);
                    }
                }
            }
        }

        if set
            .iter()
            .max_by(|x, y| x.0.cmp(&y.0))
            .map(|(i, _, _)| *i)
            .unwrap_or(1)
            > 1
        {
            Some(set)
        } else {
            None
        }
    } else {
        None
    }
}
