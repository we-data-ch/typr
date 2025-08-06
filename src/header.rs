use crate::adt_header::AdtHeader;
use crate::Lang;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub struct Header {
   function_list: String,
   pub adt: AdtHeader,
}

impl Header {
    pub fn add_generic_function(self, data: &[Lang]) -> Header {
        let data = data.iter()
            .filter(|x| Self::is_gen_func_allowed(x))
            .collect::<Vec<_>>();
        let adt_header = self.adt.set_generic_methods(
            data.iter()
                .fold(self.adt.generic_methods.clone(),
                |adt, lang| add_if_absent(adt, (*lang).clone().clone())));
        Header {
            adt: adt_header,
            ..self
        }
    }

    pub fn is_gen_func_allowed(l: &Lang) -> bool {
        if let Lang::GenFunc(_s, name, _) = l {
           if Self::in_black_list(&name) { false } else { true }
        } else { false }
    }

    pub fn in_black_list(s: &str) -> bool {
        let black_list: HashSet<&str> = [
            "seq", "append", "add", "mul", "map", "dot", "t", "print"
        ].iter().cloned().collect();
        black_list.contains(s)
    }


    pub fn is_an_untyped_function(&self, name: &str) -> bool {
        let formated_name = name.replace("__", ".");
        self.function_list.lines().any(|line| line.trim() == formated_name)
    }

    pub fn add_function_list(self, list: &str) -> Header {
        Header {
            function_list: self.function_list + list,
            ..self
        }
    }


    pub fn add_module_declarations(self, data: &[Lang]) -> Header {
        let adt_header = self.adt.set_module(
            data.iter()
                .fold(self.adt.modules.clone(),
                |adt, lang| add_if_absent(adt, lang.clone())));
        Header {
            adt: adt_header,
            ..self
        }
    }

}

impl Default for Header {
    fn default() -> Header {
        Header {
            function_list: "".to_string(),
            adt: AdtHeader::default()
        }
    }
}


fn add_if_absent(mut vec: Vec<Lang>, val: Lang) -> Vec<Lang> {
    if !vec.contains(&val) {
        vec.push(val);
    }
    vec // Retourne le nouveau vecteur
}
