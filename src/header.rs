use crate::adt_header::AdtHeader;
use crate::Lang;
use std::collections::HashSet;
use crate::Adt;
use crate::Type;
use crate::function_type::FunctionType;

#[derive(Debug, Clone, PartialEq)]
pub struct Header {
   function_list: String,
   pub metadata: AdtHeader,
   fns: Vec<(Lang, FunctionType)>,
}

impl Header {
    pub fn add_generic_function(self, data: &[Lang]) -> Header {
        let data = data.iter()
            .filter(|x| Self::is_gen_func_allowed(x))
            .collect::<Vec<_>>();
        let adt_header = self.metadata.set_generic_methods(
            data.iter()
                .fold(self.metadata.generic_methods.clone(),
                |adt, lang| add_if_absent(adt, (*lang).clone().clone())));
        Header {
            metadata: adt_header,
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
        let adt_header = self.metadata.set_module(
            data.iter()
                .fold(self.metadata.modules.clone(),
                |adt, lang| add_if_absent(adt, lang.clone())));
        Header {
            metadata: adt_header,
            ..self
        }
    }

    pub fn push(self, fn_lang: Lang, fn_typ: FunctionType) -> Self {
        Self {
            fns: self.fns.iter().chain([(fn_lang, fn_typ)].iter()).cloned().collect(),
            ..self
        }
    }

    pub fn get_true_fn_type(&self, f_lang: &Lang) -> Option<FunctionType> {
        self.fns.iter()
            .find(|(fn_lang, _)| fn_lang == f_lang)
            .map(|(_, fn_typ)| fn_typ)
            .cloned()
    }

}

impl Default for Header {
    fn default() -> Header {
        Header {
            function_list: include_str!("../configs/src/functions.txt").to_string(),
            metadata: AdtHeader::default(),
            fns: vec![],
        }
    }
}


fn add_if_absent(mut vec: Vec<Lang>, val: Lang) -> Vec<Lang> {
    if !vec.contains(&val) {
        vec.push(val);
    }
    vec // Retourne le nouveau vecteur
}
