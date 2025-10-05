use crate::adt_header::AdtHeader;
use crate::Lang;
use std::collections::HashSet;
use crate::function_type::FunctionType;
use crate::config::TargetLanguage;


#[derive(Debug, Clone, PartialEq)]
pub struct Header {
   function_list: String,
   generic_function_list: String,
   pub metadata: AdtHeader,
   fns: Vec<(Lang, FunctionType)>,
}

//main
impl Header {
    pub fn add_lang(self, data: &[Lang]) -> Header {
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
        let formated_name2 = format!("\"{}\"", formated_name);
        self.function_list.lines()
            .any(|line| (line.trim() == formated_name) || line.trim().contains(&formated_name2))
    }

    pub fn add_function_list(self, list: &str) -> Header {
        Header {
            function_list: self.function_list + list,
            generic_function_list: self.generic_function_list + &Self::get_generics(list),
            ..self
        }
    }

    fn get_generics(list: &str) -> String {
        list.lines()
            .filter(|line| line.contains(".default"))
            .map(|line| line.replace(".default", ""))
            .collect::<Vec<_>>().join("\n")
    }

    pub fn not_generic_yet(&self, name: String) -> bool {
        let formated_name = name.replace("__", ".");
        let formated_name2 = format!("\"{}\"", formated_name);
        !self.generic_function_list.lines()
            .any(|line| (line.trim() == formated_name) || line.trim().contains(&formated_name2))
    }

    pub fn is_generic(&self, name: String) -> bool {
        !self.not_generic_yet(name)
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

    pub fn set_function_list(self, lang: TargetLanguage) -> Self {
        let res = match lang {
            TargetLanguage::R => include_str!("../configs/src/functions_R.txt").to_string(),
            TargetLanguage::JS => include_str!("../configs/src/functions_JS.txt").to_string()
        };
        Self {
            function_list: res,
            ..self
        }
    }

}

impl Default for Header {
    fn default() -> Header {
        let std_function_list = include_str!("../configs/src/functions_R.txt").to_string();
        Header {
            function_list: std_function_list.clone(),
            generic_function_list: Self::get_generics(&std_function_list),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_is_generic(){
       let header = Header::default();
       assert!(!header.not_generic_yet("print".to_string()));
    }

    #[test]
    fn test_typr_is_not_generic(){
       let header = Header::default();
       assert!(header.not_generic_yet("typr".to_string()));
    }

}
