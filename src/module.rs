use crate::language::Lang;

pub struct Module(pub String, pub Vec<Lang>);

impl Module {
    pub fn get_body(&self) -> Vec<Lang> {
        self.1.clone()
    }

    pub fn from_language(l: Lang) -> Option<Module> {
        match l {
            Lang::Module(name, body) 
                => Some(Module(name, body)),
            _ => None
        }
    }
}
