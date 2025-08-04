use std::fmt;
use serde::Serialize;
use crate::language::Lang;
use crate::Context;
use std::path::PathBuf;
use std::fs::File;
use std::io::Write;

#[derive(Debug, Serialize, Clone)]
pub struct Adt(pub Vec<Lang>);

impl fmt::Display for Adt {
    fn fmt(self: &Self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let cont = Context::new(vec![], vec![]);
        let res = self.0.iter().map(|x| x.to_r(&cont).0)
            .reduce(|acc, x| format!("{}, {}", acc, x))
            .unwrap_or("".to_string());
        write!(f, "sequence([{}])", res)       
    }
}

impl From<Vec<Lang>> for Adt {
   fn from(val: Vec<Lang>) -> Self {
        Adt(val)
   } 
}

impl Adt {
    pub fn iter(&self) -> std::slice::Iter<'_, Lang> {
        self.0.iter()
    }

    pub fn to_r(&self, cont: &Context) -> String {
        //let mut current_cont = cont.update_classes();
        let current_cont = cont.clone();
        let mut results = Vec::new();
        
        for exp in self.iter() {
            let (exp_str, _new_cont) = exp.to_r(&current_cont);
            results.push(exp_str);
        }
        let res = results.join("\n");
        if res == "" {
            res
        } else {
            results.join("\n") + "\n"
        }
    }

    pub fn to_typescript(&self, cont: &Context) -> String {
        let mut current_cont = cont.clone();
        let mut results = Vec::new();
        
        for exp in self.iter() {
            let (exp_str, new_cont) = exp.to_typescript(&current_cont);
            results.push(exp_str);
            current_cont = new_cont;
        }
        results.join("\n")
    }

    pub fn to_assemblyscript(&self, cont: &Context) -> String {
        let mut current_cont = cont.clone();
        let mut results = Vec::new();
        
        for exp in self.iter() {
            let (exp_str, new_cont) = exp.to_assemblyscript(&current_cont);
            results.push(exp_str);
            current_cont = new_cont;
        }
        results.join("\n")
    }

    pub fn add(self, adt: Adt) -> Adt {
        Adt(self.0.iter().chain(adt.0.iter()).cloned().collect::<Vec<_>>())
    }

    pub fn write_to_r(&self, cont: &Context, output_dir: &PathBuf, file_name: &str) -> () {
        let rstd = include_str!("../configs/r/std.R");
        let std_path = output_dir.join("std.R");
        let mut rstd_file = File::create(std_path).unwrap();
        rstd_file.write_all(rstd.as_bytes()).unwrap();

        let app_path = output_dir.join(file_name);
        let mut app = File::create(app_path).unwrap();
        let content = format!("source('std.R', echo = FALSE)\n\n{}{}", cont.adt.get_adt().to_r(cont), self.to_r(cont));
        app.write_all(content.as_bytes()).unwrap();
    }

}


