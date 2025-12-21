#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::execute_r_function;
use crate::my_io::get_os_file;
use crate::engine::TypRFile;
use crate::TypeChecker;
use std::path::PathBuf;
use crate::read_file;
use std::path::Path;
use crate::VarType;
use crate::builder;
use crate::Context;
use crate::Var;
use crate::fs;

#[derive(Debug, Default, Clone)]
pub enum Source {
    PackageName,
    #[default]
    NameList,
    Header
}

// Manage the loading and saving of packages
#[derive(Debug, Clone)]
pub struct PackageManager {
   name: String,
   kind: Source,
   content: String,
   target_path: String
}

impl PackageManager {
    pub fn save(self) -> Self {
        match self.kind {
            Source::NameList => {
                let unknown_function = builder::unknown_function();
                let res = self.content.lines()
                    .map(|line| (Var::from_name(line), unknown_function.clone()))
                    .collect::<Vec<_>>();
                let _ = VarType::from(res).save(&self.get_bin_name());
            },
            Source::Header => {
                let file_content = read_file(&PathBuf::from(self.content.clone()));
                let base_file = TypRFile::new(&file_content, self.content.clone());
                let lang = base_file.parse();
                let _ = TypeChecker::new(Context::default())
                    .typing(&lang)
                    .get_context()
                    .get_vartype()
                    .save(&self.get_bin_name());
            }
            Source::PackageName => {
                let function_list = execute_r_function(&format!("library({})\n\npaste(ls('package:{}', all = FALSE), collapse =';')", self.content, self.content))
                    .expect("The R command didn't work");
                //Remove extra character at the beginning and at the end
                let function_list = function_list[..(function_list.len()-1)][5..]
                    .to_string().replace("<-", "");
                let unknown_function = builder::unknown_function();
                let var_types = function_list.split(";")
                    .map(|name| (Var::from_name(name).set_importability(true), unknown_function.clone()))
                    .collect::<Vec<_>>();
                let _ = VarType::from(var_types).save(&self.get_bin_name());
            }
        }
        self
    }

    pub fn set_target_path(self, path: &str) -> Self {
        Self {
            target_path: path.to_string(),
            ..self
        }
    }

    pub fn set_content(self, content: &str) -> Self {
        Self {
            content: content.to_string(),
            ..self
        }
    }

    pub fn set_name(self, name: &str) -> Self {
        Self {
            name: name.to_string(),
            ..self
        }
    }

    pub fn load(&self) -> Result<VarType, String> {
        Self::load_with_path(&self.name, &self.target_path)
    }

    fn get_bin_name(&self) -> String {
        Self::to_bin_name(&self.name, &self.target_path)
    }

    fn to_bin_name(name: &str, path: &str) -> String {
       path.to_string() + "." + name + ".bin"
    }

    fn load_from_name(name: &str) -> Result<VarType, String> {
        Self::load_with_path(name, "./")
    }

    fn load_with_path(name: &str, path: &str) -> Result<VarType, String> {
        let var_type = VarType::new();
        let full_name = Self::to_bin_name(name, path);
        let res = var_type.load(&full_name);
        match res {
            Ok(var_type) => Ok(var_type),
            _ => Err(format!("File {} not found", full_name))
        }
    }

    pub fn to_name_list(content: &str) -> Result<PackageManager, String> {
        let package_manager = PackageManager {
            content: content.to_string(),
            kind: Source::NameList,
            ..PackageManager::default()
        };
        Ok(package_manager)
    }

    pub fn to_header(content: &str) -> Result<PackageManager, String> {
        let package_manager = PackageManager {
            content: content.to_string(),
            kind: Source::Header,
            ..PackageManager::default()
        };
        Ok(package_manager)
    }

    pub fn to_package(content: &str) -> Result<PackageManager, String> {
        let package_manager = PackageManager {
            content: content.to_string(),
            name: content.to_string(),
            kind: Source::PackageName,
            ..PackageManager::default()
        };
        Ok(package_manager)
    }

    pub fn remove(&self) {
        let _ = fs::remove_file(&self.get_bin_name());
    }

    fn remove_from_path(name: &str, path: &str) {
        let _ = fs::remove_file(Self::to_bin_name(name, "./"));
    }

    fn remove_from_name(name: &str) {
        let _ = fs::remove_file(Self::to_bin_name(name, "./"));
    }

    fn does_name_exists(name: &str) -> bool {
        Path::new(&Self::to_bin_name(name, "./")).exists()
    }

    fn does_name_exists_with_path(name: &str, path: &str) -> bool {
        Path::new(&Self::to_bin_name(name, path)).exists()
    }

    pub fn exists(&self) -> bool {
        Path::new(&self.get_bin_name()).exists()
    }

}

impl Default for PackageManager {
    fn default() -> Self {
        PackageManager {
            name: String::default(),
            kind: Source::default(),
            content: String::default(),
            target_path: "./".to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // We should be able to access a VarType by giving a binary name
    // We should be able to save different kind of source and get a binary file

    #[test]
    #[should_panic]
    fn test_loading_unexisting_file(){
       let var_type = PackageManager::load_from_name("test").unwrap();
    }

    #[test]
    fn test_loading_existing_file() {
        let var_type = PackageManager::load_with_path("std_r", "configs/bin/");
        assert!(var_type.is_ok(), "The path configs/bin should exist");
    }

    #[test]
    fn test_saving_name_list() {
        let var_type = PackageManager::to_name_list("name1\nname2\nname3")
            .unwrap().set_name("name_list").save();
        assert!(var_type.exists(), "The names should exist as .name_list.bin");
    }

    #[test]
    fn test_saving_typr_code() {
        let var_type = PackageManager::to_header("configs/std/test.ty")
            .unwrap().set_name("header").save();
        assert!(var_type.exists(), "The header should exist as .header.bin");
    }

    #[test]
    fn test_saving_package() {
        let var_type = PackageManager::to_package("dplyr")
            .unwrap().set_name("package").save();
        assert!(var_type.exists(), "The header should exist as .package.bin");
    }

    #[test]
    fn test_remove_bin_files() {
        PackageManager::remove_from_name("name_list");
        PackageManager::remove_from_name("header");
        PackageManager::remove_from_name("package");
        assert!(!PackageManager::does_name_exists("name_list"), 
                "The file .name_list.bin shouldn't exist");
    }

}
