#![allow(dead_code, unused_variables, unused_imports, unreachable_code, unused_assignments)]
use crate::VarType;
//use std::path::Path;

pub enum Source {
    PackageName,
    NameList,
    Header
}

// Manage the loading and saving of packages
pub struct PackageManager {
   kind: Source,
   content: String,
   target_path: String
}

impl PackageManager {
    fn save(&self, name: &str) -> Self {
        todo!();
    }

    fn set_target_path(self, path: &str) -> Self {
        todo!();
    }

    fn set_content(self, content: &str) -> Self {
        todo!();
    }

    fn set_name(self, name: &str) -> Self {
        todo!();
    }

    fn set_kind(self, name: &str) -> Self {
        todo!();
    }

    fn load(&self) -> Result<VarType, String> {
        todo!();
    }

    fn load_from_name(name: &str) -> Result<VarType, String> {
        todo!();
    }

    fn load_with_path(name: &str, path: &str) -> Result<VarType, String> {
        todo!();
    }

    fn to_name_list(content: &str) -> Result<PackageManager, String> {
        todo!();
    }

    fn to_header(context: &str) -> Result<PackageManager, String> {
        todo!();
    }

    fn to_package(context: &str) -> Result<PackageManager, String> {
        todo!();
    }

    fn remove(context: &str) {
        todo!();
    }

    fn does_name_exists(name: &str, path: &str) -> bool {
        todo!();
    }

    fn exists(&self) -> bool {
        todo!();
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
        let var_type = PackageManager::load_with_path("std_r", "configs/bin");
        assert!(var_type.is_ok(), "The path configs/bin should exist");
    }

    #[test]
    fn test_saving_name_list() {
        let var_type = PackageManager::to_name_list("name1\nname2\nname3")
            .unwrap().save("name_list");
        assert!(var_type.exists(), "The names should exist as .name_list.bin");
    }

    #[test]
    fn test_saving_typr_code() {
        let var_type = PackageManager::to_header("@a: int")
            .unwrap().save("header");
        assert!(var_type.exists(), "The header should exist as .header.bin");
    }

    #[test]
    fn test_saving_package() {
        let var_type = PackageManager::to_package("dplyr")
            .unwrap().save("package");
        assert!(var_type.exists(), "The header should exist as .package.bin");
    }

    #[test]
    fn test_remove_bin_files() {
        PackageManager::remove("name_list");
        PackageManager::remove("header");
        PackageManager::remove("package");
        assert!(!PackageManager::does_name_exists("name_list", "."), 
                "The file .name_list.bin shouldn't exist");
    }

}
