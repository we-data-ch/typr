use crate::components::error_message::help_message::SingleBuilder;
use crate::components::error_message::help_message::DoubleBuilder;
use crate::components::error_message::help_message::ErrorMsg;
use crate::components::error_message::help_data::HelpData;
use crate::components::r#type::type_system::TypeSystem;
use crate::components::language::var::Var;
use crate::components::language::Lang;
use crate::components::r#type::Type;
use miette::Result;
use std::fs;

pub enum TypeError {
    Let(Type, Type),
    Param(Type, Type),
    UndefinedFunction(Var),
    UndefinedVariable(Lang),
    UnmatchingReturnType(Type, Type),
    ImmutableVariable(Var, Var),
    PrivateVariable(Var, Var),
    GenericPatternMatch(Type, Type),
    FieldNotFound((String, HelpData), Type),
    WrongExpression(HelpData),
    WrongIndexing(Type, Type),
}

// main
impl ErrorMsg for TypeError {
    fn display(self) -> String {
        let msg: Result<()> = match self {
            TypeError::Let(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let (file_name, text) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), 
                                fs::read_to_string("std.ty").unwrap_or("".to_string())));
                DoubleBuilder::new(file_name.clone(), text.clone(), file_name, text)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("type {} doesn't match type {}", t1.pretty(), t2.pretty()))
                    .pos_text1(format!("Expected {}", t1.pretty()))
                    .pos_text2(format!("Recieved {}", t2.pretty()))
                    .build()
            },
            TypeError::Param(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), fs::read_to_string("std.ty").unwrap()));
                let (file_name2, text2) = help_data2.get_file_data()
                    .unwrap_or(("std.ty".to_string(), fs::read_to_string("std.ty").unwrap()));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("type {} doesn't match type {}", t1.pretty(), t2.pretty()))
                    .pos_text1(format!("Expected {}", t1.pretty()))
                    .pos_text2(format!("Recieved {}", t2.pretty()))
                    .build()
            },
            TypeError::GenericPatternMatch(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), "".to_string()));
                let (file_name2, text2) = help_data2.get_file_data()
                    .unwrap_or(("std.ty".to_string(), "".to_string()));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("can't pattern match {{{} => {}}}", t1.pretty2(), t2.pretty2()))
                    .pos_text1(format!("{} should be a generic variable", t1.pretty2()))
                    .pos_text2(format!("Substitution type: {}", t2.pretty2()))
                    .build()
            },
            TypeError::UnmatchingReturnType(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), fs::read_to_string("std.ty").unwrap()));
                let (file_name2, text2) = help_data2.get_file_data()
                    .unwrap_or(("std.ty".to_string(), fs::read_to_string("std.ty").unwrap()));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("The output type of the function don't match it's type annotation\nExpected: {:?}\nFound: {}", t1.pretty(), t2.pretty()))
                    .pos_text1(format!("Expected {}", t1.pretty()))
                    .pos_text2(format!("Recieved {}", t2.pretty()))
                    .build()
            },
            TypeError::UndefinedFunction(fun)
                => {
                    let help_data = fun.get_help_data();
                    let (file_name, text) = help_data.get_file_data()
                        .unwrap_or(("std.ty".to_string(), "".to_string()));
                    let res = SingleBuilder::new(file_name, text)
                        .pos((help_data.get_offset(), 0))
                        .text("The function doesn't exist");

                    res.build()
                },
            TypeError::UndefinedVariable(var)
                => {
                    let help_data = var.get_help_data();
                    let var2 = Var::from_language(var).unwrap();
                    let (file_name, text) = help_data.get_file_data()
                    .unwrap_or(("std.ty".to_string(), "".to_string()));
                    SingleBuilder::new(file_name, text)
                        .pos((help_data.get_offset(), 0))
                        .pos_text(format!("Undefined variable '{}'", var2.get_name()))
                        .text(format!("Undefined variable '{}'", var2.get_name()))
                        .help(format!("- Check the orthograph \n- if it's a function check if it's defined for the given type {}", var2.get_type()))
                        .build()
                }
            TypeError::ImmutableVariable(var_assign, var)
                => {
                let var = var.clone().set_type(var.get_type().generalize());
                let help_data1 = var_assign.get_help_data();
                let help_data2 = var.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .expect(&format!("The file name of {:?} for {} doesn't exist", 
                                     help_data1, var_assign));
                let (file_name2, text2) = help_data2.get_file_data()
                    .expect(&format!("The file name of {:?} for {} doesn't exist", 
                                     help_data2, var));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("The variable {} is immutable", var))
                    .pos_text1(format!("Forbidden assignation to {}", var))
                    .pos_text2(format!("{} defined with 'let' (=immutable) here", var))
                    .help("Try to replace the 'let' keyword by the 'mut' keyword")
                    .build()
                }
            TypeError::PrivateVariable(var_used, var)
                => {
                let var = var.clone().set_type(var.get_type().generalize());
                let help_data1 = var_used.get_help_data();
                let help_data2 = var.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .expect(&format!("The file name of {:?} for {} doesn't exist", 
                                     help_data1, var_used));
                let (file_name2, text2) = help_data2.get_file_data()
                    .expect(&format!("The file name of {:?} for {} doesn't exist", 
                                     help_data2, var));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("The variable {} is private", var))
                    .pos_text1(format!("Forbidden access to {} which is private", var))
                    .pos_text2(format!("{} defined as private (without 'pub') here", var))
                    .help("Try to add the 'pub' keyword befor the 'let' keyword")
                    .build()
                }
            TypeError::FieldNotFound((name, h), typ) => {
                let help_data1 = h;
                let help_data2 = typ.get_help_data();
                let (file_name, text) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), 
                                fs::read_to_string("std.ty").unwrap_or("".to_string())));
                DoubleBuilder::new(file_name.clone(), text.clone(), file_name, text)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("{} doesn't have a field '{}'", typ.pretty(), name))
                    .pos_text1(format!("Field '{}' doesn't exist", name))
                    .pos_text2(format!("Type {} defined here", typ.pretty()))
                    .build()
            },
            TypeError::WrongExpression(help_data) => {
                let (file_name, text) = help_data.get_file_data()
                    .unwrap_or(("std.ty".to_string(), 
                                fs::read_to_string("std.ty").unwrap_or("".to_string())));
                SingleBuilder::new(file_name, text)
                    .pos((help_data.get_offset(), 0))
                    .build()
            }
            TypeError::WrongIndexing(t1, t2) => {
                let help_data1 = t1.get_help_data();
                let help_data2 = t2.get_help_data();
                let (file_name1, text1) = help_data1.get_file_data()
                    .unwrap_or(("std.ty".to_string(), "".to_string()));
                let (file_name2, text2) = help_data2.get_file_data()
                    .unwrap_or(("std.ty".to_string(), "".to_string()));
                DoubleBuilder::new(file_name1, text1, file_name2, text2)
                    .pos1((help_data1.get_offset(), 0))
                    .pos2((help_data2.get_offset(), 1))
                    .text(format!("Wrong indexing"))
                    .pos_text1(format!("{} Can't be indexed", t1.pretty2()))
                    .pos_text2(format!("Has a bigger dimension {}", t2.pretty2()))
                    .build()
            },
                
        };
        format!("{:?}", msg)
    }

}
