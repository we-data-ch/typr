use rpds::Vector;
use crate::Type;
use std::collections::HashSet;
use crate::help_data::HelpData;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum TypeOperator {
    Union,
    Intersection,
    #[default]
    Unknown,
}

impl TypeOperator {
    fn build_type(self, types: HashSet<Type>, help_data: HelpData) -> Type {
        match self {
            TypeOperator::Union => Type::Union(types, help_data),
            TypeOperator::Intersection => Type::Intersection(types, help_data),
            _ => panic!("We can't combine types with the empty type operator")
        }
    }
}

#[derive(Debug, Default)]
pub struct TypeStack {
    types: Vector<Type>,
    last_operator: TypeOperator
}

impl TypeStack {
    fn push_type(self, typ: Type) -> Self {
        Self {
            types: self.types.push_back(typ),
            ..self
        }
    }

    fn push_operator(self, op: TypeOperator) -> Self {
        Self {
            last_operator: op,
            ..self
        }
    }

    fn differ_from_last_operator(&self, op: TypeOperator) -> bool {
        op != self.last_operator && self.last_operator != TypeOperator::Unknown
    }

    fn push_op_helper(self, op: TypeOperator) -> Self {
        if op == TypeOperator::Unknown {
            self.compute()
        } else if self.differ_from_last_operator(op) {
            self.compute().push_operator(op)
        } else {
            self.push_operator(op)
        }
    }

    fn push_op(self, op: Option<TypeOperator>) -> Self {
        match op {
            Some(ope) => self.push_op_helper(ope),
            _ => self // maybe add a .compute()
        }
    }


    pub fn get_type(self) -> Type {
        self.types.last()
            .expect("This is an empty stack!")
            .clone()
    }

    fn compute(self) -> Self {
        let new_types = self.types.iter()
            .cloned()
            .collect::<HashSet<_>>();
        let help_data: HelpData = self.types[0].clone().into();
        let new_type = self.last_operator.build_type(new_types, help_data);
        Self {
            types: Vector::default().push_back(new_type),
            ..self
        }
    }

    pub fn load(self, v: &[(Option<TypeOperator>, Type)]) -> Self {
        v.iter()
         .fold(self, 
              |acc, (op, typ)| 
                acc.push_type(typ.clone()).push_op(op.clone()))
    }

    #[cfg(test)]
    fn get_last_operator(&self) -> TypeOperator {
        self.last_operator.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder;
    use crate::builder::integer_type_default;
    use crate::builder::character_type_default;
    use crate::type_category::TypeCategory;

    #[test]
    fn test_type_operator_default_value(){
        assert_eq!(TypeOperator::default(), TypeOperator::Unknown,
        "The default value of the TypeOperator should be 'Unknown'"); 
    }

    #[test]
    #[should_panic]
    fn test_empty_stack_get_type() {
        TypeStack::default().get_type();
    }

    #[test]
    fn test_empty_stack_get_first_type() {
        let typ = builder::empty_type();
        let stack = TypeStack::default().push_type(typ.clone());
        assert_eq!(stack.get_type(), typ,
        "The TypeStack should keep the given type in its 'types' field");
    }

    #[test]
    fn test_stack_get_last_operator1() {
        let stack = TypeStack::default();
        assert_eq!(stack.get_last_operator(), TypeOperator::Unknown,
        "The result should be 'Unknown' if no operator has not been added yet");
    }

    #[test]
    fn test_stack_get_last_operator2() {
        let stack = TypeStack::default().push_op(Some(TypeOperator::Union));
        assert_eq!(stack.get_last_operator(), TypeOperator::Union,
        "The result should be 'Union' since we added an union operator");
    }

    #[test]
    fn test_stack_push_operator_to_compute1() {
        let stack = TypeStack::default()
            .push_type(integer_type_default())
            .push_type(character_type_default())
            .push_op(Some(TypeOperator::Union))
            .push_op(Some(TypeOperator::Unknown));

        assert_eq!(stack.get_type().to_category(), TypeCategory::Union,
        "The result should be an Union type if the last operator added is None");
    }

    #[test]
    fn test_stack_push_operator_to_compute2() {
        let stack = TypeStack::default()
            .push_type(integer_type_default())
            .push_op(Some(TypeOperator::Intersection))
            .push_type(character_type_default())
            .push_op(Some(TypeOperator::Intersection))
            .push_type(character_type_default())
            .push_op(Some(TypeOperator::Union));
        assert_eq!(stack.get_type().to_category(), TypeCategory::Intersection,
        "The result should be an Intersection if the last operator added differ from the previous ones.");
    }

    #[test]
    fn test_stack_push_operator_to_compute3() {
        let integer = integer_type_default();
        let character = character_type_default();
        let stack = TypeStack::default()
            .push_type(integer.clone())
            .push_op(Some(TypeOperator::Intersection))
            .push_type(character.clone())
            .push_op(Some(TypeOperator::Union))
            .push_type(integer.clone())
            .push_op(Some(TypeOperator::Intersection));
        let intersection = builder::intersection_type(&[integer.clone(), character]);
        let union = builder::union_type(&[intersection, integer]);
        assert_eq!(stack.get_type(), union,
        "The result should be an Union of an intersection");
    }

}
