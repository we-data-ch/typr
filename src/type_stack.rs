use rpds::Stack;
use crate::Type;
use std::collections::HashSet;
use crate::graph::TypeSystem;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum TypeOperator {
    Union,
    Intersection,
    #[default]
    Unknown,
}

impl TypeOperator {
    fn join_helper(self, type1: Type, type2: Type) -> Type {
        let types = [type1.clone(), type2.clone()].iter().cloned().collect::<HashSet<_>>();
        match self {
            TypeOperator::Union => Type::Union(types, type1.into()),
            TypeOperator::Intersection => Type::Intersection(types, type1.into()),
            _ => panic!("We can't combine {} and {} with the empty type operator",
                                type1.pretty(), type2.pretty())
        }
    }

    fn join(self, type1: Option<&Type>, type2: Option<&Type>) -> Option<Type> {
        match (type1, type2) {
            (Some(t1), Some(t2)) 
                => Some(self.join_helper(t1.clone(), t2.clone())),
            _ => None
        }
    }
}

#[derive(Debug, Default)]
pub struct TypeStack {
    types: Stack<Type>,
    operators: Stack<TypeOperator>,
    last_operator: TypeOperator
}

impl TypeStack {
    fn push_type(self, typ: Type) -> Self {
        Self {
            types: self.types.push(typ),
            ..self
        }
    }

    fn push_operator(self, op: TypeOperator) -> Self {
        Self {
            operators: self.operators.push(op),
            last_operator: op,
            ..self
        }
    }

    fn push_op_helper(self, op: TypeOperator) -> Self {
        if op == TypeOperator::Unknown {
            self.compute()
        } else if op != self.last_operator && self.last_operator != TypeOperator::Unknown {
            self.compute().push_operator(op)
        } else {
            self.push_operator(op)
        }
    }

    fn push_op(self, op: Option<TypeOperator>) -> Self {
        match op {
            Some(ope) => self.push_op_helper(ope),
            _ => self
        }
    }


    pub fn get_type(self) -> Type {
        match self.types.peek() {
            Some(typ) => typ.clone(),
            _ => panic!("This is an empty stack!")
        } 
    }

    fn compute(self) -> Self {
        let mut stack = self;
        while stack.operators.peek().is_some() {
            stack = match stack.operators.peek() {
                None => stack,
                Some(res) => {
                    let type1 = stack.types.peek(); let types = stack.types.pop().unwrap();
                    let type2 = types.peek(); let types = types.pop().unwrap();
                    let new_type = res.join(type1, type2)
                       .expect("The type operator Stack wasn't able to build the type");
                    Self {
                        types: types.push(new_type),
                        ..stack
                    }
                }
            }
        }
        stack
    }

    pub fn load(self, v: &[(Type, Option<TypeOperator>)]) -> Self {
        v.iter()
         .fold(self, 
              |acc, (typ, op)| 
                acc.push_type(typ.clone()).push_op(op.clone()))
    }
}
