use core::panic;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Void,
    Bool,
    Int,
    Float,
    Nil,
    String,
    Function(String, Vec<Type>, Box<Type>), //functiion signature
    // TypeVar(u32),
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },
    Enum {
        name: String,
        variants: Vec<(String, Vec<Type>)>,
    },
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        return self.is_float() || self.is_int();
    }

    pub fn is_float(&self) -> bool {
        return matches!(self, Type::Float);
    }

    pub fn is_int(&self) -> bool {
        return matches!(self, Type::Int);
    }
    pub fn check_numeric(&self, other: &Type) -> Type {
        match (self, other) {
            (Type::Float, Type::Float) => Type::Float,
            (Type::Float, Type::Int) => Type::Float,
            (Type::Int, Type::Float) => Type::Float,
            (Type::Int, Type::Int) => Type::Int,
            _ => {
                panic!("unsupported typechecking")
            }
        }
    }
}

pub fn pretty_print_type(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Float => "float".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Void => "void".to_string(),
        Type::Nil => todo!(),
        Type::String => todo!(),
        Type::Function(_, items, _) => todo!(),
        Type::Struct { name, fields } => todo!(),
        Type::Enum { name, variants } => todo!(),
    }
}
