use std::collections::HashMap;

use crate::hir::Augmented;
use crate::{hir, typecheck};

#[derive(Clone, Debug, PartialEq)]
pub enum KindValue {
    Unknown,
    Int,
    Float,
    Bool,
    Type,
    Value,
    Generic {
        paramkinds: Vec<KindValue>,
        returnkind: Box<KindValue>,
    },
}

impl std::fmt::Display for KindValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            KindValue::Unknown => write!(f, "Unknown"),
            KindValue::Type => write!(f, "Type"),
            KindValue::Int => write!(f, "Int"),
            KindValue::Float => write!(f, "Float"),
            KindValue::Bool => write!(f, "Bool"),
            KindValue::Generic {
                paramkinds,
                returnkind,
            } => {
                write!(f, "Constructor {{ paramkinds: [")?;
                for arg in paramkinds.iter() {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, "], returnkind: {} }}", returnkind)
            }
        }
    }
}

impl KindValue {
    pub fn supports_assign(&self, other: &KindValue) -> bool {
        match (self, other) {
            (KindValue::Unknown, _) => true,
            (KindValue::Int, KindValue::Int) => true,
            (KindValue::Float, KindValue::Float) => true,
            (KindValue::Bool, KindValue::Bool) => true,
            (KindValue::Type, KindValue::Type) => true,
            (
                KindValue::Generic {
                    paramkinds,
                    returnkind,
                },
                KindValue::Generic {
                    paramkinds: other_paramkinds,
                    returnkind: other_returnkind,
                },
            ) => {
                if paramkinds.len() != other_paramkinds.len() {
                    return false;
                }
                for (paramkind, other_paramkind) in paramkinds.iter().zip(other_paramkinds.iter()) {
                    if !paramkind.supports_assign(other_paramkind) {
                        return false;
                    }
                }
                return returnkind.supports_assign(other_returnkind);
            }
            _ => false,
        }
    }
}


impl std::fmt::Display for TypeValueConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeValueConstructor::SymbolicVariable(id) => {
                write!(f, "SymbolicVariable({})", id)
            }
            TypeValueConstructor::RefConstructor => write!(f, "RefConstructor"),
            TypeValueConstructor::ArrayConstructor => write!(f, "ArrayConstructor"),
            TypeValueConstructor::SliceConstructor => write!(f, "SliceConstructor"),
            TypeValueConstructor::IntConstructor => write!(f, "IntConstructor"),
            TypeValueConstructor::FloatConstructor => write!(f, "FloatConstructor"),
        }
    }

}

impl std::fmt::Display for TypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeValue::Unknown => write!(f, "Unknown"),
            TypeValue::SymbolicVariable(id) => {
                write!(f, "SymbolicVariable({})", id)
            }
            TypeValue::Unit => write!(f, "Unit"),
            TypeValue::Bool => write!(f, "Bool"),
            TypeValue::RefConstructor => write!(f, "RefConstructor"),
            TypeValue::ArrayConstructor => write!(f, "ArrayConstructor"),
            TypeValue::SliceConstructor => write!(f, "SliceConstructor"),
            TypeValue::IntConstructor => write!(f, "IntConstructor"),
            TypeValue::FloatConstructor => write!(f, "FloatConstructor"),
            TypeValue::IntLit(n) => write!(f, "IntLit({})", n),
            TypeValue::BoolLit(b) => write!(f, "BoolLit({})", b),
            TypeValue::FloatLit(n) => write!(f, "FloatLit({})", n),
            TypeValue::Fn {
                paramtys,
                returntype,
            } => write!(
                f,
                "Fn {{ paramtys: [{}], returntype: {} }}",
                paramtys
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                returntype
            ),
            TypeValue::Struct(fields) => write!(
                f,
                "Struct {{ fields: [{}] }}",
                fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypeValue::Enum(fields) => write!(
                f,
                "Enum {{ fields: [{}] }}",
                fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypeValue::Union(fields) => write!(
                f,
                "Union {{ fields: [{}] }}",
                fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypeValue::Generic { typarams, body } => write!(
                f,
                "Generic {{ typarams: [{}], body: {} }}",
                typarams
                    .iter()
                    .map(|typaram| print_typaram(typaram))
                    .collect::<Vec<String>>()
                    .join(", "),
                (body)
            ),
            TypeValue::Concretization {
                constructor: symbolic_constructor,
                tyargs,
            } => write!(
                f,
                "Concretization {{ symbolic_constructor: {}, tyargs: [{}] }}",
                symbolic_constructor,
                tyargs
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

fn print_typaram(typaram: &Augmented<hir::TypePatExpr>) -> String {
    match &typaram.val {
        hir::TypePatExpr::Error => "Error".to_string(),
        hir::TypePatExpr::Identifier(id) => {
            format!("Identifier({})", id,)
        }
        hir::TypePatExpr::Typed { ref id, ref kind } => {
            format!(
                "Typed {{ id: {}, kind: {} }}",
                id,
                typecheck::evaluate_hir_kind(kind)
            )
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeValueConstructor {
    SymbolicVariable(usize),
    RefConstructor,
    ArrayConstructor,
    SliceConstructor,
    IntConstructor,
    FloatConstructor,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeValue {
    // An error when parsing
    Unknown,
    SymbolicVariable(usize),
    // types
    Unit,
    Bool,
    // constructors
    RefConstructor,
    ArrayConstructor,
    SliceConstructor,
    IntConstructor,
    FloatConstructor,
    // const literals
    IntLit(i64),
    BoolLit(bool),
    FloatLit(f64),
    // type of a function
    Fn {
        paramtys: Vec<TypeValue>,
        returntype: Box<TypeValue>,
    },
    // struct and enumify
    Struct(HashMap<String, TypeValue>),
    Enum(HashMap<String, TypeValue>),
    Union(HashMap<String, TypeValue>),
    // type of a generic value. Instantiated at every use point
    // type -> type
    Generic {
        typarams: Vec<Augmented<hir::TypePatExpr>>,
        body: Box<TypeValue>,
    },
    // where the type constructor is symbolic
    Concretization {
        constructor: TypeValueConstructor,
        tyargs: Vec<TypeValue>,
    },
}
