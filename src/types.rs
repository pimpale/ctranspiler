use std::collections::HashMap;

use lsp_types::Range;

use crate::{dlogger::DiagnosticLogger, hir::Environment};

#[derive(Clone, Debug, PartialEq)]
pub enum KindValue {
    Unknown,
    Int,
    Float,
    Bool,
    Type,
    Val,
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
            KindValue::Val => write!(f, "Val"),
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
            (KindValue::Val, KindValue::Val) => true,
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
            TypeValue::Never => write!(f, "Never"),
            TypeValue::SymbolicVariable(id) => {
                write!(f, "SymbolicVariable({})", id)
            }
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

pub fn print_typaram(typaram: &Option<TypeParam>) -> String {
    match typaram {
        Some(TypeParam { id, .. }) => format!("Some({})", id),
        None => "None".to_string(),
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
pub struct TypeParam {
    pub range: Range,
    pub id: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeValue {
    // An error when parsing
    Unknown,
    SymbolicVariable(usize),
    // types
    Never,
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
        typarams: Vec<Option<TypeParam>>,
        body: Box<TypeValue>,
    },
    // where the type constructor is symbolic
    Concretization {
        constructor: TypeValueConstructor,
        tyargs: Vec<TypeValue>,
    },
}

pub fn typevalue_kind(ty: &TypeValue, env: &mut Environment) -> KindValue {
    match ty {
        TypeValue::Unknown => KindValue::Unknown,
        TypeValue::Never => KindValue::Type,
        TypeValue::SymbolicVariable(id) => env.kind_table[*id].clone().unwrap(),
        TypeValue::Bool => KindValue::Type,
        TypeValue::RefConstructor => KindValue::Generic {
            paramkinds: vec![KindValue::Type],
            returnkind: Box::new(KindValue::Type),
        },
        TypeValue::ArrayConstructor => KindValue::Generic {
            paramkinds: vec![KindValue::Type, KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        TypeValue::SliceConstructor => KindValue::Generic {
            paramkinds: vec![KindValue::Type],
            returnkind: Box::new(KindValue::Type),
        },
        TypeValue::IntConstructor => KindValue::Generic {
            paramkinds: vec![KindValue::Bool, KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        TypeValue::FloatConstructor => KindValue::Generic {
            paramkinds: vec![KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        TypeValue::IntLit(_) => KindValue::Int,
        TypeValue::BoolLit(_) => KindValue::Bool,
        TypeValue::FloatLit(_) => KindValue::Float,
        TypeValue::Fn { .. } => KindValue::Val,
        TypeValue::Struct(_) => KindValue::Val,
        TypeValue::Enum(_) => KindValue::Val,
        TypeValue::Union(_) => KindValue::Val,
        TypeValue::Generic { typarams, body } => KindValue::Generic {
            paramkinds: typarams
                .iter()
                .map(|x| match x {
                    Some(TypeParam { id, .. }) => env.kind_table[*id].clone().unwrap(),
                    None => KindValue::Unknown,
                })
                .collect(),
            returnkind: Box::new(typevalue_kind(body, env)),
        },
        TypeValue::Concretization { constructor, .. } => match constructor {
            TypeValueConstructor::SymbolicVariable(id) => {
                match env.kind_table[*id].clone().unwrap() {
                    KindValue::Generic { returnkind, .. } => *returnkind,
                    _ => KindValue::Unknown,
                }
            }
            TypeValueConstructor::RefConstructor => KindValue::Type,
            TypeValueConstructor::ArrayConstructor => KindValue::Type,
            TypeValueConstructor::SliceConstructor => KindValue::Type,
            TypeValueConstructor::IntConstructor => KindValue::Type,
            TypeValueConstructor::FloatConstructor => KindValue::Type,
        },
    }
}

// gets the kind of a value that could be assignable to a variable with this type
pub fn get_kind_of_member(
    kind: KindValue,
    range: Range,
    dlogger: &mut DiagnosticLogger,
) -> KindValue {
    match kind {
        KindValue::Unknown => KindValue::Unknown,
        KindValue::Type => KindValue::Val,
        KindValue::Generic {
            paramkinds,
            returnkind,
        } => KindValue::Generic {
            paramkinds,
            returnkind: Box::new(get_kind_of_member(*returnkind, range, dlogger)),
        },
        _ => {
            dlogger.log_cannot_get_kind_of_member(range, &kind.to_string());
            KindValue::Unknown
        }
    }
}

// gets the kind of the type of this variable
pub fn get_kind_of_type(
    kind: KindValue,
    range: Range,
    dlogger: &mut DiagnosticLogger,
) -> KindValue {
    match kind {
        KindValue::Unknown => KindValue::Unknown,
        KindValue::Val => KindValue::Type,
        KindValue::Generic {
            paramkinds,
            returnkind,
        } => KindValue::Generic {
            paramkinds,
            returnkind: Box::new(get_kind_of_type(*returnkind, range, dlogger)),
        },
        _ => {
            dlogger.log_cannot_get_kind_of_type(range, &kind.to_string());
            KindValue::Unknown
        }
    }
}

pub fn kind_is_val(kind: &KindValue) -> Option<bool> {
    match kind {
        KindValue::Unknown => None,
        KindValue::Val => Some(true),
        KindValue::Generic { returnkind, .. } => kind_is_val(returnkind),
        _ => Some(false),
    }
}
