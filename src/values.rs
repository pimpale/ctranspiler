use std::collections::HashMap;

use lsp_types::Range;
use num_bigint::{BigInt, BigUint};

use crate::{dlogger::DiagnosticLogger, hir::Environment};

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Never => write!(f, "Never"),
            Value::SymbolicVariable(id) => {
                write!(f, "SymbolicVariable({})", id)
            }
            Value::Bool => write!(f, "Bool"),
            Value::RefTyConstructor => write!(f, "RefConstructor"),
            Value::ArrayTyConstructor => write!(f, "ArrayConstructor"),
            Value::SliceTyConstructor => write!(f, "SliceConstructor"),
            Value::IntTyConstructor => write!(f, "IntConstructor"),
            Value::FloatTyConstructor => write!(f, "FloatConstructor"),
            Value::IntLit(n) => write!(f, "IntLit({})", n),
            Value::BoolLit(b) => write!(f, "BoolLit({})", b),
            Value::FloatLit(n) => write!(f, "FloatLit({})", n),
            Value::Fn {
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
            Value::Struct(fields) => write!(
                f,
                "Struct {{ fields: [{}] }}",
                fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Value::Enum(fields) => write!(
                f,
                "Enum {{ fields: [{}] }}",
                fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Value::Union(fields) => write!(
                f,
                "Union {{ fields: [{}] }}",
                fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Value::Generic { typarams, body } => write!(
                f,
                "Generic {{ typarams: [{}], body: {} }}",
                typarams
                    .iter()
                    .map(|typaram| print_typaram(typaram))
                    .collect::<Vec<String>>()
                    .join(", "),
                (body)
            ),
            Value::Concretization {
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

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    SymbolicVariable {
        id: usize,
        ty: Box<Value>,
    },
    // Type
    Type {
        // universe is level + 2
        level: usize,
    },
    // constructors
    RefTyConstructor {
        // universe is level + 1
        level: usize,
    },
    ArrayTyConstructor {
        // universe is level + 1
        level: usize,
    },
    SliceTyConstructor {
        // universe is level + 1
        level: usize,
    },
    NatTyConstructor {
        // universe is level + 1
        level: usize,
    },
    IntTyConstructor {
        // universe is level + 1
        level: usize,
    },
    FloatTyConstructor {
        // universe is level + 1
        level: usize,
    },
    // values that inhabit a type
    Ref {
        ty: Box<Value>,
        var: *const Value,
    },
    Array {
        inner_ty: Box<Value>,
        values: Vec<Value>,
    },
    Slice {
        inner_ty: Box<Value>,
        values: *const Value,
    },
    Nat {
        universe: usize,
        bits: u64,
        value: u64,
    },
    Int {
        universe: usize,
        bits: u64,
        value: i64,
    },
    // a function value
    LamTy {
        level: usize,
        paramtys: Vec<Value>,
        returnty: Box<Value>,
    },
    Lam {
        ty: Box<Value>,
        body: Option<Box<Value>>,
    },
    // a pointer to a function
    FnPtr {
        ty: Box<Value>,
    },
    // a thunk
    Thunk {
        lam: Box<Value>,
        args: Vec<Value>,
    },
    // compound types
    StructTy {
        // universe is level + 1
        level: usize,
        fields: HashMap<String, Value>,
    },
    EnumTy {
        // universe is level + 1
        level: usize,
        fields: HashMap<String, Value>,
    },
    UnionTy {
        // universe is level + 1
        level: usize,
        fields: HashMap<String, Value>,
    },
    // values that inhabit a compound type
    Struct {
        ty: Box<Value>,
        fields: HashMap<String, Value>,
    },
    Enum {
        ty: Box<Value>,
        variant: Box<Value>,
    },
    Union {
        ty: Box<Value>,
        variant: Box<Value>,
    },
}

impl Value {
    pub fn nat64(value: u64) -> Value {
        Value::Nat {
            universe: 1,
            bits: 64,
            value,
        }
    }

    pub fn nat_ty(level: usize, bits: u64) -> Value {
        Value::Thunk {
            lam: Box::new(Value::NatTyConstructor { level }),
            args: vec![Value::Nat {
                universe: level + 1,
                bits: 64,
                value: bits,
            }],
        }
    }

    pub fn int_ty(level: usize, bits: u64) -> Value {
        Value::Thunk {
            lam: Box::new(Value::IntTyConstructor { level }),
            args: vec![Value::Nat {
                universe: level + 1,
                bits: 64,
                value: bits,
            }],
        }
    }

    pub fn universe(&self) -> usize {
        match self {
            Value::SymbolicVariable { ty, .. } => ty.universe() - 1,
            Value::Type { level } => level + 2,
            Value::RefTyConstructor { level } => level + 1,
            Value::ArrayTyConstructor { level } => level + 1,
            Value::SliceTyConstructor { level } => level + 1,
            Value::NatTyConstructor { level } => level + 1,
            Value::IntTyConstructor { level } => level + 1,
            Value::FloatTyConstructor { level } => level + 1,
            Value::Lam { ty, .. } => ty.universe() - 1,
            Value::LamTy { level, .. } => level + 1,
            Value::Thunk { lam, .. } => match lam.ty() {
                Value::LamTy { returnty, .. } => returnty.universe() - 1,
                _ => unreachable!("bad thunk"),
            },
            Value::StructTy { level, .. } => level + 1,
            Value::EnumTy { level, .. } => level + 1,
            Value::UnionTy { level, .. } => level + 1,
            Value::Ref { ty, .. } => ty.universe() - 1,
            Value::Array { inner_ty, .. } => inner_ty.universe() - 1,
            Value::Slice { inner_ty, .. } => inner_ty.universe() - 1,
            Value::Nat { universe, .. } => *universe,
            Value::Int { universe, .. } => *universe,
            Value::FnPtr { ty, .. } => ty.universe() - 1,
            Value::Struct { ty, .. } => ty.universe() - 1,
            Value::Enum { ty, .. } => ty.universe() - 1,
            Value::Union { ty, .. } => ty.universe() - 1,
        }
    }

    pub fn ty(&self) -> Value {
        match self {
            Value::SymbolicVariable { ty, .. } => *ty.clone(),
            Value::Type { level } => Value::Type { level: *level + 1 },
            Value::RefTyConstructor { level } => Value::LamTy {
                level: *level,
                paramtys: vec![Value::Type { level: *level }],
                returnty: Box::new(Value::Type { level: *level }),
            },
            Value::ArrayTyConstructor { level } => Value::LamTy {
                level: level + 1,
                paramtys: vec![Value::Type { level: *level }, Value::nat_ty(*level, 64)],
                returnty: Box::new(Value::Type { level: *level }),
            },
            Value::SliceTyConstructor { level } => Value::LamTy {
                level: level + 1,
                paramtys: vec![Value::Type { level: *level }],
                returnty: Box::new(Value::Type { level: *level }),
            },
            Value::NatTyConstructor { level } => Value::LamTy {
                level: level + 1,
                paramtys: vec![Value::nat_ty(level + 1, 64)],
                returnty: Box::new(Value::Type { level: *level }),
            },
            Value::IntTyConstructor { level } => Value::LamTy {
                level: level + 1,
                paramtys: vec![Value::nat_ty(*level, 64)],
                returnty: Box::new(Value::Type { level: *level }),
            },
            Value::FloatTyConstructor { level } => Value::LamTy {
                level: level + 1,
                paramtys: vec![Value::nat_ty(*level, 64)],
                returnty: Box::new(Value::Type { level: *level }),
            },
            Value::Lam { ty, .. } => *ty.clone(),
            Value::LamTy { level, .. } => Value::Type { level: *level },
            Value::Thunk { lam, .. } => match lam.ty() {
                Value::LamTy { returnty, .. } => *returnty.clone(),
                _ => unreachable!("bad thunk"),
            },
            Value::StructTy { level, .. } => Value::Type { level: *level },
            Value::EnumTy { level, .. } => Value::Type { level: *level },
            Value::UnionTy { level, .. } => Value::Type { level: *level },
            Value::Ref { ty, .. } => *ty.clone(),
            Value::Array { inner_ty, values } => Value::Thunk {
                lam: Box::new(Value::ArrayTyConstructor {
                    level: inner_ty.universe(),
                }),
                args: vec![*inner_ty.clone(), Value::nat64(values.len() as u64)],
            },
            Value::Slice { inner_ty, .. } => Value::Thunk {
                lam: Box::new(Value::SliceTyConstructor {
                    level: inner_ty.universe() - 1,
                }),
                args: vec![*inner_ty.clone()],
            },
            Value::Nat { universe, bits, .. } => Value::nat_ty(*universe, *bits),
            Value::Int { universe, .. } => Value::int_ty(*universe, 64),
            Value::FnPtr { ty } => *ty.clone(),
            Value::Struct { ty, .. } => *ty.clone(),
            Value::Enum { ty, .. } => *ty.clone(),
            Value::Union { ty, .. } => *ty.clone(),
        }
    }
}

pub fn typevalue_kind(ty: &Value, env: &mut Environment) -> KindValue {
    match ty {
        Value::Never => KindValue::Type,
        Value::SymbolicVariable(id) => env.id_kind_table[*id].clone(),
        Value::Bool => KindValue::Type,
        Value::RefTyConstructor => KindValue::Generic {
            paramkinds: vec![KindValue::Type],
            returnkind: Box::new(KindValue::Type),
        },
        Value::ArrayTyConstructor => KindValue::Generic {
            paramkinds: vec![KindValue::Type, KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        Value::SliceTyConstructor => KindValue::Generic {
            paramkinds: vec![KindValue::Type],
            returnkind: Box::new(KindValue::Type),
        },
        Value::IntTyConstructor => KindValue::Generic {
            paramkinds: vec![KindValue::Bool, KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        Value::FloatTyConstructor => KindValue::Generic {
            paramkinds: vec![KindValue::Int],
            returnkind: Box::new(KindValue::Type),
        },
        Value::IntLit(_) => KindValue::Int,
        Value::BoolLit(_) => KindValue::Bool,
        Value::FloatLit(_) => KindValue::Float,
        Value::Fn { .. } => KindValue::Val,
        Value::Struct(_) => KindValue::Val,
        Value::Enum(_) => KindValue::Val,
        Value::Union(_) => KindValue::Val,
        Value::Generic { typarams, body } => KindValue::Generic {
            paramkinds: typarams
                .iter()
                .map(|x| match x {
                    Some(TypeParam { id, .. }) => env.id_kind_table[*id].clone(),
                    None => KindValue::Unknown,
                })
                .collect(),
            returnkind: Box::new(typevalue_kind(body, env)),
        },
        Value::Concretization { constructor, .. } => match constructor {
            TypeValueConstructor::SymbolicVariable(id) => match env.id_kind_table[*id].clone() {
                KindValue::Generic { returnkind, .. } => *returnkind,
                _ => KindValue::Unknown,
            },
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
