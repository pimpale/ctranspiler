use lsp_types::Range;

use crate::{ast, builtin::Builtin, values::Value};

pub struct Environment {
    // the identifier table (using absolute indices)
    pub id_name_table: Vec<Vec<String>>,
    pub id_range_table: Vec<Range>,
    pub id_modifier_table: Vec<ast::IdentifierModifier>,

    // the label table
    pub lb_name_table: Vec<String>,
    pub lb_range_table: Vec<Range>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            id_name_table: vec![],
            id_range_table: vec![],
            id_modifier_table: vec![],
            lb_name_table: vec![],
            lb_range_table: vec![],
        }
    }
}

pub struct ExecutionEnvironment {
    stack: Vec<Value>,
}

impl ExecutionEnvironment {
    pub fn new() -> ExecutionEnvironment {
        let env = ExecutionEnvironment { stack: vec![] };

        env
    }

    pub fn ty(&self, v: &Value) -> Value {
        match v {
            Value::SymbolicVariable { ty, .. } => *ty.clone(),
            Value::Lam { ty, .. } => *ty.clone(),
            w @ Value::PiTy { .. } => Value::Builtin(Builtin::Type, w.universe()),
            Value::Thunk { result_type, .. } => *result_type.clone(),
            Value::StructTy { level, .. } => Value::Builtin(Builtin::Type, *level),
            Value::EnumTy { level, .. } => Value::Builtin(Builtin::Type, *level),
            Value::UnionTy { level, .. } => Value::Builtin(Builtin::Type, *level),
            Value::Ref { ty, .. } => *ty.clone(),
            Value::Array { inner_ty, values } => Value::Thunk {
                result_type: Box::new(Value::Builtin(Builtin::Type, inner_ty.universe() - 1)),
                lam: Box::new(Value::Builtin(Builtin::Array, inner_ty.universe() - 1)),
                args: vec![*inner_ty.clone(), Value::nat64(values.len() as u64)],
            },
            Value::Slice { inner_ty, .. } => Value::Thunk {
                result_type: Box::new(Value::Builtin(Builtin::Type, inner_ty.universe() - 1)),
                lam: Box::new(Value::Builtin(Builtin::Slice, inner_ty.universe() - 1)),
                args: vec![*inner_ty.clone()],
            },
            Value::Nat { universe, bits, .. } => Value::nat_ty(*universe, *bits),
            Value::Int { universe, bits, .. } => Value::int_ty(*universe, *bits),
            Value::FnPtr { ty } => *ty.clone(),
            Value::Struct { ty, .. } => *ty.clone(),
            Value::Enum { ty, .. } => *ty.clone(),
            Value::Union { ty, .. } => *ty.clone(),
            Value::Builtin(builtin, level) => match builtin {
                Builtin::Ref => Value::PiTy {
                    param_tys: vec![Value::Builtin(Builtin::Type, *level)],
                    dep_ty: (),
                },
                Builtin::Array => Value::PiTy {
                    paramtys: vec![Value::Type { level: *level }, Value::nat_ty(*level, 64)],
                    returnty: Box::new(Value::Type { level: *level }),
                },
                Builtin::SliceTyConstructor(level) => Value::LamTy {
                    paramtys: vec![Value::Type { level: *level }],
                    returnty: Box::new(Value::Type { level: *level }),
                },
                Builtin::NatTyConstructor(level) => Value::LamTy {
                    paramtys: vec![Value::nat_ty(level + 1, 64)],
                    returnty: Box::new(Value::Type { level: *level }),
                },
                Builtin::IntTyConstructor(level) => Value::LamTy {
                    paramtys: vec![Value::nat_ty(*level, 64)],
                    returnty: Box::new(Value::Type { level: *level }),
                },
                Builtin::FloatTyConstructor(level) => Value::LamTy {
                    paramtys: vec![Value::nat_ty(*level, 64)],
                    returnty: Box::new(Value::Type { level: *level }),
                },
                Builtin::IntNegGen(universe) => Value::LamTy {
                    paramtys: vec![Value::nat_ty(universe + 1, 64)],
                    returnty: (),
                },
            },
        }
    }
}
