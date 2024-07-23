use std::collections::HashMap;

use lsp_types::Range;

use crate::{ast, builtin::Builtin, thir};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    SymbolicVariable {
        id: usize,
        ty: Box<Value>,
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
        length: usize,
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
    // Type of a function, also known as a Pi type
    // https://en.wikipedia.org/wiki/Dependent_type#%CE%A0_type
    PiTy {
        // types of the parameters
        param_tys: Vec<Value>,
        // function mapping the parameters to a result type
        dep_ty: Box<Closure>,
    },
    Lam {
        ty: Box<Value>,
        body: Box<Closure>,
    },
    // a pointer to a function
    FnPtr {
        ty: Box<Value>,
    },
    // a thunk
    Thunk {
        result_type: Box<Value>,
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
    // BUILTINS
    Builtin(Builtin, usize),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unsafe {
            match self {
                Value::SymbolicVariable { id, .. } => write!(f, "SymbolicVariable({})", id),
                Value::Ref { var, .. } => {
                    write!(f, "{}.& ({:#})", *var.clone(), var.clone() as usize)
                }
                Value::Array { values, .. } => {
                    write!(f, "Array[")?;
                    for value in values {
                        write!(f, "{}, ", value)?;
                    }
                    write!(f, "]")
                }
                Value::Slice {
                    inner_ty,
                    length,
                    values,
                } => {
                    write!(f, "Slice[{}; ", inner_ty)?;
                    for i in 0..*length {
                        write!(f, "{}, ", *values.add(i))?;
                    }
                    write!(f, "]")
                }
                Value::Nat { value, .. } => write!(f, "{}u", value),
                Value::Int { value, .. } => write!(f, "{}", value),
                Value::PiTy { param_tys, dep_ty } => {
                    write!(f, "Fn(")?;
                    for paramty in param_tys {
                        write!(f, " {}", paramty)?;
                    }
                    write!(f, " -> {})", dep_ty)
                }
                Value::Lam { ty, body } => todo!(),
                Value::FnPtr { ty } => {
                    write!(f, "{}.&", ty)
                }
                Value::Thunk { lam, args, .. } => {
                    write!(f, "{}(", lam)?;
                    for arg in args {
                        write!(f, " {}", arg)?;
                    }
                    write!(f, ")");
                    Ok(())
                }
                Value::StructTy { level, fields } => {
                    write!(f, "StructTy.{} {{", level)?;
                    for (name, ty) in fields {
                        write!(f, " {} : {},", name, ty)?;
                    }
                    write!(f, " }}");
                    Ok(())
                }
                Value::EnumTy { level, fields } => {
                    write!(f, "EnumTy.{} {{", level)?;
                    for (name, ty) in fields {
                        write!(f, " {} : {},", name, ty)?;
                    }
                    write!(f, " }}");
                    Ok(())
                }
                Value::UnionTy { level, fields } => {
                    write!(f, "UnionTy.{} {{", level)?;
                    for (name, ty) in fields {
                        write!(f, " {} : {},", name, ty)?;
                    }
                    write!(f, " }}");
                    Ok(())
                }
                Value::Struct { ty, fields } => {
                    write!(f, ".{{")?;
                    for (name, value) in fields {
                        write!(f, " {} : {},", name, value)?;
                    }
                    write!(f, " }}");
                    Ok(())
                }
                Value::Enum { ty, variant } => todo!(),
                Value::Union { ty, variant } => todo!(),
                Value::Builtin(builtin, level) => write!(f, "{}.{}", builtin, level),
            }
        }
    }
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
            result_type: Box::new(Value::Builtin(Builtin::Type, level)),
            lam: Box::new(Value::Builtin(Builtin::Nat, level)),
            args: vec![Value::Nat {
                universe: level + 1,
                bits: 64,
                value: bits,
            }],
        }
    }

    pub fn int_ty(level: usize, bits: u64) -> Value {
        Value::Thunk {
            result_type: Box::new(Value::Builtin(Builtin::Type, level)),
            lam: Box::new(Value::Builtin(Builtin::Int, level)),
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
            Value::Lam { ty, .. } => ty.universe() - 1,
            Value::PiTy {
                param_tys: param_ty,
                dep_ty,
            } => usize::max(
                param_ty.iter().map(Value::universe).max().unwrap(),
                dep_ty.universe(),
            ),
            Value::Thunk { result_type, .. } => result_type.universe() - 1,
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
            Value::Builtin(builtin, level) => match builtin {
                // constructors
                Builtin::Type => level + 2,
                Builtin::Ref => level + 1,
                Builtin::Array => level + 1,
                Builtin::Slice => level + 1,
                Builtin::Nat => level + 1,
                Builtin::Int => level + 1,
                Builtin::Float => level + 1,
                // traits
                Builtin::AddTrait => level + 1,
                Builtin::SubTrait => level + 1,
                Builtin::MulTrait => level + 1,
                Builtin::DivTrait => level + 1,
                Builtin::RemTrait => level + 1,
                Builtin::AddAssignTrait => level + 1,
                Builtin::SubAssignTrait => level + 1,
                Builtin::MulAssignTrait => level + 1,
                Builtin::DivAssignTrait => level + 1,
                Builtin::RemAssignTrait => level + 1,
                // all non-type builtins have the same universe as their level
                _ => *level,
            },
        }
    }
}

pub struct ExecutionEnvironment {
    // the outer vec corresponds to the id of the variable
    // the inner vec corresponds to the stack of values
    id_values: Vec<Vec<Value>>,
    id_name_table: Vec<Vec<String>>,
    id_range_table: Vec<Range>,
    id_modifier_table: Vec<ast::IdentifierModifier>,
}

impl ExecutionEnvironment {
    pub fn new(
        id_name_table: Vec<Vec<String>>,
        id_range_table: Vec<Range>,
        id_modifier_table: Vec<ast::IdentifierModifier>,
    ) -> ExecutionEnvironment {
        let env = ExecutionEnvironment {
            id_values: vec![Vec::new(); id_name_table.len()],
            id_name_table,
            id_range_table,
            id_modifier_table,
        };

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

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    unpack: Vec<thir::PatExpr>,
    body: thir::ValExpr,
}

impl Closure {
    pub fn universe(&self) -> usize {
        todo!()
    }

    pub fn apply(&self, args: Vec<Value>) -> Value {
        todo!()
    }
}

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<closure>")
    }
}