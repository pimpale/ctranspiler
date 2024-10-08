use std::collections::HashMap;

use apint::{Int, UInt};

use crate::{ast, builtin::Builtin, thir};

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    // first pop the captures onto the stack
    captures: Vec<Value>,
    // then push the parameters onto the stack
    // run the parameter patterns in order to bind the parameters to the values
    params: Vec<thir::PatExpr>,
    // finally run the body
    body: thir::ValExpr,
}

impl Closure {
    pub fn universe(&self) -> usize {
        todo!()
    }
}

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<closure>")
    }
}

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
        value: UInt,
    },
    Int {
        universe: usize,
        value: Int,
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
    Nominal {
        nom_id: usize,
        value: Box<Value>,
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
                Value::Nat { value, .. } => write!(
                    f,
                    "{}u",
                    value.clone().into_apint().as_string_with_radix(10)
                ),
                Value::Int { value, .. } => write!(f, "{}", value.clone().into_apint().as_string_with_radix(10)),
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
            value: UInt::from(value),
        }
    }

    pub fn nat_ty(level: usize, bits: u64) -> Value {
        Value::Thunk {
            result_type: Box::new(Value::Builtin(Builtin::Type, level)),
            lam: Box::new(Value::Builtin(Builtin::Nat, level)),
            args: vec![Value::Nat {
                universe: level + 1,
                value: UInt::from(bits),
            }],
        }
    }

    pub fn int_ty(level: usize, bits: u64) -> Value {
        Value::Thunk {
            result_type: Box::new(Value::Builtin(Builtin::Type, level)),
            lam: Box::new(Value::Builtin(Builtin::Int, level)),
            args: vec![Value::Nat {
                universe: level + 1,
                value: UInt::from(bits),
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
                Builtin::LtTrait => level + 1,
                Builtin::LteTrait => level + 1,
                Builtin::GtTrait => level + 1,
                Builtin::GteTrait => level + 1,
                Builtin::EqTrait => level + 1,
                Builtin::AddAssignTrait => level + 1,
                Builtin::SubAssignTrait => level + 1,
                Builtin::MulAssignTrait => level + 1,
                Builtin::DivAssignTrait => level + 1,
                Builtin::RemAssignTrait => level + 1,
                Builtin::IndexTrait => level + 1,
                // all non-type builtins have the same universe as their level
                _ => *level,
            },
        }
    }

    pub fn alpha_eq(&self, other: &Value) -> bool {
        todo!()
    }
}
