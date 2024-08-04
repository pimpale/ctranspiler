use lsp_types::Range;

use crate::{
    ast,
    builtin::Builtin,
    thir,
    values::{Closure, Value},
};

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
            Value::Builtin(builtin, level) => todo!(),
            Value::Nominal { value, .. } => self.ty(value),
        }
    }

    // tries to evaluate a thir pattern, returning a vector on success
    pub fn try_eval_thir_pat(&mut self, pat: &thir::PatExpr, val: Value) -> bool {
        match pat {
            thir::PatExpr::Error => unreachable!("erroneous code cannot be evaluated"),
            thir::PatExpr::Ignore => true,
            thir::PatExpr::Identifier(_) => {
                self.stack.push(val);
                true
            }
            thir::PatExpr::StructLiteral(fields) => {
                // assert the value is a struct
                let mut val_fields = match val {
                    Value::Struct { fields, .. } => fields,
                    _ => unreachable!("not struct; should have been typechecked"),
                };

                for (fieldname, fieldpat) in fields {
                    let successful = self.try_eval_thir_pat(
                        &fieldpat.val,
                        val_fields
                            .remove(&fieldname.val)
                            .expect("does not have field, should have been typechecked"),
                    );
                    if !successful {
                        return false;
                    }
                }

                true
            }
            thir::PatExpr::New { pat, nom_id } => {
                // assert that it is a nominal type
                let v = match val {
                    Value::Nominal {
                        value,
                        nom_id: val_nom_id,
                    } => {
                        if *nom_id == val_nom_id {
                            value
                        } else {
                            unreachable!(
                                "not the right nominal type; should have been typechecked"
                            );
                        }
                    }
                    _ => unreachable!("not nominal type; should have been typechecked"),
                };
                self.try_eval_thir_pat(&pat.val, *v)
            }
            thir::PatExpr::Literal(literal) => {
                // check alpha-equivalence
                val.alpha_eq(literal)
            }
        }
    }

    // eval some value-generating code in the context of this environment
    pub fn eval_thir(&mut self, expr: thir::ValExpr) -> Value {
        match expr {
            thir::ValExpr::Error => unreachable!("erroneous code cannot be evaluated"),
            thir::ValExpr::Int { value } => todo!(),
            thir::ValExpr::Bool { value } => todo!(),
            thir::ValExpr::Float { value } => todo!(),
            thir::ValExpr::String(string) => todo!(),
            thir::ValExpr::Use(_, _) => todo!(),
            thir::ValExpr::Builtin { builtin, level } => todo!(),
            thir::ValExpr::Lam { captures, params, body } => todo!(),
            thir::ValExpr::StructLiteral(_) => todo!(),
            thir::ValExpr::New { ty, val } => todo!(),
            thir::ValExpr::CaseOf { expr, cases } => todo!(),
            thir::ValExpr::Block { statements, last_expr } => todo!(),
            thir::ValExpr::ArrayLiteral(_) => todo!(),
            thir::ValExpr::FieldAccess { root, field } => todo!(),
            thir::ValExpr::And { left, right } => todo!(),
            thir::ValExpr::Or { left, right } => todo!(),
            thir::ValExpr::App { fun, args } => todo!(),
            thir::ValExpr::PiTy { captures, params, dep_ty } => todo!(),
            thir::ValExpr::Struct(_) => todo!(),
            thir::ValExpr::Enum(_) => todo!(),
            thir::ValExpr::Union(_) => todo!(),
            thir::ValExpr::Extern { name, ty } => todo!(),
            thir::ValExpr::Loop { body } => todo!(),
            thir::ValExpr::Label { label, value } => todo!(),
            thir::ValExpr::Ret { label, value } => todo!(),
        }
    }

    pub fn eval_closure(&mut self, closure: Closure) -> Value {}
}
