use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

use crate::dlogger::DiagnosticLogger;
use crate::hir::Augmented;
use crate::{hir, hir_kindcheck};
use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::Signed;

#[derive(Clone, Debug, PartialEq)]
pub enum KindValue {
    Unknown,
    Int,
    Float,
    Bool,
    Type,
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

pub fn print_type_value(ty: &TypeValue, env: &Env) -> String {
    match ty {
        TypeValue::Error => "Error".to_string(),
        TypeValue::SymbolicVariable(id) => {
            format!("SymbolicVariable({})", env.type_name_table[*id])
        }
        TypeValue::Unit => "Unit".to_string(),
        TypeValue::Bool => "Bool".to_string(),
        TypeValue::RefConstructor => "RefConstructor".to_string(),
        TypeValue::ArrayConstructor => "ArrayConstructor".to_string(),
        TypeValue::SliceConstructor => "SliceConstructor".to_string(),
        TypeValue::IntConstructor => "IntConstructor".to_string(),
        TypeValue::UIntConstructor => "UIntConstructor".to_string(),
        TypeValue::FloatConstructor => "FloatConstructor".to_string(),
        TypeValue::Ref(ty) => format!("Ref({})", print_type_value(ty, env)),
        TypeValue::Array(ty, size) => format!(
            "Array({}, {})",
            print_type_value(ty, env),
            print_type_value(size, env)
        ),
        TypeValue::Slice(ty) => format!("Slice({})", print_type_value(ty, env)),
        TypeValue::Int(ty) => format!("Int({})", print_type_value(ty, env)),
        TypeValue::UInt(ty) => format!("UInt({})", print_type_value(ty, env)),
        TypeValue::Float(ty) => format!("Float({})", print_type_value(ty, env)),
        TypeValue::IntLit(n) => format!("IntLit({})", n),
        TypeValue::BoolLit(b) => format!("BoolLit({})", b),
        TypeValue::FloatLit(n) => format!("FloatLit({})", n),
        TypeValue::Fn {
            paramtys,
            returntype,
        } => format!(
            "Fn {{ paramtys: [{}], returntype: {} }}",
            paramtys
                .iter()
                .map(|ty| print_type_value(ty, env))
                .collect::<Vec<String>>()
                .join(", "),
            print_type_value(returntype, env)
        ),
        TypeValue::Struct(fields) => format!(
            "Struct {{ fields: [{}] }}",
            fields
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, print_type_value(ty, env)))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        TypeValue::Enum(fields) => format!(
            "Enum {{ fields: [{}] }}",
            fields
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, print_type_value(ty, env)))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        TypeValue::Union(fields) => format!(
            "Union {{ fields: [{}] }}",
            fields
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, print_type_value(ty, env)))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        TypeValue::Generic { typarams, body } => format!(
            "Generic {{ typarams: [{}], body: {} }}",
            typarams
                .iter()
                .map(|typaram| print_typaram(typaram, env))
                .collect::<Vec<String>>()
                .join(", "),
            print_type_value(body, env)
        ),
        TypeValue::Concretization {
            symbolic_constructor,
            tyargs,
        } => format!(
            "Concretization {{ symbolic_constructor: {}, tyargs: [{}] }}",
            env.type_name_table[*symbolic_constructor],
            tyargs
                .iter()
                .map(|ty| print_type_value(ty, env))
                .collect::<Vec<String>>()
                .join(", ")
        ),
    }
}

fn print_typaram(typaram: &Augmented<hir::TypePatExpr>, env: &Env) -> String {
    match &typaram.val {
        hir::TypePatExpr::Error => "Error".to_string(),
        hir::TypePatExpr::Identifier(id) => {
            format!("Identifier({})", id,)
        }
        hir::TypePatExpr::Typed { ref id, ref kind } => {
            format!("Typed {{ id: {}, kind: {} }}", id, hir_kindcheck::evaluate_hir_kind(kind))
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeValue {
    // An error when parsing
    Error,
    SymbolicVariable(usize),
    // types
    Unit,
    Bool,
    // constructors
    RefConstructor,
    ArrayConstructor,
    SliceConstructor,
    IntConstructor,
    UIntConstructor,
    FloatConstructor,
    // the constructed type
    Ref(Box<TypeValue>),
    Array(Box<TypeValue>, Box<TypeValue>),
    Slice(Box<TypeValue>),
    Int(Box<TypeValue>),
    UInt(Box<TypeValue>),
    Float(Box<TypeValue>),
    // const literals
    IntLit(BigInt),
    BoolLit(bool),
    FloatLit(BigRational),
    // type of a function
    Fn {
        paramtys: Vec<TypeValue>,
        returntype: Box<TypeValue>,
    },
    // struct and enumify
    Struct(Vec<(String, TypeValue)>),
    Enum(Vec<(String, TypeValue)>),
    Union(Vec<(String, TypeValue)>),
    // type of a generic value. Instantiated at every use point
    // type -> type
    Generic {
        typarams: Vec<Augmented<hir::TypeParamExpr>>,
        body: Box<TypeValue>,
    },
    // where the type constructor is symbolic
    Concretization {
        symbolic_constructor: usize,
        tyargs: Vec<TypeValue>,
    },
}

enum UnificationError {
    SymbolicVariableMismatch(usize, usize),
    NominalIdMismatch(usize, usize),
    NominalInnerMismatch(Box<UnificationError>),
    RefInnerMismatch(Box<UnificationError>),
    ArrayLengthMismatch(Box<UnificationError>),
    ArrayElementMismatch(Box<UnificationError>),
    SliceElementMismatch(Box<UnificationError>),
    IntWidthMismatch(Box<UnificationError>),
    UIntWidthMismatch(Box<UnificationError>),
    FloatPrecisionMismatch(Box<UnificationError>),
    IntLitMismatch {
        expected: BigInt,
        actual: BigInt,
    },
    BoolLitMismatch {
        expected: bool,
        actual: bool,
    },
    FloatLitMismatch {
        expected: BigRational,
        actual: BigRational,
    },
    FnParamMismatch {
        index: usize,
        error: Box<UnificationError>,
    },
    FnParamCountMismatch {
        expected: usize,
        actual: usize,
    },
    FnReturnMismatch(Box<UnificationError>),
    StructFieldNameMismatch {
        expected: String,
        actual: String,
    },
    StructFieldTypeMismatch {
        field: String,
        error: Box<UnificationError>,
    },
    StructFieldCountMismatch {
        expected: usize,
        actual: usize,
    },
    EnumVariantNameMismatch {
        expected: String,
        actual: String,
    },
    EnumVariantTypeMismatch {
        variant: String,
        error: Box<UnificationError>,
    },
    EnumVariantCountMismatch {
        expected: usize,
        actual: usize,
    },
    UnionVariantNameMismatch {
        expected: String,
        actual: String,
    },
    UnionVariantTypeMismatch {
        variant: String,
        error: Box<UnificationError>,
    },
    UnionVariantCountMismatch {
        expected: usize,
        actual: usize,
    },
    ConstructorTyparamNameMismatch {
        expected: usize,
        actual: usize,
    },
    ConstructorTyparamKindMismatch {
        index: usize,
        expected: KindValue,
        actual: KindValue,
    },
    ConstructorTyparamCountMismatch {
        expected: usize,
        actual: usize,
    },
    ConstructorBodyMismatch(Box<UnificationError>),
    GenericTyparamKindMismatch {
        index: usize,
        expected: KindValue,
        actual: KindValue,
    },
    GenericTyparamCountMismatch {
        expected: usize,
        actual: usize,
    },
    GenericBodyMismatch(Box<UnificationError>),
    ConcretizationConstructorMismatch {
        expected: usize,
        actual: usize,
    },
    ConcretizationTyargMismatch {
        index: usize,
        error: Box<UnificationError>,
    },
    ConcretizationTyargCountMismatch {
        expected: usize,
        actual: usize,
    },
    TypeMismatch {
        expected: TypeValue,
        actual: TypeValue,
    },
}

impl TypeValue {
    fn subst(self: &Self, bindings: &HashMap<usize, TypeValue>) -> TypeValue {
        match self {
            TypeValue::SymbolicVariable(id) => match bindings.get(id) {
                Some(ty) => ty.clone(),
                None => TypeValue::SymbolicVariable(*id),
            },
            TypeValue::Ref(v) => TypeValue::Ref(Box::new(v.subst(bindings))),
            TypeValue::Array(v, s) => {
                TypeValue::Array(Box::new(v.subst(bindings)), Box::new(s.subst(bindings)))
            }
            TypeValue::Slice(v) => TypeValue::Slice(Box::new(v.subst(bindings))),
            TypeValue::Int(v) => TypeValue::Int(Box::new(v.subst(bindings))),
            TypeValue::UInt(v) => TypeValue::UInt(Box::new(v.subst(bindings))),
            TypeValue::Float(v) => TypeValue::Float(Box::new(v.subst(bindings))),
            TypeValue::Fn {
                paramtys,
                returntype,
            } => TypeValue::Fn {
                paramtys: paramtys.iter().map(|ty| ty.subst(bindings)).collect(),
                returntype: Box::new(returntype.subst(bindings)),
            },
            TypeValue::Struct(fields) => TypeValue::Struct(
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.subst(bindings)))
                    .collect(),
            ),
            TypeValue::Enum(fields) => TypeValue::Enum(
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.subst(bindings)))
                    .collect(),
            ),
            TypeValue::Union(fields) => TypeValue::Union(
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.subst(bindings)))
                    .collect(),
            ),
            TypeValue::Generic { typarams, body } => TypeValue::Generic {
                typarams: typarams.clone(),
                body: Box::new(body.subst(bindings)),
            },
            TypeValue::Concretization {
                symbolic_constructor,
                tyargs,
            } => {
                // substitute in the arguments
                let tyargs = tyargs
                    .iter()
                    .map(|ty| ty.subst(bindings))
                    .collect::<Vec<_>>();
                match bindings.get(symbolic_constructor) {
                    // if we replaced the constructor, then we can evaluate
                    Some(constructor) => concretize_type_expr(constructor, tyargs),
                    // otherwise we just keep the concretization
                    None => TypeValue::Concretization {
                        symbolic_constructor: *symbolic_constructor,
                        tyargs,
                    },
                }
            }
            _ => self.clone(),
        }
    }

    // returns a hashmap of bindings between symbolic variables (represented with a usize) and the types they would take on if this binding were used
    fn unify_with<'a>(
        self: &Self,
        other: &'a TypeValue,
    ) -> Result<HashMap<usize, &'a TypeValue>, UnificationError> {
        match (self, other) {
            (TypeValue::Error, _) => Ok(HashMap::new()),
            (_, TypeValue::Error) => Ok(HashMap::new()),
            (TypeValue::SymbolicVariable(id1), _) => Ok(HashMap::from([(id1.clone(), other)])),
            (TypeValue::Unit, TypeValue::Unit) => Ok(HashMap::new()),
            (TypeValue::Bool, TypeValue::Bool) => Ok(HashMap::new()),
            (TypeValue::RefConstructor, TypeValue::RefConstructor) => Ok(HashMap::new()),
            (TypeValue::ArrayConstructor, TypeValue::ArrayConstructor) => Ok(HashMap::new()),
            (TypeValue::SliceConstructor, TypeValue::SliceConstructor) => Ok(HashMap::new()),
            (TypeValue::IntConstructor, TypeValue::IntConstructor) => Ok(HashMap::new()),
            (TypeValue::UIntConstructor, TypeValue::UIntConstructor) => Ok(HashMap::new()),
            (TypeValue::FloatConstructor, TypeValue::FloatConstructor) => Ok(HashMap::new()),
            (TypeValue::Ref(inner1), TypeValue::Ref(inner2)) => inner1
                .unify_with(inner2)
                .map_err(|err| UnificationError::RefInnerMismatch(Box::new(err))),
            (TypeValue::Array(elem1, len1), TypeValue::Array(elem2, len2)) => {
                elem1
                    .unify_with(elem2)
                    .map_err(|err| UnificationError::ArrayElementMismatch(Box::new(err)))?;
                len1.unify_with(len2)
                    .map_err(|err| UnificationError::ArrayLengthMismatch(Box::new(err)))
            }
            (TypeValue::Slice(elemty1), TypeValue::Slice(elemty2)) => elemty1
                .unify_with(elemty2)
                .map_err(|err| UnificationError::SliceElementMismatch(Box::new(err))),
            (TypeValue::Int(width1), TypeValue::Int(width2)) => width1
                .unify_with(width2)
                .map_err(|err| UnificationError::IntWidthMismatch(Box::new(err))),
            (TypeValue::UInt(width1), TypeValue::UInt(width2)) => width1
                .unify_with(width2)
                .map_err(|err| UnificationError::UIntWidthMismatch(Box::new(err))),
            (TypeValue::Float(precision1), TypeValue::Float(precision2)) => precision1
                .unify_with(precision2)
                .map_err(|err| UnificationError::FloatPrecisionMismatch(Box::new(err))),
            (TypeValue::IntLit(int1), TypeValue::IntLit(int2)) => {
                if int1 == int2 {
                    Ok(HashMap::new())
                } else {
                    Err(UnificationError::IntLitMismatch {
                        expected: *int2,
                        actual: *int1,
                    })
                }
            }
            (TypeValue::BoolLit(val1), TypeValue::BoolLit(val2)) => {
                if val1 == val2 {
                    Ok(HashMap::new())
                } else {
                    Err(UnificationError::BoolLitMismatch {
                        expected: *val2,
                        actual: *val1,
                    })
                }
            }
            (TypeValue::FloatLit(val1), TypeValue::FloatLit(val2)) => {
                if val1 == val2 {
                    Ok(HashMap::new())
                } else {
                    Err(UnificationError::FloatLitMismatch {
                        expected: *val2,
                        actual: *val1,
                    })
                }
            }
            (
                TypeValue::Fn {
                    paramtys: paramtys1,
                    returntype: returntype1,
                },
                TypeValue::Fn {
                    paramtys: paramtys2,
                    returntype: returntype2,
                },
            ) => {
                let bindings = HashMap::new();
                for (i, (paramty1, paramty2)) in paramtys1.iter().zip(paramtys2.iter()).enumerate()
                {
                    let parambindings = paramty1.unify_with(paramty2).map_err(|err| {
                        UnificationError::FnParamMismatch {
                            index: i,
                            error: Box::new(err),
                        }
                    })?;
                    for (id, ty) in parambindings {
                        match bindings.entry(id) {
                            Entry::Occupied(entry) => {
                                let boundty = entry.get();
                                match ty.unify_with(boundty) {
                                    Ok(_) => {}
                                    Err(err) => {
                                        return Err(UnificationError::FnParamBindingMismatch {
                                            id,
                                            expected: boundty.clone(),
                                            actual: ty.clone(),
                                        });
                                    }
                                }
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(ty);
                            }
                        }
                    }
                }
                if paramtys1.len() != paramtys2.len() {
                    return Err(UnificationError::FnParamCountMismatch {
                        expected: paramtys2.len(),
                        actual: paramtys1.len(),
                    });
                }
                returntype1
                    .unify_with(returntype2)
                    .map_err(|err| UnificationError::FnReturnMismatch(Box::new(err)))
            }
            (Self::Struct(l0), Self::Struct(r0)) => {
                for ((l_field_name, l_field_type), (r_field_name, r_field_type)) in
                    l0.iter().zip(r0.iter())
                {
                    if l_field_name != r_field_name {
                        Err(UnificationError::StructFieldNameMismatch {
                            expected: r_field_name.clone(),
                            actual: l_field_name.clone(),
                        })?;
                    }
                    l_field_type.unify_with(r_field_type).map_err(|err| {
                        UnificationError::StructFieldTypeMismatch {
                            field: r_field_name.clone(),
                            error: Box::new(err),
                        }
                    })?;
                }
                Err(UnificationError::StructFieldCountMismatch {
                    expected: r0.len(),
                    actual: l0.len(),
                })?;
                Ok(())
            }
            (Self::Enum(l0), Self::Enum(r0)) => {
                for ((l_variant_name, l_variant_type), (r_variant_name, r_variant_type)) in
                    l0.iter().zip(r0.iter())
                {
                    if l_variant_name != r_variant_name {
                        Err(UnificationError::EnumVariantNameMismatch {
                            expected: r_variant_name.clone(),
                            actual: l_variant_name.clone(),
                        })?;
                    }
                    l_variant_type.unify_with(r_variant_type).map_err(|err| {
                        UnificationError::EnumVariantTypeMismatch {
                            variant: r_variant_name.clone(),
                            error: Box::new(err),
                        }
                    })?;
                }
                Err(UnificationError::EnumVariantCountMismatch {
                    expected: r0.len(),
                    actual: l0.len(),
                })?;
                Ok(())
            }
            (Self::Union(l0), Self::Union(r0)) => {
                for ((l_variant_name, l_variant_type), (r_variant_name, r_variant_type)) in
                    l0.iter().zip(r0.iter())
                {
                    if l_variant_name != r_variant_name {
                        Err(UnificationError::UnionVariantNameMismatch {
                            expected: r_variant_name.clone(),
                            actual: l_variant_name.clone(),
                        })?;
                    }
                    l_variant_type.unify_with(r_variant_type).map_err(|err| {
                        UnificationError::UnionVariantTypeMismatch {
                            variant: r_variant_name.clone(),
                            error: Box::new(err),
                        }
                    })?;
                }
                Err(UnificationError::UnionVariantCountMismatch {
                    expected: r0.len(),
                    actual: l0.len(),
                })?;
                Ok(())
            }
            (
                Self::Generic {
                    typarams: l_typarams,
                    body: l_body,
                },
                Self::Generic {
                    typarams: r_typarams,
                    body: r_body,
                },
            ) => {
                for (i, (l_typaram, r_typaram)) in
                    l_typarams.iter().zip(r_typarams.iter()).enumerate()
                {
                    match (l_typaram, r_typaram) {
                        (
                            TypeParam::SymbolicVariable {
                                id: l_id,
                                kind: l_kind,
                            },
                            TypeParam::SymbolicVariable {
                                id: r_id,
                                kind: r_kind,
                            },
                        ) => {
                            if l_kind != r_kind {
                                return Err(UnificationError::GenericTyparamKindMismatch {
                                    index: i,
                                    expected: r_kind.clone(),
                                    actual: l_kind.clone(),
                                });
                            }
                        }
                        (_, _) => (),
                    }
                }
                if l_typarams.len() != r_typarams.len() {
                    return Err(UnificationError::GenericTyparamCountMismatch {
                        expected: r_typarams.len(),
                        actual: l_typarams.len(),
                    });
                }
                l_body
                    .unify_with(r_body)
                    .map_err(|err| UnificationError::GenericBodyMismatch(Box::new(err)))
            }
            (
                Self::Concretization {
                    symbolic_constructor: l_symbolic_constructor,
                    tyargs: l_tyargs,
                },
                Self::Concretization {
                    symbolic_constructor: r_symbolic_constructor,
                    tyargs: r_tyargs,
                },
            ) => {
                if l_symbolic_constructor != r_symbolic_constructor {
                    return Err(UnificationError::ConcretizationConstructorMismatch {
                        expected: r_symbolic_constructor.clone(),
                        actual: l_symbolic_constructor.clone(),
                    });
                }
                for (i, (l_tyarg, r_tyarg)) in l_tyargs.iter().zip(r_tyargs.iter()).enumerate() {
                    l_tyarg.unify_with(r_tyarg).map_err(|err| {
                        UnificationError::ConcretizationTyargMismatch {
                            index: i,
                            error: Box::new(err),
                        }
                    })?;
                }
                if l_tyargs.len() != r_tyargs.len() {
                    return Err(UnificationError::ConcretizationTyargCountMismatch {
                        expected: r_tyargs.len(),
                        actual: l_tyargs.len(),
                    });
                }
                Ok(())
            }
            (l, r) => Err(UnificationError::TypeMismatch {
                expected: r.clone(),
                actual: l.clone(),
            }),
        }
    }
}

fn concretize_type_expr(constructor: &TypeValue, mut tyargs: Vec<TypeValue>) -> TypeValue {
    match constructor {
        TypeValue::Error => TypeValue::Error,
        TypeValue::SymbolicVariable(id) => TypeValue::Concretization {
            symbolic_constructor: *id,
            tyargs,
        },
        TypeValue::RefConstructor => {
            assert!(tyargs.len() == 1, "wrong number of arguments");
            let arg0 = tyargs.remove(0);
            TypeValue::Ref(Box::new(arg0))
        }
        TypeValue::ArrayConstructor => {
            assert!(tyargs.len() == 2, "wrong number of arguments");
            let arg1 = tyargs.remove(1);
            let arg0 = tyargs.remove(0);
            TypeValue::Array(Box::new(arg0), Box::new(arg1))
        }
        TypeValue::SliceConstructor => {
            assert!(tyargs.len() == 1, "wrong number of arguments");
            let arg0 = tyargs.remove(0);
            TypeValue::Ref(Box::new(arg0))
        }
        TypeValue::IntConstructor => {
            assert!(tyargs.len() == 1, "wrong number of arguments");
            let arg0 = tyargs.remove(0);
            TypeValue::Int(Box::new(arg0))
        }
        TypeValue::UIntConstructor => {
            assert!(tyargs.len() == 1, "wrong number of arguments");
            let arg0 = tyargs.remove(0);
            TypeValue::UInt(Box::new(arg0))
        }
        TypeValue::FloatConstructor => {
            assert!(tyargs.len() == 1, "wrong number of arguments");
            let arg0 = tyargs.remove(0);
            TypeValue::Float(Box::new(arg0))
        }
        TypeValue::Generic { typarams, body } => {
            assert!(
                typarams.len() == tyargs.len(),
                "wrong number of arguments; should be kindchecked"
            );
            let mut bindings = HashMap::new();
            for (typat, tyarg) in std::iter::zip(typarams, tyargs) {
                match &typat.val {
                    hir::TypePatExpr::Typed { id, .. } => {
                        bindings.insert(*id, tyarg);
                    }
                    hir::TypePatExpr::Identifier(id) => {
                        bindings.insert(*id, tyarg);
                    }
                    hir::TypePatExpr::Error => {}
                }
            }
            body.subst(&bindings)
        }
        _ => {
            unreachable!("concretization of a non-generic; should have been kindchecked");
        }
    }
}

// evaluate a type expression in a context
// NOTE: we assume that all identifiers have already been resolved.
// NOTE: we assume that all types are well-kinded
pub fn typecheck_hir_type_infermode(
    v: &Augmented<hir::TypeExpr>,
    dlogger: &mut DiagnosticLogger,
    type_name_table: &Vec<String>,
    type_value_table: &Vec<Option<TypeValue>>,
) -> TypeValue {
    match &v.val {
        hir::TypeExpr::Error => TypeValue::Error,
        hir::TypeExpr::Identifier(id) => TypeValue::SymbolicVariable(*id),
        hir::TypeExpr::UnitTy => TypeValue::Unit,
        hir::TypeExpr::BoolTy => TypeValue::Bool,
        hir::TypeExpr::RefConstructorTy => TypeValue::RefConstructor,
        hir::TypeExpr::ArrayConstructorTy => TypeValue::ArrayConstructor,
        hir::TypeExpr::SliceConstructorTy => TypeValue::SliceConstructor,
        hir::TypeExpr::IntConstructorTy => TypeValue::IntConstructor,
        hir::TypeExpr::UIntConstructorTy => TypeValue::UIntConstructor,
        hir::TypeExpr::FloatConstructorTy => TypeValue::FloatConstructor,
        hir::TypeExpr::Int(x) => TypeValue::IntLit(x.clone()),
        hir::TypeExpr::Bool(x) => TypeValue::BoolLit(x.clone()),
        hir::TypeExpr::Float(x) => TypeValue::FloatLit(x.clone()),
        hir::TypeExpr::Struct(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((
                    identifier.clone(),
                    typecheck_hir_type_infermode(&expr, dlogger, type_name_table, type_value_table),
                ));
            }
            TypeValue::Struct(s_fields)
        }
        hir::TypeExpr::Enum(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((
                    identifier.clone(),
                    typecheck_hir_type_infermode(&expr, dlogger, type_name_table, type_value_table),
                ));
            }
            TypeValue::Enum(s_fields)
        }
        hir::TypeExpr::Union(fields) => {
            let mut s_fields = vec![];
            for (identifier, expr) in fields {
                s_fields.push((
                    identifier.clone(),
                    typecheck_hir_type_infermode(&expr, dlogger, type_name_table, type_value_table),
                ));
            }
            TypeValue::Union(s_fields)
        }
        hir::TypeExpr::Fn { paramtys, returnty } => TypeValue::Fn {
            paramtys: paramtys
                .iter()
                .map(|x| {
                    typecheck_hir_type_infermode(x, dlogger, type_name_table, type_value_table)
                })
                .collect(),
            returntype: Box::new(typecheck_hir_type_infermode(
                &returnty,
                dlogger,
                type_name_table,
                type_value_table,
            )),
        },
        hir::TypeExpr::Generic {
            params,
            returnkind: _,
            body,
        } => {
            let body = Box::new(typecheck_hir_type_infermode(
                body,
                dlogger,
                type_name_table,
                type_value_table,
            ));
            TypeValue::Generic {
                typarams: params.clone(),
                body,
            }
        }
        // substitute the generic arguments into the body
        hir::TypeExpr::Concretization { genericty, tyargs } => {
            // first we evaluate the type of the generic function
            let generic_val = typecheck_hir_type_infermode(
                &genericty,
                dlogger,
                type_name_table,
                type_value_table,
            );
            let tyargs = tyargs
                .iter()
                .map(|x| {
                    typecheck_hir_type_infermode(x, dlogger, type_name_table, type_value_table)
                })
                .collect::<Vec<_>>();
            // attempt to concretize the type (will only work if there are no symbolic variables in the type)
            concretize_type_expr(&generic_val, tyargs)
        }
    }
}

pub struct Env {
    pub type_name_table: Vec<String>,
    pub type_value_table: Vec<Option<TypeValue>>,
    pub val_name_table: Vec<String>,
    pub val_type_table: Vec<Option<TypeValue>>,
}

// checks and patches the
pub fn typecheck_hir_elseexpr_checkmode_and_patch(
    v: &Augmented<hir::ElseExpr>,
    dlogger: &mut DiagnosticLogger,
    expected_type: &TypeValue,
) {
    match v.val {
        hir::ElseExpr::Error => {}
        hir::ElseExpr::Else(ref e) => {
            typecheck_hir_blockexpr_checkmode(e, dlogger, env, expected_type)
        }
        hir::ElseExpr::Elif {
            ref cond,
            ref then_branch,
            ref else_branch,
        } => {
            typecheck_hir_value_checkmode(cond, dlogger, env, &TypeValue::Bool);
            typecheck_hir_blockexpr_checkmode(then_branch, dlogger, env, expected_type);
            if let Some(else_branch) = else_branch {
                typecheck_hir_elseexpr_checkmode(else_branch, dlogger, env, expected_type);
            }
        }
    }
}

pub fn typecheck_hir_blockstatement(
    v: &Augmented<hir::BlockStatement>,
    dlogger: &mut DiagnosticLogger,
    env: &Env,
) {
    match v.val {
        hir::BlockStatement::NoOp => {}
        // should already be typechecked
        hir::BlockStatement::TypeDef {
            ref typat,
            ref value,
        } => {
            for typat in typarams {
                intro_hir_typat_symbolic(typat, dlogger, env);
            }
            let tyval = typecheck_hir_type_infermode(value, dlogger, env);
            let typarams = typarams
                .iter()
                .map(|x| hir_typat_to_param(x, dlogger, env))
                .collect::<Vec<_>>();
            intro_hir_typat(typat, dlogger, env, tyval, typarams);
        }
        hir::BlockStatement::ValDef { ref pat, ref value } => {
            for typat in typarams {
                intro_hir_typat_symbolic(typat, dlogger, env);
            }
            let valty = typecheck_hir_value_infermode(value, dlogger, env);
            let typarams = typarams
                .iter()
                .map(|x| hir_typat_to_param(x, dlogger, env))
                .collect::<Vec<_>>();
            intro_and_typecheck_hir_pat_checkmode(pat, dlogger, env, &valty, &typarams);
        }
        hir::BlockStatement::Set {
            ref place,
            ref value,
        } => {
            let ty = typecheck_hir_value_infermode(place, dlogger, env);
            typecheck_hir_value_checkmode(value, dlogger, env, &ty);
        }
        hir::BlockStatement::While { ref cond, ref body } => {
            typecheck_hir_value_checkmode(cond, dlogger, env, &TypeValue::Bool);
            typecheck_hir_blockexpr_checkmode(body, dlogger, env, &TypeValue::Unit);
        }
        hir::BlockStatement::For {
            pattern,
            start,
            end,
            by,
            body,
            ..
        } => {
            let index_type = typecheck_hir_value_infermode(&start, dlogger, env);
            typecheck_hir_value_checkmode(&end, dlogger, env, &index_type);
            if let Some(by) = by {
                typecheck_hir_value_checkmode(&by, dlogger, env, &index_type);
            }
            typecheck_hir_blockexpr_checkmode(&body, dlogger, env, &TypeValue::Unit);
        }
        hir::BlockStatement::Do(ref expr) => {
            typecheck_hir_value_checkmode(expr, dlogger, env, &TypeValue::Unit);
        }
    }
}

pub fn typecheck_hir_blockexpr_checkmode(
    v: &Augmented<hir::BlockExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &Env,
    expected_type: &TypeValue,
) {
    for s in v.val.statements.iter() {
        typecheck_hir_blockstatement(s, dlogger, env);
    }
    if let Some(ref e) = v.val.last_expression {
        typecheck_hir_value_checkmode(e, dlogger, env, expected_type);
    }
}

pub fn typecheck_hir_blockexpr_infermode(
    v: &Augmented<hir::BlockExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &Env,
) -> TypeValue {
    for s in v.val.statements.iter() {
        typecheck_hir_blockstatement(s, dlogger, env);
    }
    if let Some(ref e) = v.val.last_expression {
        typecheck_hir_value_infermode(e, dlogger, env)
    } else {
        TypeValue::Unit
    }
}

pub fn typecheck_hir_value_infermode(
    v: &Augmented<hir::ValExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &Env,
) -> TypeValue {
    match &v.val {
        hir::ValExpr::Error => TypeValue::Error,
        hir::ValExpr::Unit => TypeValue::Unit,
        hir::ValExpr::Int(_) => TypeValue::Int(Box::new(TypeValue::IntLit((64).into()))),
        hir::ValExpr::Bool(_) => TypeValue::Bool,
        hir::ValExpr::Float(_) => TypeValue::Float(Box::new(TypeValue::IntLit((64).into()))),
        hir::ValExpr::String(_) => TypeValue::Slice(Box::new(TypeValue::Int(Box::new(
            TypeValue::IntLit((8).into()),
        )))),
        hir::ValExpr::Identifier(id) => env.val_type_table[*id]
            .expect("identifier not initialized with type")
            .clone(),
        hir::ValExpr::Ref(v) => {
            TypeValue::Ref(Box::new(typecheck_hir_value_infermode(v, dlogger, env)))
        }
        hir::ValExpr::Deref(v) => {
            let vtype = typecheck_hir_value_infermode(v, dlogger, env);
            match vtype {
                TypeValue::Ref(x) => *x,
                _ => {
                    dlogger.log_deref_of_non_reference(v.range);
                    TypeValue::Error
                }
            }
        }
        hir::ValExpr::StructLiteral(fields) => {
            let mut fieldtypes = Vec::new();
            for (name, field) in fields.iter_mut() {
                fieldtypes.push((
                    name.clone(),
                    typecheck_hir_value_infermode(field, dlogger, env),
                ));
            }
            TypeValue::Struct(fieldtypes)
        }
        hir::ValExpr::BinaryOp {
            op,
            left_operand,
            right_operand,
        } => match op {
            // math or numerical comparison
            hir::ValBinaryOpKind::Add
            | hir::ValBinaryOpKind::Sub
            | hir::ValBinaryOpKind::Mul
            | hir::ValBinaryOpKind::Div
            | hir::ValBinaryOpKind::Rem => {
                let left_type = typecheck_hir_value_infermode(left_operand, dlogger, env);

                match left_type {
                    binop_type @ (TypeValue::Int(_) | TypeValue::UInt(_) | TypeValue::Float(_)) => {
                        typecheck_hir_value_checkmode(right_operand, dlogger, env, &binop_type);
                        binop_type
                    }
                    _ => {
                        dlogger.log_math_on_non_numeric(v.range);
                        TypeValue::Error
                    }
                }
            }
            hir::ValBinaryOpKind::Lt
            | hir::ValBinaryOpKind::Leq
            | hir::ValBinaryOpKind::Gt
            | hir::ValBinaryOpKind::Geq => {
                let left_type = typecheck_hir_value_infermode(left_operand, dlogger, env);

                match left_type {
                    TypeValue::Int(_) | TypeValue::UInt(_) | TypeValue::Float(_) => {
                        typecheck_hir_value_checkmode(right_operand, dlogger, env, &left_type);
                        TypeValue::Bool
                    }
                    _ => {
                        dlogger.log_comparison_on_non_numeric(v.range);
                        TypeValue::Bool
                    }
                }
            }
            hir::ValBinaryOpKind::And | hir::ValBinaryOpKind::Or => {
                typecheck_hir_value_checkmode(left_operand, dlogger, env, &TypeValue::Bool);
                typecheck_hir_value_checkmode(right_operand, dlogger, env, &TypeValue::Bool);
                TypeValue::Bool
            }
            hir::ValBinaryOpKind::Eq | hir::ValBinaryOpKind::Neq => {
                let left_type = typecheck_hir_value_infermode(left_operand, dlogger, env);
                typecheck_hir_value_checkmode(right_operand, dlogger, env, &left_type);

                match left_type {
                    TypeValue::Int(_) | TypeValue::UInt(_) | TypeValue::Float(_) => TypeValue::Bool,
                    _ => {
                        dlogger.log_equality_on_non_integral(left_operand.range);
                        TypeValue::Bool
                    }
                }
            }
        },
        hir::ValExpr::IfThen {
            cond,
            then_branch,
            else_branch,
        } => {
            typecheck_hir_value_checkmode(cond, dlogger, env, &TypeValue::Bool);
            let if_type = typecheck_hir_blockexpr_infermode(then_branch, dlogger, env);
            if let Some(elseexpr) = else_branch {
                typecheck_hir_elseexpr_checkmode(elseexpr, dlogger, env, &if_type);
            }
            if_type
        }
        hir::ValExpr::CaseOf {
            expr,
            first_case,
            rest_cases,
        } => {
            let expr_type = typecheck_hir_value_infermode(expr, dlogger, env);
            intro_and_typecheck_hir_targetexpr(&first_case.val.target, dlogger, env, &expr_type);
            let first_case_type = typecheck_hir_value_infermode(&first_case.val.body, dlogger, env);
            for case in rest_cases {
                intro_and_typecheck_hir_targetexpr(&case.val.target, dlogger, env, &expr_type);
                typecheck_hir_value_checkmode(&case.val.body, dlogger, env, &first_case_type);
            }
            first_case_type
        }
        hir::ValExpr::Block(block) => typecheck_hir_blockexpr_infermode(block, dlogger, env),
        hir::ValExpr::ArrayAccess { root, index } => {
            let rtype = typecheck_hir_value_infermode(root, dlogger, env);
            typecheck_hir_value_checkmode(
                index,
                dlogger,
                env,
                &TypeValue::Int(Box::new(TypeValue::IntLit((64).into()))),
            );
            match rtype {
                TypeValue::Array(x, _) => *x,
                _ => {
                    dlogger.log_array_access_of_non_array(root.range);
                    TypeValue::Error
                }
            }
        }
        hir::ValExpr::FieldAccess { root, field } => {
            let rtype = typecheck_hir_value_infermode(root, dlogger, env);
            match rtype {
                TypeValue::Struct(fields) => {
                    for (i, f) in fields.iter().enumerate() {
                        if f.0 == *field {
                            return f.1.clone();
                        }
                    }
                    dlogger.log_field_access_of_nonexistent_field(root.range, field);
                    TypeValue::Error
                }
                _ => {
                    dlogger.log_field_access_of_non_struct(root.range);
                    TypeValue::Error
                }
            }
        }
        hir::ValExpr::ArrayLiteral(ref elems) => match elems.as_slice() {
            [] => {
                dlogger.log_cannot_infer_array_type(v.range);
                TypeValue::Error
            }
            [first, others @ ..] => {
                let first_type = typecheck_hir_value_infermode(first, dlogger, env);
                for elem in others {
                    typecheck_hir_value_checkmode(elem, dlogger, env, &first_type);
                }
                TypeValue::Array(
                    Box::new(first_type),
                    Box::new(TypeValue::IntLit(elems.len().into())),
                )
            }
        },
        // concretize a generic val
        hir::ValExpr::Concretization { generic, tyargs } => {
            let generic_type = typecheck_hir_value_infermode(generic, dlogger, env);
            let tyargs = tyargs
                .iter()
                .map(|tyarg| typecheck_hir_type_infermode(tyarg, dlogger, env))
                .collect::<Vec<_>>();
            match generic_type {
                TypeValue::Generic {
                    ref body,
                    ref typarams,
                } => {
                    if tyargs.len() != typarams.len() {
                        dlogger.log_wrong_number_type_args(v.range, typarams.len(), tyargs.len());
                        TypeValue::Error
                    } else {
                        for (tyval, typat) in std::iter::zip(tyargs, typarams) {
                            env.bind_typaram(typat, tyval);
                        }
                        let ty = body.subst(env);
                        for typat in typarams {
                            env.unbind_typaram(typat);
                        }
                        ty
                    }
                }
                _ => {
                    dlogger.log_concretization_of_non_generic(v.range);
                    TypeValue::Error
                }
            }
        }
        // apply a nongeneric function
        hir::ValExpr::App { fun, args } => {
            let fn_type = typecheck_hir_value_infermode(fun, dlogger, env);
            match fn_type {
                TypeValue::Fn {
                    paramtys,
                    returntype,
                } => {
                    if args.len() != paramtys.len() {
                        dlogger.log_wrong_number_args(v.range, paramtys.len(), args.len());
                    } else {
                        for (arg, paramty) in args.iter().zip(paramtys.iter()) {
                            typecheck_hir_value_checkmode(arg, dlogger, env, paramty);
                        }
                    }
                    *returntype
                }
                _ => {
                    dlogger
                        .log_application_of_non_function(v.range, &print_type_value(&fn_type, env));
                    TypeValue::Error
                }
            }
        }
    }
}

fn intro_hir_typat(
    typat: &Augmented<hir::TypePatExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    typeval: TypeValue,
    typarams: Vec<TypeParam>,
) {
    match typat.val {
        hir::TypePatExpr::Error => {}
        hir::TypePatExpr::Identifier { id, kind } => match typarams.len() {
            0 => {
                env.type_value_table[id] = Some(typeval);
            }
            _ => {
                env.type_value_table[id] = Some(TypeValue::Constructor {
                    typarams,
                    body: Box::new(typeval),
                });
            }
        },
    }
}

fn intro_hir_typat_symbolic(
    typat: &Augmented<hir::TypePatExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
) {
    match typat.val {
        hir::TypePatExpr::Error => {}
        hir::TypePatExpr::Identifier { id, kind } => {
            env.type_value_table[id] = Some(TypeValue::SymbolicVariable(id));
        }
    }
}

fn intro_and_typecheck_hir_pat_checkmode(
    pat: &Augmented<hir::ValPatExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    expected_type: &TypeValue,
    typarams: &Vec<TypeParam>,
) {
    match pat.val {
        hir::ValPatExpr::Error => {}
        hir::ValPatExpr::Ignore => {}
        hir::ValPatExpr::Identifier { mutable, id } => {
            env.val_type_table[id] = match typarams.len() {
                0 => Some(expected_type.clone()),
                _ => Some(TypeValue::Generic {
                    body: Box::new(expected_type.clone()),
                    typarams: typarams.clone(),
                }),
            };
        }
        hir::ValPatExpr::StructLiteral(got_fields) => match expected_type {
            TypeValue::Struct(expected_fields) => {
                for (ref field_name, ref field_pat) in got_fields {
                    let field_type = expected_fields
                        .iter()
                        .find(|(n, _)| n == field_name)
                        .map(|(_, t)| t);
                    match field_type {
                        Some(field_type) => {
                            intro_and_typecheck_hir_pat_checkmode(
                                field_pat, dlogger, env, field_type, typarams,
                            );
                        }
                        None => {
                            dlogger.log_struct_pattern_field_not_in_struct(
                                field_pat.range,
                                field_name,
                            );
                        }
                    }
                }
            }
            _ => {
                dlogger.log_struct_pattern_on_non_struct(pat.range);
            }
        },
        hir::ValPatExpr::Typed { ref pat, ref ty } => {
            let pat_ty = typecheck_hir_type_infermode(ty, dlogger, env);
            if !pat_ty.unify_with(expected_type) {
                dlogger.log_type_mismatch(
                    ty.range,
                    &print_type_value(expected_type, env),
                    &print_type_value(&pat_ty, env),
                );
            }
            intro_and_typecheck_hir_pat_checkmode(pat, dlogger, env, &pat_ty, typarams);
        }
    }
}

// this function recursively introduces all variables in a pattern into the environment, with the error type.
// we do this to ensure that following pieces of code can assume that all variables are in the environment.
fn intro_hir_pat_errors(
    pat: &Augmented<hir::ValPatExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    typarams: &Vec<TypeParam>,
) {
    match pat.val {
        hir::ValPatExpr::Error => {}
        hir::ValPatExpr::Ignore => {}
        hir::ValPatExpr::Identifier { mutable, id } => {
            env.val_type_table[id] = match typarams.len() {
                0 => Some(TypeValue::Error),
                _ => Some(TypeValue::Generic {
                    body: Box::new(TypeValue::Error),
                    typarams: typarams.clone(),
                }),
            };
        }
        hir::ValPatExpr::StructLiteral(got_fields) => {
            for (_, ref field_pat) in got_fields {
                intro_hir_pat_errors(field_pat, dlogger, env, typarams);
            }
        }
        hir::ValPatExpr::Typed { ref pat, ref ty } => {
            intro_hir_pat_errors(pat, dlogger, env, typarams);
        }
    }
}

fn intro_and_typecheck_hir_pat_infermode(
    pat: &Augmented<hir::ValPatExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    typarams: &Vec<TypeParam>,
) -> TypeValue {
    match pat.val {
        hir::ValPatExpr::Error => TypeValue::Error,
        hir::ValPatExpr::Ignore => {
            dlogger.log_cannot_infer_pattern_type(pat.range);
            TypeValue::Error
        }
        hir::ValPatExpr::Identifier { .. } => {
            dlogger.log_cannot_infer_pattern_type(pat.range);
            intro_hir_pat_errors(pat, dlogger, env, typarams);
            TypeValue::Error
        }
        hir::ValPatExpr::StructLiteral(_) => {
            dlogger.log_cannot_infer_pattern_type(pat.range);
            intro_hir_pat_errors(pat, dlogger, env, typarams);
            TypeValue::Error
        }
        hir::ValPatExpr::Typed { ref pat, ref ty } => {
            let pat_ty = typecheck_hir_type_infermode(ty, dlogger, env);
            intro_and_typecheck_hir_pat_checkmode(pat, dlogger, env, &pat_ty, typarams);
            pat_ty
        }
    }
}

fn typecheck_numerical_literal(
    i: &BigInt,
    nbits: &TypeValue,
    signed: bool,
    dlogger: &mut DiagnosticLogger,
    range: Range,
) {
    // the number of bits needed to fully represent the magnitude of i
    let ibits = BigInt::from(i.bits());
    // the number of bits needed to fully represent the magnitude of (i + 1)
    let i_plus_one_bits = BigInt::from((i + BigInt::from(1)).bits());
    match nbits {
        TypeValue::IntLit(nbits) => {
            if signed {
                if i.is_negative() && &i_plus_one_bits >= nbits {
                    dlogger.log_int_too_small(range, nbits);
                } else if ibits >= nbits - 1 {
                    dlogger.log_int_too_large(range, nbits);
                }
            } else {
                if i.is_negative() {
                    dlogger.log_uint_negative(range);
                } else if &ibits >= nbits {
                    dlogger.log_uint_too_large(range, nbits);
                }
            }
        }
        TypeValue::SymbolicVariable(_) => {}
        _ => unreachable!("should have been kindchecked"),
    }
}

fn intro_and_typecheck_hir_targetexpr(
    target: &Augmented<hir::CaseTargetExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    expr_type: &TypeValue,
) {
    match (target.val, expr_type) {
        (hir::CaseTargetExpr::Error, _) => {}
        (hir::CaseTargetExpr::Unit, TypeValue::Unit) => {}
        (hir::CaseTargetExpr::Bool(_), TypeValue::Bool) => {}
        (hir::CaseTargetExpr::Int(ref i), TypeValue::Int(ref nbits)) => {
            typecheck_numerical_literal(i, nbits, true, dlogger, target.range)
        }
        (hir::CaseTargetExpr::Int(ref i), TypeValue::UInt(ref nbits)) => {
            typecheck_numerical_literal(i, nbits, false, dlogger, target.range)
        }
        (hir::CaseTargetExpr::PatExpr(ref pat), _) => {
            intro_and_typecheck_hir_pat_checkmode(pat, dlogger, env, expr_type, &vec![]);
        }
        _ => {
            let case_target_str = match target.val {
                hir::CaseTargetExpr::Error => unreachable!(),
                hir::CaseTargetExpr::Unit => "unit".to_string(),
                hir::CaseTargetExpr::Bool(_) => "bool".to_string(),
                hir::CaseTargetExpr::Int(_) => "int".to_string(),
                hir::CaseTargetExpr::PatExpr(_) => unreachable!(),
            };
            dlogger.log_case_target_type_mismatch(
                target.range,
                &print_type_value(expr_type, env),
                &case_target_str,
            );
        }
    }
}

pub fn typecheck_hir_value_checkmode(
    v: &Augmented<hir::ValExpr>,
    dlogger: &mut DiagnosticLogger,
    env: &mut Env,
    expected_type: &TypeValue,
) {
    match (v.val, expected_type) {
        (hir::ValExpr::Float(_), TypeValue::Float(_)) => {}
        // block expressions
        (hir::ValExpr::Block(ref block), _) => {
            typecheck_hir_blockexpr_checkmode(block, dlogger, env, expected_type);
        }
        (
            hir::ValExpr::IfThen {
                ref cond,
                ref then_branch,
                ref else_branch,
            },
            _,
        ) => {
            typecheck_hir_value_checkmode(cond, dlogger, env, &TypeValue::Bool);
            typecheck_hir_blockexpr_checkmode(then_branch, dlogger, env, expected_type);
            if let Some(ref elseexpr) = else_branch {
                typecheck_hir_elseexpr_checkmode(elseexpr, dlogger, env, expected_type);
            }
        }
        (
            hir::ValExpr::CaseOf {
                ref expr,
                ref first_case,
                ref rest_cases,
            },
            _,
        ) => {
            let expr_type = typecheck_hir_value_infermode(expr, dlogger, env);
            intro_and_typecheck_hir_targetexpr(&first_case.val.target, dlogger, env, &expr_type);
            typecheck_hir_value_checkmode(&first_case.val.body, dlogger, env, expected_type);
            for case in rest_cases {
                intro_and_typecheck_hir_targetexpr(&case.val.target, dlogger, env, &expr_type);
                typecheck_hir_value_checkmode(&case.val.body, dlogger, env, expected_type);
            }
        }
        // everything else
        _ => {
            let actual_type = typecheck_hir_value_infermode(v, dlogger, env);
            if !actual_type.unify_with(expected_type) {
                dlogger.log_type_mismatch(
                    v.range,
                    &print_type_value(expected_type, env),
                    &print_type_value(&actual_type, env),
                );
            }
        }
    }
}
