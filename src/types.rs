use num_bigint::BigInt;
use num_rational::BigRational;

#[derive(Clone, Debug)]
pub enum KindExpr {
    Error,
    Type,
    Int,
    UInt,
    Float,
    Bool,
    // this is the kind of a generic function
    GenericFn {
        args: Vec<KindExpr>,
        returnkind: Box<KindExpr>,
    },
}

#[derive(Clone, Debug)]
pub enum TypeExpr {
    // An error when parsing
    Error,
    Identifier(String),
    // types
    UnitTy,
    ArrayTy,
    SliceTy,
    IntTy,
    UIntTy,
    FloatTy,
    BoolTy,
    // const literals
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    // unary ops (syntax sugar)
    Ref(Box<TypeExpr>),
    // struct and enumify
    Struct(Vec<(String, TypeExpr)>),
    Enum(Vec<(String, TypeExpr)>),
    Union(Vec<(String, TypeExpr)>),
    // generic function
    Generic {
        fun: Box<TypeExpr>,
        args: Vec<TypeExpr>,
    },
    // type of a function
    Fn {
        args: Vec<TypeExpr>,
        returntype: Box<TypeExpr>,
    },
}