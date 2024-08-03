use serde::{Deserialize, Serialize};
use strum::EnumIter;

#[derive(Clone, Debug, PartialEq, EnumIter, Serialize, Deserialize)]
pub enum Builtin {
    // for type in particular the universe is level + 2
    Type,
    // for constructors the universe is level + 1
    Ref,
    MutRef,
    Array,
    Slice,
    Bool,
    Nat,
    Int,
    Float,
    // Operator support
    AddTrait,
    SubTrait,
    MulTrait,
    DivTrait,
    RemTrait,
    EqTrait,
    LtTrait,
    LteTrait,
    GtTrait,
    GteTrait,
    AddAssignTrait,
    SubAssignTrait,
    MulAssignTrait,
    DivAssignTrait,
    RemAssignTrait,
    IndexTrait,
    // for the following, the universe is just the level
    // Mutability intrinsics
    Assign,
    Copy,
    Reborrow,
    Readonly,
    // integer operators
    IntNeg, // [U]i -> i
    IntAdd, // [U](i, i) -> i
    IntSub, // [U](i, i) -> i
    IntMul, // [U](i, i) -> i
    IntDiv, // [U](i, i) -> i
    IntRem, // [U](i, i) -> i
    IntLt,  // [U](i, i) -> bool
    IntLte, // [U](i, i) -> bool
    IntGt,  // [U](i, i) -> bool
    IntGte, // [U](i, i) -> bool
    IntEq,  // [U](i, i) -> bool
    // nat operators
    NatAdd,  // [U](u, u) -> u
    NatSub,  // [U](u, u) -> u
    NatMul,  // [U](u, u) -> u
    NatDiv,  // [U](u, u) -> u
    NatRem,  // [U](u, u) -> u
    NatLt,   // [U](u, u) -> bool
    NatLte,  // [U](u, u) -> bool
    NatGt,   // [U](u, u) -> bool
    NatGte,  // [U](u, u) -> bool
    NatEq,   // [U](u, u) -> bool
    NatShlL, // [U](u, u) -> u
    NatShrL, // [U](u, u) -> u
    NatShrA, // [U](u, u) -> u
    NatRol,  // [U](u, u) -> u
    NatRor,  // [U](u, u) -> u
    NatAnd,  // [U](u, u) -> u
    NatOr,   // [U](u, u) -> u
    NatXor,  // [U](u, u) -> u
    NatInv,  // [U]u -> u
    // float operators
    FloatAdd, // [F](f, f) -> f
    FloatSub, // [F](f, f) -> f
    FloatMul, // [F](f, f) -> f
    FloatDiv, // [F](f, f) -> f
    FloatRem, // [F](f, f) -> f
    FloatNeg, // [F]f -> f
    FloatLt,  // [F](f, f) -> bool
    FloatLte, // [F](f, f) -> bool
    FloatGt,  // [F](f, f) -> bool
    FloatGte, // [F](f, f) -> bool
    FloatEq,  // [F](f, f) -> bool
    // array operators
    ArrayIndex,
    ArrayIndexMut,
    // slice operators
    SliceFromArray,
    SliceIndex,
    SliceIndexMut,
    // conversion operators
    // convert int to int
    ConvNatNat, // [T, U] t -> u
    ConvIntInt, // [T, U] t -> u
    ConvNatInt, // [T, U] t -> u
    ConvIntNat, // [T, U] t -> u
    // convert float to float
    ConvFloatFloat, // [T, U] f -> f
    // convert int to float
    ConvIntFloat, // [T, U] u -> f
    // convert float to int
    ConvFloatInt, // [T, U] f -> u
}

impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Builtin::Type => write!(f, "@Type"),
            Builtin::Ref => write!(f, "@Ref"),
            Builtin::MutRef => write!(f, "@MutRef"),
            Builtin::Array => write!(f, "@Array"),
            Builtin::Slice => write!(f, "@Slice"),
            Builtin::Bool => write!(f, "@Bool"),
            Builtin::Nat => write!(f, "@Nat"),
            Builtin::Int => write!(f, "@Int"),
            Builtin::Float => write!(f, "@Float"),
            Builtin::AddTrait => write!(f, "@Add"),
            Builtin::SubTrait => write!(f, "@Sub"),
            Builtin::MulTrait => write!(f, "@Mul"),
            Builtin::DivTrait => write!(f, "@Div"),
            Builtin::RemTrait => write!(f, "@Rem"),
            Builtin::EqTrait => write!(f, "@Eq"),
            Builtin::LtTrait => write!(f, "@Lt"),
            Builtin::LteTrait => write!(f, "@Lte"),
            Builtin::GtTrait => write!(f, "@Gt"),
            Builtin::GteTrait => write!(f, "@Gte"),
            Builtin::AddAssignTrait => write!(f, "@AddAssign"),
            Builtin::SubAssignTrait => write!(f, "@SubAssign"),
            Builtin::MulAssignTrait => write!(f, "@MulAssign"),
            Builtin::DivAssignTrait => write!(f, "@DivAssign"),
            Builtin::RemAssignTrait => write!(f, "@RemAssign"),
            Builtin::IndexTrait => write!(f, "@Index"),
            Builtin::Assign => write!(f, "@assign"),
            Builtin::Copy => write!(f, "@copy"),
            Builtin::Reborrow => write!(f, "@reborrow"),
            Builtin::Readonly => write!(f, "@readonly"),
            Builtin::IntNeg => write!(f, "@int_neg"),
            Builtin::IntAdd => write!(f, "@int_add"),
            Builtin::IntSub => write!(f, "@int_sub"),
            Builtin::IntMul => write!(f, "@int_mul"),
            Builtin::IntDiv => write!(f, "@int_div"),
            Builtin::IntRem => write!(f, "@int_rem"),
            Builtin::IntLt => write!(f, "@int_lt"),
            Builtin::IntLte => write!(f, "@int_lte"),
            Builtin::IntGt => write!(f, "@int_gt"),
            Builtin::IntGte => write!(f, "@int_gte"),
            Builtin::IntEq => write!(f, "@int_eq"),
            Builtin::NatAdd => write!(f, "@nat_add"),
            Builtin::NatSub => write!(f, "@nat_sub"),
            Builtin::NatMul => write!(f, "@nat_mul"),
            Builtin::NatDiv => write!(f, "@nat_div"),
            Builtin::NatRem => write!(f, "@nat_rem"),
            Builtin::NatLt => write!(f, "@nat_lt"),
            Builtin::NatLte => write!(f, "@nat_lte"),
            Builtin::NatGt => write!(f, "@nat_gt"),
            Builtin::NatGte => write!(f, "@nat_gte"),
            Builtin::NatEq => write!(f, "@nat_eq"),
            Builtin::NatShlL => write!(f, "@nat_shll"),
            Builtin::NatShrL => write!(f, "@nat_shrl"),
            Builtin::NatShrA => write!(f, "@nat_shra"),
            Builtin::NatRol => write!(f, "@nat_rol"),
            Builtin::NatRor => write!(f, "@nat_ror"),
            Builtin::NatAnd => write!(f, "@nat_and"),
            Builtin::NatOr => write!(f, "@nat_or"),
            Builtin::NatXor => write!(f, "@nat_xor"),
            Builtin::NatInv => write!(f, "@nat_inv"),
            Builtin::FloatAdd => write!(f, "@float_add"),
            Builtin::FloatSub => write!(f, "@float_sub"),
            Builtin::FloatMul => write!(f, "@float_mul"),
            Builtin::FloatDiv => write!(f, "@float_div"),
            Builtin::FloatRem => write!(f, "@float_rem"),
            Builtin::FloatNeg => write!(f, "@float_neg"),
            Builtin::FloatLt => write!(f, "@float_lt"),
            Builtin::FloatLte => write!(f, "@float_lte"),
            Builtin::FloatGt => write!(f, "@float_gt"),
            Builtin::FloatGte => write!(f, "@float_gte"),
            Builtin::FloatEq => write!(f, "@float_eq"),
            Builtin::ArrayIndex => write!(f, "@array_index"),
            Builtin::ArrayIndexMut => write!(f, "@array_index_mut"),
            Builtin::SliceFromArray => write!(f, "@slice_from_array"),
            Builtin::SliceIndex => write!(f, "@slice_index"),
            Builtin::SliceIndexMut => write!(f, "@slice_index_mut"),
            Builtin::ConvNatNat => write!(f, "@nat_to_nat"),
            Builtin::ConvIntInt => write!(f, "@int_to_int"),
            Builtin::ConvNatInt => write!(f, "@nat_to_int"),
            Builtin::ConvIntNat => write!(f, "@int_to_nat"),
            Builtin::ConvFloatFloat => write!(f, "@float_to_float"),
            Builtin::ConvIntFloat => write!(f, "@int_to_float"),
            Builtin::ConvFloatInt => write!(f, "@float_to_int"),
        }
    }
}
