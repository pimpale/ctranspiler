use serde::{Deserialize, Serialize};
use strum::EnumIter;

#[derive(Clone, Debug, PartialEq, EnumIter, Serialize, Deserialize)]
pub enum Builtin {
    // for type in particular the universe is level + 2
    Type,
    // for constructors the universe is level + 1
    Ref,
    Array,
    Slice,
    Nat,
    Int,
    Float,
    // Operator support
    AddTrait,
    SubTrait,
    MulTrait,
    DivTrait,
    RemTrait,
    AddAssignTrait,
    SubAssignTrait,
    MulAssignTrait,
    DivAssignTrait,
    RemAssignTrait,
    // for the following, the universe is just the level
    // integer operators
    IntNeg, // [U]i -> i
    IntAdd, // [U](i, i) -> i
    IntSub, // [U](i, i) -> i
    IntMul, // [U](i, i) -> i
    IntDiv, // [U](i, i) -> i
    IntRem, // [U](i, i) -> i
    // nat operators
    NatAdd,  // [U](u, u) -> u
    NatSub,  // [U](u, u) -> u
    NatMul,  // [U](u, u) -> u
    NatDiv,  // [U](u, u) -> u
    NatRem,  // [U](u, u) -> u
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
            Builtin::Array => write!(f, "@Array"),
            Builtin::Slice => write!(f, "@Slice"),
            Builtin::Nat => write!(f, "@Nat"),
            Builtin::Int => write!(f, "@Int"),
            Builtin::Float => write!(f, "@Float"),
            Builtin::AddTrait => write!(f, "@Add"),
            Builtin::SubTrait => write!(f, "@Sub"),
            Builtin::MulTrait => write!(f, "@Mul"),
            Builtin::DivTrait => write!(f, "@Div"),
            Builtin::RemTrait => write!(f, "@Rem"),
            Builtin::AddAssignTrait => write!(f, "@AddAssign"),
            Builtin::SubAssignTrait => write!(f, "@SubAssign"),
            Builtin::MulAssignTrait => write!(f, "@MulAssign"),
            Builtin::DivAssignTrait => write!(f, "@DivAssign"),
            Builtin::RemAssignTrait => write!(f, "@RemAssign"),
            Builtin::IntNeg => write!(f, "@int_neg"),
            Builtin::IntAdd => write!(f, "@int_add"),
            Builtin::IntSub => write!(f, "@int_sub"),
            Builtin::IntMul => write!(f, "@int_mul"),
            Builtin::IntDiv => write!(f, "@int_div"),
            Builtin::IntRem => write!(f, "@int_rem"),
            Builtin::NatAdd => write!(f, "@nat_add"),
            Builtin::NatSub => write!(f, "@nat_sub"),
            Builtin::NatMul => write!(f, "@nat_mul"),
            Builtin::NatDiv => write!(f, "@nat_div"),
            Builtin::NatRem => write!(f, "@nat_rem"),
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
