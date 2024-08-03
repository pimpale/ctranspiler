use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use strum::AsRefStr;

use crate::builtin::{self, Builtin};

#[derive(Debug, Clone, AsRefStr, PartialEq)]
pub enum TokenKind {
    // function, type, or variable
    Identifier(String),
    // Keywords
    Case,      // case
    Let,       // let
    Rec,       // rec
    Mut,       // mut
    In,        // in
    Fn,        // fn
    Loop,      // loop
    Ret,       // ret
    Namespace, // namespace
    Use,       // use
    New,       // new
    Struct,    // struct
    Enum,      // enum
    Union,     // union
    Nominal,   // nominal
    Extern,    // extern
    // Builtins
    Builtin(Builtin), // builtin
    // Values
    Label(String),                          // 'label
    String { value: Vec<u8>, block: bool }, // "string"
    Int(BigInt),                            // 7
    Float(BigRational),                     // 0.7
    // Copy Operator
    Copy,     // '
    Reborrow, // ~
    // Math Operators
    Plus,  // +
    Minus, // -
    Mul,   // *
    Div,   // /
    Rem,   // %
    // Boolean Operators
    Not, // not
    And, // and
    Or,  // or
    // Type operators
    Comma, // ,
    // Range Operators
    Range,          // ..
    RangeInclusive, // ..=
    // Comparison and Equality
    Equal,        // =
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
    // Assignment
    Assign,    // :=
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    // Reference
    Ref,    // &
    Mutref, // !
    Deref,  // @
    // Arrows
    Pipe,    // |
    Defun,   // =>
    ToArrow, // ->
    // Other Miscellaneous Operator Things
    ParenLeft,      // (
    ParenRight,     // )
    BracketLeft,    // [
    BracketRight,   // ]
    OpenStructLeft, // .{
    BraceLeft,      // {
    BraceRight,     // }
    AscribeType,    // :
    AscribeTypeRev, // ::
    ModuleAccess,   // .
    Semicolon,      // ;
    Ignore,         // _
    // Comments, and Attributes
    Metadata { value: Vec<u8>, significant: bool }, // #!attribute and # comment
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: Option<TokenKind>,
    pub range: Range,
}

impl Token {
    pub fn new(kind: TokenKind, range: Range) -> Self {
        Token {
            kind: Some(kind),
            range,
        }
    }
}
