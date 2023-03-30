use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use strum::AsRefStr;

#[derive(Debug, Clone, AsRefStr, PartialEq)]
pub enum TokenKind {
    // function, type, or variable
    Identifier(Vec<u8>),
    // Keywords
    Case,   // case
    Of,     // of
    Val,    // val
    Struct, // struct
    Enum,   // enum
    Const,  // const
    Let,    // let
    Mut,    // mut
    Set,    // set
    In,     // in
    By,     // by
    Fn,     // fn
    While,  // while
    For,    // for
    Block,    // block
    Type,   // type
    // Literals and constants
    Lifetime(Vec<u8>),                      // 'lifetime
    Inf,                                    // inf
    Nan,                                    // nan
    Bool(bool),                             // true, false
    String { value: Vec<u8>, block: bool }, // "string"
    Int(BigInt),                            // 7
    Float(BigRational),                     // 0.7
    LifetimeTy,                             // lifetime
    Unit,                                   // ()
    // Math Operators
    Plus,  // +
    Minus, // -
    Mul,   // *
    Div,   // /
    Rem,   // %
    // Boolean Operators
    And, // and
    Or,  // or
    // Type operators
    Comma, // ,
    // Range Operators
    Range,          // ..
    RangeInclusive, // ..=
    // Comparison and Equality
    Equal,        // ==
    NotEqual,     // !=
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
    // Assignment
    Assign, // =
    // Reference
    Ref,   // &
    Deref, // @
    // Arrows
    Pipe,  // |
    Defun, // ->
    // Other Miscellaneous Operator Things
    ParenLeft,    // (
    ParenRight,   // )
    BracketLeft,  // [
    BracketRight, // ]
    BraceLeft,    // {
    BraceRight,   // }
    Constrain,    // :
    ModuleAccess, // .
    Semicolon,    // ;
    Ignore,       // _
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
