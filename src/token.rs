use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;
use strum::AsRefStr;

#[derive(Debug, Clone, AsRefStr, PartialEq)]
pub enum TokenKind {
    // function, type, or variable
    Identifier(String),
    // Keywords
    Case,     // case
    Valdef,   // val
    Typedef,  // type
    Mut,      // mut
    Set,      // set
    In,       // in
    Fn,       // fn
    If,       // if
    Else,     // else
    By,       // by
    While,    // while
    For,      // for
    Namespace,// namespace
    Use,      // use
    New,      // new
    Struct,   // struct
    Enum,     // enum
    Union,    // union
    Block,    // block
    Generic,  // generic
    // Values
    Lifetime(Vec<u8>),                      // 'lifetime
    Inf,                                    // inf
    Nan,                                    // nan
    Bool(bool),                             // true, false
    String { value: Vec<u8>, block: bool }, // "string"
    Int(BigInt),                            // 7
    Float(BigRational),                     // 0.7
    // Types
    NeverTy, // !
    ArrayTy, // Array
    SliceTy, // Slice
    IntTy,   // Int
    FloatTy, // Float
    BoolTy,  // Bool
    // Kinds
    TypeKind,  // TYPE
    BoolKind,  // BOOL
    IntKind,   // INT
    FloatKind, // FLOAT
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
    Assign,         // =
    // Reference
    Ref,   // &
    Deref, // @
    // Splat
    Splat, // **
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
