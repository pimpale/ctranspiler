use std::collections::HashMap;


#[derive(Clone, Debug, PartialEq)]
pub enum Hint {
    Unknown,
    Multi(Vec<Hint>),
    SymbolicVariable {
        id: usize,
        ty: Box<Hint>,
    },
    // values that inhabit a type
    Ref {
        ty: Box<Hint>,
        var: *mut Hint,
    },
    Array {
        inner_ty: Box<Hint>,
        values: Vec<Hint>,
    },
    Slice {
        inner_ty: Box<Hint>,
        length: usize,
        values: *mut Hint,
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
        param_tys: Vec<Hint>,
        // function mapping the parameters to a result type
        dep_ty: Box<Closure>,
    },
    Lam {
        ty: Box<Hint>,
        body: Box<Closure>,
    },
    // a pointer to a function
    FnPtr {
        ty: Box<Hint>,
    },
    // a thunk
    Thunk {
        result_type: Box<Hint>,
        lam: Box<Hint>,
        args: Vec<Hint>,
    },
    // compound types
    StructTy {
        // universe is level + 1
        level: usize,
        fields: HashMap<String, Hint>,
    },
    EnumTy {
        // universe is level + 1
        level: usize,
        fields: HashMap<String, Hint>,
    },
    UnionTy {
        // universe is level + 1
        level: usize,
        fields: HashMap<String, Hint>,
    },
    // values that inhabit a compound type
    Struct {
        ty: Box<Hint>,
        fields: HashMap<String, Hint>,
    },
    Enum {
        ty: Box<Hint>,
        variant: Box<Hint>,
    },
    Union {
        ty: Box<Hint>,
        variant: Box<Hint>,
    },
    // BUILTINS
    Builtin(Builtin, usize),
}
