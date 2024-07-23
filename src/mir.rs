use crate::{builtin, values};

// Function-Local
pub struct BasicBlockIdx(usize);
pub struct StackVariableIdx(usize);
// Module-Local
pub struct GlobalConstantIdx(usize);

pub enum Place {
    StackVariableIdx(StackVariableIdx),
    GlobalConstantIdx(GlobalConstantIdx),
}

pub enum Statement {
    /// pushes a local onto the stack
    DeclareLocal { value: RValue },
    /// Mutates a place
    Mutate { target: Place, value: RValue },
}

pub enum RValueKind {
    Builtin(builtin::Builtin),
    Use(Place),
    GetRef { mutable: bool, place: Place },
}

pub struct RValue {
    kind: RValueKind,
}

pub struct Local {
    ty: values::Value,
    mutable: bool,
}

pub enum Terminator {
    // switch
    Switches {
        place: Place,
        cases: (GlobalConstantIdx, BasicBlockIdx),
    },
    // leaves the program
    Exit,
    // Calls another function
    Call {
        // the function to call
        function: RValue,
        // the arguments provided to the call
        args: Vec<RValue>,
        // the call will write its return value in this place
        write_result: Place,
        // execution will continue from this point
        dest: BasicBlockIdx,
    },
}

pub struct BasicBlock {
    statements: Vec<Statement>,
    terminator: Terminator,
}

pub struct MirFunc {
    arg_count: usize,
    locals: Vec<Local>,
    basic_blocks: Vec<BasicBlock>,
    root: BasicBlockIdx,
}

struct Global {
    // type
    ty: values::Value,
    // representation of the data in bytes
    data: Vec<u8>,
}

pub struct Environment {
    constants: Vec<MirFunc>,
    functions: Vec<Global>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            constants: Vec::new(),
            functions: Vec::new(),
        }
    }
}