
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
    Builtin(Builtin),
    Use(Place),
    GetRef { mutable: bool, place: Place },
}

pub struct RValue {
    kind: RValueKind,
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
        arg: Vec<RValue>,
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
    root: BasicBlock,
}

pub enum MirModuleEntry {
    Constant { toeval: BasicBlock },
    Function(MirFunc),
}

#[derive(Debug)]
pub enum Builtin {
    // builtin operators
    // boolean operators
    BoolNot, // bool -> bool
    // integer operators
    IntAddGen,         // [U](u, u) -> u
    IntSubGen,         // [U](u, u) -> u
    IntMulGen,         // [U](u, u) -> u
    IntDivGen,         // [U](u, u) -> u
    IntRemGen,         // [U](u, u) -> u
    IntShlLGen,        // [U](u, u) -> u
    IntShrLGen,        // [U](u, u) -> u
    IntShrAGen,        // [U](u, u) -> u
    IntRolGen,         // [U](u, u) -> u
    IntRorGen,         // [U](u, u) -> u
    IntAndGen,         // [U](u, u) -> u
    IntOrGen,          // [U](u, u) -> u
    IntXorGen,         // [U](u, u) -> u
    IntInvGen,         // [U]u -> u
    IntNegGen,         // [U]u -> u
    // float operators
    FloatAddGen,    // [F](f, f) -> f
    FloatSubGen,    // [F](f, f) -> f
    FloatMulGen,    // [F](f, f) -> f
    FloatDivGen,    // [F](f, f) -> f
    FloatRemGen,    // [F](f, f) -> f
    FloatNegGen,    // [F]f -> f
    // conversion operators
    // convert int to int
    ConvIntIntGen, // [T, U] t -> u
    // convert float to float
    ConvFloatFloatGen, // [T, U] f -> f
    // convert int to float
    ConvIntFloatGen, // [T, U] u -> f
    // convert float to int
    ConvFloatIntGen, // [T, U] f -> u
}
