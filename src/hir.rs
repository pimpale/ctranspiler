use lsp_types::Range;
use num_bigint::BigInt;
use num_rational::BigRational;

#[derive(Clone, Debug, PartialEq)]
pub struct Augmented<T> {
    pub range: Range,
    pub val: T,
}

#[derive(Clone, Debug)]
pub enum HirPhase {
    NameResolved,
    KindChecked,
    TypeChecked,
    TempsGenerated,
    BorrowChecking,
    Monomorphization,
}

#[derive(Clone, Debug, PartialEq)]
pub enum KindExpr {
    Error,
    Int,
    Float,
    Bool,
    Type,
    Constructor {
        paramkinds: Vec<Augmented<KindExpr>>,
        returnkind: Box<Augmented<KindExpr>>,
    },
}

impl std::default::Default for KindExpr {
    fn default() -> Self {
        KindExpr::Error
    }
}

#[derive(Clone, Debug)]
pub enum TypeExpr {
    // An error when parsing
    Error,
    Identifier(usize),
    // types
    BoolTy,
    RefConstructorTy,
    ArrayConstructorTy,
    SliceConstructorTy,
    IntConstructorTy,
    UIntConstructorTy,
    FloatConstructorTy,
    // const literals
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    // type of a function
    Fn {
        paramtys: Vec<Augmented<TypeExpr>>,
        returnty: Box<Augmented<TypeExpr>>,
    },
    // struct and enum
    Struct(Vec<(Augmented<String>, Augmented<TypeExpr>)>),
    Enum(Vec<(Augmented<String>, Augmented<TypeExpr>)>),
    Union(Vec<(Augmented<String>, Augmented<TypeExpr>)>),
    // generic
    Concretization {
        genericty: Box<Augmented<TypeExpr>>,
        tyargs: Vec<Augmented<TypeExpr>>,
    },
    Generic {
        params: Vec<Augmented<TypePatExpr>>,
        returnkind: Option<Box<Augmented<KindExpr>>>,
        body: Box<Augmented<TypeExpr>>,
    },
}

impl std::default::Default for TypeExpr {
    fn default() -> Self {
        TypeExpr::Error
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypePatExpr {
    Error,
    Identifier(usize),
    Typed {
        id: usize,
        kind: Box<Augmented<KindExpr>>,
    },
}

impl std::default::Default for TypePatExpr {
    fn default() -> Self {
        TypePatExpr::Error
    }
}

#[derive(Clone, Debug)]
pub enum CaseTargetExpr {
    Error,
    Bool(bool),
    Int(BigInt),
    PatExpr(Box<Augmented<ValPatExpr>>),
}

impl std::default::Default for CaseTargetExpr {
    fn default() -> Self {
        CaseTargetExpr::Error
    }
}

#[derive(Clone, Debug)]
pub enum ValPatExpr {
    Error,
    Ignore,
    Identifier {
        mutable: bool,
        id: usize,
    },
    StructLiteral(Vec<(Augmented<String>, Augmented<ValPatExpr>)>),
    New {
        pat: Box<Augmented<ValPatExpr>>,
        ty: Box<Augmented<TypeExpr>>,
    },
    Typed {
        pat: Box<Augmented<ValPatExpr>>,
        ty: Box<Augmented<TypeExpr>>,
    },
}

impl std::default::Default for ValPatExpr {
    fn default() -> Self {
        ValPatExpr::Error
    }
}

#[derive(Clone, Debug)]
pub enum ValBinaryOpKind {
    // Math
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Lt,
    Leq,
    Gt,
    Geq,
    // Booleans
    And,
    Or,
    // Equality
    Eq,
    Neq,
}

#[derive(Clone, Debug)]
pub enum ValExpr {
    // An error when parsing
    Error,
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    String(Vec<u8>),
    Ref(Box<Augmented<ValExpr>>),
    Deref(Box<Augmented<ValExpr>>),
    // Generic (only used in functions atm)
    Generic {
        params: Vec<Augmented<TypePatExpr>>,
        // return kind is always TYPE
        body: Box<Augmented<ValExpr>>,
    },
    // Function
    FnDef {
        params: Vec<Augmented<ValPatExpr>>,
        returnty: Option<Box<Augmented<TypeExpr>>>,
        body: Box<Augmented<ValExpr>>,
    },
    // Constructs a new compound type
    StructLiteral(Vec<(Augmented<String>, Augmented<ValExpr>)>),
    // Creates a new instance of a nominal type
    New {
        ty: Box<Augmented<TypeExpr>>,
        val: Box<Augmented<ValExpr>>,
    },
    // Matches an expression to the first matching pattern and destructures it
    CaseOf {
        expr: Box<Augmented<ValExpr>>,
        cases: Vec<(Augmented<CaseTargetExpr>, Augmented<ValExpr>)>,
    },
    // Block
    Block {
        statements: Vec<Augmented<BlockStatement>>,
        last_expression: Option<Box<Augmented<ValExpr>>>,
    },
    // Inline array
    ArrayLiteral(Vec<Augmented<ValExpr>>),
    BinaryOp {
        op: ValBinaryOpKind,
        left_operand: Box<Augmented<ValExpr>>,
        right_operand: Box<Augmented<ValExpr>>,
    },
    // A reference to a previously defined variable
    Identifier(usize),
    // index into an array
    ArrayAccess {
        root: Box<Augmented<ValExpr>>,
        index: Box<Augmented<ValExpr>>,
    },
    // FieldAccess
    FieldAccess {
        root: Box<Augmented<ValExpr>>,
        field: String,
    },
    // Concretization
    Concretization {
        generic: Box<Augmented<ValExpr>>,
        tyargs: Vec<Augmented<TypeExpr>>,
    },
    // Function application
    App {
        fun: Box<Augmented<ValExpr>>,
        args: Vec<Augmented<ValExpr>>,
    },
}

impl std::default::Default for ValExpr {
    fn default() -> Self {
        ValExpr::Error
    }
}

#[derive(Clone, Debug)]
pub enum BlockStatement {
    Error,
    TypeDef {
        typat: Box<Augmented<TypePatExpr>>,
        value: Box<Augmented<TypeExpr>>,
    },
    ValDef {
        pat: Box<Augmented<ValPatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    Set {
        place: Box<Augmented<ValExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    IfThen {
        cond: Box<Augmented<ValExpr>>,
        then_branch: Vec<Augmented<BlockStatement>>,
        else_branch: Vec<Augmented<BlockStatement>>,
    },
    While {
        cond: Box<Augmented<ValExpr>>,
        body: Vec<Augmented<BlockStatement>>,
    },
    For {
        pattern: Box<Augmented<ValPatExpr>>,
        start: Box<Augmented<ValExpr>>,
        end: Box<Augmented<ValExpr>>,
        inclusive: bool,
        by: Option<Box<Augmented<ValExpr>>>,
        body: Vec<Augmented<BlockStatement>>,
    },
    Do(Box<Augmented<ValExpr>>),
}

#[derive(Clone, Debug)]
pub enum FileStatement {
    Error,
    TypeDef {
        typat: Box<Augmented<TypePatExpr>>,
        value: Box<Augmented<TypeExpr>>,
    },
    ValDef {
        pat: Box<Augmented<ValPatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
}

// TODO: add imports
#[derive(Clone, Debug)]
pub struct TranslationUnit {
    pub declarations: Vec<Augmented<FileStatement>>,
    pub phase: HirPhase,
}

// pub trait HirVisitor {
//     fn dfs_visit_translation_unit(&mut self, unit: &mut TranslationUnit) {
//         for decl in &mut unit.declarations {
//             self.visit_file_statement(decl);
//         }
//     }

//     fn dfs_visit_file_statement(&mut self, statement: &mut Augmented<FileStatement>) {
//         match &mut statement.val {
//             FileStatement::NoOp => {}
//             FileStatement::TypeDef { value, typat } => {
//                 self.visit_type_pat_expr(typat);
//                 self.visit_type_expr(value);
//             }
//             FileStatement::ValDef { pat, value } => {
//                 self.visit_pat_expr(pat);
//                 self.visit_val_expr(value);
//             }
//         }
//     }

//     fn dfs_visit_block_expr(&mut self, block: &mut Augmented<BlockExpr>) {
//         for statement in &mut block.val.statements {
//             self.visit_block_statement(statement);
//         }
//     }

//     fn dfs_visit_block_statement(&mut self, statement: &mut Augmented<BlockStatement>) {
//         match &mut statement.val {
//             BlockStatement::NoOp => {}
//             BlockStatement::TypeDef { value, typat } => {
//                 self.visit_type_pat_expr(typat);
//                 self.visit_type_expr(value);
//             }
//             BlockStatement::ValDef { pat, value } => {
//                 self.visit_pat_expr(pat);
//                 self.visit_val_expr(value);
//             }
//             BlockStatement::Set { place, value } => {
//                 self.visit_val_expr(place);
//                 self.visit_val_expr(value);
//             }
//             BlockStatement::While { cond, body } => {
//                 self.visit_val_expr(cond);
//                 self.visit_block_expr(body);
//             }
//             BlockStatement::For {
//                 pattern,
//                 start,
//                 end,
//                 inclusive: _,
//                 by,
//                 body,
//             } => {
//                 self.visit_pat_expr(pattern);
//                 self.visit_val_expr(start);
//                 self.visit_val_expr(end);
//                 if let Some(by) = by {
//                     self.visit_val_expr(by);
//                 }
//                 self.visit_block_expr(body);
//             }
//             BlockStatement::Do(expr) => {
//                 self.visit_val_expr(expr);
//             }
//         }
//     }

//     fn dfs_visit_kind_expr(&mut self, expr: &mut Augmented<KindExpr>) {
//         match &mut expr.val {
//             KindExpr::Error => {}
//             KindExpr::Bool => {}
//             KindExpr::Int => {}
//             KindExpr::Float => {}
//             KindExpr::Type => {}
//             KindExpr::Constructor {
//                 paramkinds,
//                 returnkind,
//             } => {
//                 for paramkind in paramkinds {
//                     self.visit_kind_expr(paramkind);
//                 }
//                 self.visit_kind_expr(returnkind);
//             }
//         }
//     }

//     fn visit_type_pat_expr(&mut self, expr: &mut Augmented<TypePatExpr>) {
//         match &mut expr.val {
//             TypePatExpr::Error => {}
//             TypePatExpr::Identifier(_) => {}
//             TypePatExpr::Typed { kind, .. } => {
//                 self.visit_kind_expr(kind);
//             }
//         }
//     }

//     fn visit_type_param_expr(&mut self, expr: &mut Augmented<TypeParamExpr>) {
//         match &mut expr.val {
//             TypeParamExpr::Error => {}
//             TypeParamExpr::Typed { kind, .. } => {
//                 self.visit_kind_expr(kind);
//             }
//         }
//     }

//     fn dfs_visit_type_expr(&mut self, expr: &mut Augmented<TypeExpr>) {
//         match &mut expr.val {
//             TypeExpr::Error => {}
//             TypeExpr::Identifier(_) => {}
//             TypeExpr::UnitTy => {}
//             TypeExpr::BoolTy => {}
//             TypeExpr::RefConstructorTy => {}
//             TypeExpr::UIntConstructorTy => {}
//             TypeExpr::IntConstructorTy => {}
//             TypeExpr::FloatConstructorTy => {}
//             TypeExpr::ArrayConstructorTy => {}
//             TypeExpr::SliceConstructorTy => {}
//             TypeExpr::Int(_) => {}
//             TypeExpr::Bool(_) => {}
//             TypeExpr::Float(_) => {}
//             TypeExpr::Struct(items) => {
//                 for (_, item) in items {
//                     self.visit_type_expr(item);
//                 }
//             }
//             TypeExpr::Enum(items) => {
//                 for (_, item) in items {
//                     self.visit_type_expr(item);
//                 }
//             }
//             TypeExpr::Union(items) => {
//                 for (_, item) in items {
//                     self.visit_type_expr(item);
//                 }
//             }
//             TypeExpr::Concretization { genericty, tyargs } => {
//                 self.visit_type_expr(genericty);
//                 for tyarg in tyargs {
//                     self.visit_type_expr(tyarg);
//                 }
//             }
//             TypeExpr::Fn { paramtys, returnty } => {
//                 for paramty in paramtys {
//                     self.visit_type_expr(paramty);
//                 }
//                 self.visit_type_expr(returnty);
//             }
//             TypeExpr::Generic {
//                 params,
//                 returnkind,
//                 body,
//             } => {
//                 for param in params {
//                     self.visit_type_param_expr(param);
//                 }
//                 self.visit_kind_expr(returnkind);
//                 self.visit_type_expr(body);
//             }
//         }
//     }

//     fn dfs_visit_pat_expr(&mut self, expr: &mut Augmented<PatExpr>) {
//         match &mut expr.val {
//             PatExpr::Error => {}
//             PatExpr::Ignore => {}
//             PatExpr::Identifier { .. } => {}
//             PatExpr::StructLiteral { ty, fields } => {
//                 self.visit_type_expr(ty);
//                 for (_, item) in fields {
//                     self.visit_pat_expr(item);
//                 }
//             }
//             PatExpr::Typed { pat, ty } => {
//                 self.visit_pat_expr(pat);
//                 self.visit_type_expr(ty);
//             }
//         }
//     }

//     fn visit_case_target_expr(&mut self, expr: &mut Augmented<CaseTargetExpr>) {
//         match &mut expr.val {
//             CaseTargetExpr::Error => {}
//             CaseTargetExpr::Unit => {}
//             CaseTargetExpr::Bool(_) => {}
//             CaseTargetExpr::Int(_) => {}
//             CaseTargetExpr::PatExpr(pat) => {
//                 self.visit_pat_expr(pat);
//             }
//         }
//     }

//     fn visit_case_expr(&mut self, expr: &mut Augmented<CaseExpr>) {
//         self.visit_case_target_expr(&mut expr.val.target);
//         self.visit_val_expr(&mut expr.val.body)
//     }

//     fn dfs_visit_else_expr(&mut self, expr: &mut Augmented<ElseExpr>) {
//         match &mut expr.val {
//             ElseExpr::Error => {}
//             ElseExpr::Elif {
//                 cond,
//                 then_branch,
//                 else_branch,
//             } => {
//                 self.visit_val_expr(cond);
//                 self.visit_block_expr(then_branch);
//                 if let Some(else_branch) = else_branch {
//                     self.visit_else_expr(else_branch);
//                 }
//             }
//             ElseExpr::Else(expr) => {
//                 self.visit_block_expr(expr);
//             }
//         }
//     }

//     fn dfs_visit_val_expr(&mut self, expr: &mut Augmented<ValExpr>) {
//         match &mut expr.val {
//             ValExpr::Error => {}
//             ValExpr::Unit => {}
//             ValExpr::Int(_) => {}
//             ValExpr::Bool(_) => {}
//             ValExpr::Float(_) => {}
//             ValExpr::String(_) => {}
//             ValExpr::Ref(expr) => {
//                 self.visit_val_expr(expr);
//             }
//             ValExpr::Deref(expr) => {
//                 self.visit_val_expr(expr);
//             }
//             ValExpr::StructLiteral(items) => {
//                 for (_, item) in items {
//                     self.visit_val_expr(item);
//                 }
//             }
//             ValExpr::BinaryOp {
//                 op: _,
//                 left_operand,
//                 right_operand,
//             } => {
//                 self.visit_val_expr(left_operand);
//                 self.visit_val_expr(right_operand);
//             }
//             ValExpr::IfThen {
//                 cond,
//                 then_branch,
//                 else_branch,
//             } => {
//                 self.visit_val_expr(cond);
//                 self.visit_block_expr(then_branch);
//                 if let Some(else_branch) = else_branch {
//                     self.visit_else_expr(else_branch);
//                 }
//             }
//             ValExpr::CaseOf {
//                 expr,
//                 first_case,
//                 rest_cases,
//             } => {
//                 self.visit_val_expr(expr);
//                 self.visit_case_expr(first_case);
//                 for case in rest_cases {
//                     self.visit_case_expr(case);
//                 }
//             }
//             ValExpr::Block(expr) => {
//                 self.visit_block_expr(expr);
//             }
//             ValExpr::ArrayLiteral(items) => {
//                 for item in items {
//                     self.visit_val_expr(item);
//                 }
//             }
//             ValExpr::Identifier(_) => {}
//             ValExpr::ArrayAccess { root, index } => {
//                 self.visit_val_expr(root);
//                 self.visit_val_expr(index);
//             }
//             ValExpr::FieldAccess { root, field: _ } => {
//                 self.visit_val_expr(root);
//             }
//             ValExpr::Concretization { generic, tyargs } => {
//                 self.visit_val_expr(generic);
//                 for tyarg in tyargs {
//                     self.visit_type_expr(tyarg);
//                 }
//             }
//             ValExpr::App { fun, args } => {
//                 self.visit_val_expr(fun);
//                 for arg in args {
//                     self.visit_val_expr(arg);
//                 }
//             }
//             ValExpr::FnDef {
//                 typarams,
//                 params,
//                 returnty,
//                 body,
//             } => {
//                 for typaram in typarams {
//                     self.visit_type_param_expr(typaram);
//                 }
//                 for param in params {
//                     self.visit_param_expr(param);
//                 }
//                 self.visit_type_expr(returnty);
//                 self.visit_val_expr(body);
//             }
//         }
//     }

//     fn visit_translation_unit(&mut self, tu: &mut TranslationUnit) {
//         self.dfs_visit_translation_unit(tu);
//     }
//     fn visit_kind_expr(&mut self, expr: &mut Augmented<KindExpr>) {
//         self.dfs_visit_kind_expr(expr);
//     }
//     fn visit_val_expr(&mut self, expr: &mut Augmented<ValExpr>) {
//         self.dfs_visit_val_expr(expr);
//     }
//     fn visit_pat_expr(&mut self, expr: &mut Augmented<PatExpr>) {
//         self.dfs_visit_pat_expr(expr);
//     }
//     fn visit_type_expr(&mut self, expr: &mut Augmented<TypeExpr>) {
//         self.dfs_visit_type_expr(expr);
//     }
//     fn visit_block_expr(&mut self, expr: &mut Augmented<BlockExpr>) {
//         self.dfs_visit_block_expr(expr);
//     }
//     fn visit_else_expr(&mut self, expr: &mut Augmented<ElseExpr>) {
//         self.dfs_visit_else_expr(expr);
//     }
//     fn visit_file_statement(&mut self, statement: &mut Augmented<FileStatement>) {
//         self.dfs_visit_file_statement(statement)
//     }
//     fn visit_block_statement(&mut self, statement: &mut Augmented<BlockStatement>) {
//         self.dfs_visit_block_statement(statement)
//     }
// }
