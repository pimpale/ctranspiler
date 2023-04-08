use indexmap::IndexMap;
use num_bigint::BigInt;
use num_rational::BigRational;

use crate::ast::Augmented;

#[derive(Clone, Debug)]
pub enum HirPhase {
    Raw,
    Desugared,
    NameResolved,
    TypeChecked,
    TempsGenerated,
    BorrowChecking,
    Monomorphization,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum TypeExpr {
    // An error when parsing
    Error,
    Identifier(String),
    Identifier2(usize),
    // types
    UnitTy,
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
        tyreturn: Box<Augmented<TypeExpr>>,
    },
    // struct and enumify
    Struct(IndexMap<String, Augmented<TypeExpr>>),
    Enum(IndexMap<String, Augmented<TypeExpr>>),
    Union(IndexMap<String, Augmented<TypeExpr>>),
    // generic
    Concretization {
        genericty: Box<Augmented<TypeExpr>>,
        tyargs: Vec<Augmented<TypeExpr>>,
    },
}

#[derive(Clone, Debug)]
pub enum TypePatExpr {
    Error,
    Identifier {
        identifier: String,
        kind: Box<Augmented<KindExpr>>,
    },
    Identifier2 {
        identifier: usize,
        kind: Box<Augmented<KindExpr>>,
    },
}

#[derive(Clone, Debug)]
pub enum ValBinaryOpKind {
    // Not permitted after SyntaxDesugared
    Pipe,
    // Math
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Booleans
    And,
    Or,
    // Comparison
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
}

#[derive(Clone, Debug)]
pub enum CaseTargetExpr {
    Error,
    Unit,
    Bool(bool),
    Int(BigInt),
    PatExpr(Box<Augmented<PatExpr>>),
}

#[derive(Clone, Debug)]
pub struct CaseExpr {
    pub target: Box<Augmented<CaseTargetExpr>>,
    pub body: Box<Augmented<ValExpr>>,
}

#[derive(Clone, Debug)]
pub enum PatExpr {
    Error,
    Ignore,
    Identifier {
        mutable: bool,
        identifier: String,
    },
    Identifier2(usize),
    StructLiteral(IndexMap<String, Augmented<PatExpr>>),
    Typed {
        pat: Box<Augmented<PatExpr>>,
        ty: Box<Augmented<TypeExpr>>,
    },
}

#[derive(Clone, Debug)]
pub enum ElseExpr {
    // An error when parsing
    Error,
    Else(Box<Augmented<BlockExpr>>),
    Elif {
        cond: Box<Augmented<ValExpr>>,
        then_branch: Box<Augmented<BlockExpr>>,
        else_branch: Option<Box<Augmented<ElseExpr>>>,
    },
}

#[derive(Clone, Debug)]
pub enum ValExpr {
    // An error when parsing
    Error,
    Unit,
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    String(Vec<u8>),
    Ref(Box<Augmented<ValExpr>>),
    Deref(Box<Augmented<ValExpr>>),
    // Constructs a new compound type
    StructLiteral(IndexMap<String, Augmented<ValExpr>>),
    // Binary operation
    BinaryOp {
        op: ValBinaryOpKind,
        left_operand: Box<Augmented<ValExpr>>,
        right_operand: Box<Augmented<ValExpr>>,
    },
    IfThen {
        cond: Box<Augmented<ValExpr>>,
        then_branch: Box<Augmented<BlockExpr>>,
        else_branch: Option<Box<Augmented<ElseExpr>>>,
    },
    // Matches an expression to the first matching pattern and destructures it
    CaseOf {
        expr: Box<Augmented<ValExpr>>,
        cases: Vec<Augmented<CaseExpr>>,
    },
    // Block
    Block(Box<Augmented<BlockExpr>>),
    // Inline array
    ArrayLiteral(Vec<Augmented<ValExpr>>),
    // A reference to a previously defined variable
    Identifier(String),
    Identifier2(usize),
    // Lambda function
    FnLiteral {
        params: Vec<Augmented<PatExpr>>,
        returnty: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<BlockExpr>>,
    },
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

#[derive(Clone, Debug)]
pub struct BlockExpr {
    pub statements: Vec<Augmented<BlockStatement>>,
    pub trailing_semicolon: bool,
}

#[derive(Clone, Debug)]
pub enum BlockStatement {
    NoOp,
    TypeDef {
        typarams: Vec<Augmented<TypePatExpr>>,
        typat: Box<Augmented<TypePatExpr>>,
        value: Box<Augmented<TypeExpr>>,
    },
    Use {
        prefix: String,
    },
    Let {
        typarams: Vec<Augmented<TypePatExpr>>,
        pat: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    Set {
        place: Box<Augmented<ValExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    While {
        cond: Box<Augmented<ValExpr>>,
        body: Box<Augmented<BlockExpr>>,
    },
    For {
        pattern: Box<Augmented<PatExpr>>,
        start: Box<Augmented<ValExpr>>,
        end: Box<Augmented<ValExpr>>,
        inclusive: bool,
        by: Option<Box<Augmented<ValExpr>>>,
        body: Box<Augmented<BlockExpr>>,
    },
    Do(Box<Augmented<ValExpr>>),
}

#[derive(Clone, Debug)]
pub enum FileStatement {
    NoOp,
    TypeDef {
        typarams: Vec<Augmented<TypePatExpr>>,
        typat: Box<Augmented<TypePatExpr>>,
        value: Box<Augmented<TypeExpr>>,
    },
    Let {
        typarams: Vec<Augmented<TypePatExpr>>,
        pat: Box<Augmented<PatExpr>>,
        value: Box<Augmented<ValExpr>>,
    },
    Prefix {
        prefix: String,
        items: Vec<Augmented<FileStatement>>,
    },
    Use {
        prefix: String,
    },
}

// TODO: add imports
#[derive(Clone, Debug)]
pub struct TranslationUnit {
    pub declarations: Vec<Augmented<FileStatement>>,
    pub phase: HirPhase,
}

pub trait HirVisitor {
    fn dfs_visit_translation_unit(&mut self, unit: &mut TranslationUnit) {
        for decl in &unit.declarations {
            self.visit_file_statement(&mut decl);
        }
    }

    fn dfs_visit_file_statement(&mut self, statement: &mut Augmented<FileStatement>) {
        match statement.val {
            FileStatement::NoOp => {}
            FileStatement::TypeDef {
                typarams,
                value,
                typat,
            } => {
                for typaram in typarams {
                    self.visit_type_pat_expr(&mut typaram);
                }
                self.visit_type_pat_expr(&mut typat);
                self.visit_type_expr(&mut value);
            }
            FileStatement::Let {
                typarams,
                pat,
                value,
            } => {
                for typaram in typarams {
                    self.visit_type_pat_expr(&mut typaram);
                }
                self.visit_pat_expr(&mut pat);
                self.visit_val_expr(&mut value);
            }
            FileStatement::Prefix { prefix, items } => {
                for item in items {
                    self.visit_file_statement(&mut item);
                }
            }
            FileStatement::Use { prefix } => {}
        }
    }

    fn dfs_visit_block_expr(&mut self, block: &mut Augmented<BlockExpr>) {
        for statement in &block.val.statements {
            self.visit_block_statement(&mut statement);
        }
    }

    fn dfs_visit_block_statement(&mut self, statement: &mut Augmented<BlockStatement>) {
        match statement.val {
            BlockStatement::NoOp => {}
            BlockStatement::TypeDef {
                typarams,
                value,
                typat,
            } => {
                for typaram in typarams {
                    self.visit_type_pat_expr(&mut typaram);
                }
                self.visit_type_pat_expr(&mut typat);
                self.visit_type_expr(&mut value);
            }
            BlockStatement::Use { prefix } => {}
            BlockStatement::Let {
                typarams,
                pat,
                value,
            } => {
                for typaram in typarams {
                    self.visit_type_pat_expr(&mut typaram);
                }
                self.visit_pat_expr(&mut pat);
                self.visit_val_expr(&mut value);
            }
            BlockStatement::Set { place, value } => {
                self.visit_val_expr(&mut place);
                self.visit_val_expr(&mut value);
            }
            BlockStatement::While { cond, body } => {
                self.visit_val_expr(&mut cond);
                self.visit_block_expr(&mut body);
            }
            BlockStatement::For {
                pattern,
                start,
                end,
                inclusive,
                by,
                body,
            } => {
                self.visit_pat_expr(&mut pattern);
                self.visit_val_expr(&mut start);
                self.visit_val_expr(&mut end);
                if let Some(by) = by {
                    self.visit_val_expr(&mut by);
                }
                self.visit_block_expr(&mut body);
            }
            BlockStatement::Do(expr) => {
                self.visit_val_expr(&mut expr);
            }
        }
    }

    fn dfs_visit_kind_expr(&mut self, expr: &mut Augmented<KindExpr>) {
        match expr.val {
            KindExpr::Error => {}
            KindExpr::Bool => {}
            KindExpr::Int => {}
            KindExpr::Float => {}
            KindExpr::Constructor {
                paramkinds,
                returnkind,
            } => {
                for paramkind in paramkinds {
                    self.visit_kind_expr(&mut paramkind);
                }
                self.visit_kind_expr(&mut returnkind);
            }
        }
    }

    fn visit_type_pat_expr(&mut self, expr: &mut Augmented<TypePatExpr>) {
        match expr.val {
            TypePatExpr::Error => {}
            TypePatExpr::Identifier { kind, .. } => {
                self.visit_kind_expr(&mut kind);
            }
        }
    }

    fn dfs_visit_type_expr(&mut self, expr: &mut Augmented<TypeExpr>) {
        match expr.val {
            TypeExpr::Error => {}
            TypeExpr::Identifier(_) => {}
            TypeExpr::UnitTy => {}
            TypeExpr::BoolTy => {}
            TypeExpr::IntConstructorTy => {}
            TypeExpr::FloatConstructorTy => {}
            TypeExpr::ArrayConstructorTy => {}
            TypeExpr::SliceConstructorTy => {}
            TypeExpr::Int(_) => {}
            TypeExpr::Bool(_) => {}
            TypeExpr::Float(_) => {}
            TypeExpr::Struct(items) => {
                for (_, item) in items {
                    self.visit_type_expr(&mut item);
                }
            }
            TypeExpr::Enum(items) => {
                for (_, item) in items {
                    self.visit_type_expr(&mut item);
                }
            }
            TypeExpr::Union(items) => {
                for (_, item) in items {
                    self.visit_type_expr(&mut item);
                }
            }
            TypeExpr::Concretization { genericty, tyargs } => {
                self.visit_type_expr(&mut genericty);
                for tyarg in tyargs {
                    self.visit_type_expr(&mut tyarg);
                }
            }
            TypeExpr::Fn { paramtys, tyreturn } => {
                for paramty in paramtys {
                    self.visit_type_expr(&mut paramty);
                }
                self.visit_type_expr(&mut tyreturn);
            }
        }
    }

    fn dfs_visit_pat_expr(&mut self, expr: &mut Augmented<PatExpr>) {
        match expr.val {
            PatExpr::Error => {}
            PatExpr::Ignore => {}
            PatExpr::Identifier {
                mutable,
                identifier,
            } => {}
            PatExpr::StructLiteral(items) => {
                for (_, item) in items {
                    self.visit_pat_expr(&mut item);
                }
            }
            PatExpr::Typed { pat, ty } => {
                self.visit_pat_expr(&mut pat);
                self.visit_type_expr(&mut ty);
            }
        }
    }

    fn visit_case_target_expr(&mut self, expr: &mut Augmented<CaseTargetExpr>) {
        match expr.val {
            CaseTargetExpr::Error => {}
            CaseTargetExpr::Unit => {}
            CaseTargetExpr::Bool(_) => {}
            CaseTargetExpr::Int(_) => {}
            CaseTargetExpr::PatExpr(pat) => {
                self.visit_pat_expr(&mut pat);
            }
        }
    }

    fn visit_case_expr(&mut self, expr: &mut Augmented<CaseExpr>) {
        self.visit_case_target_expr(&mut expr.val.target);
        self.visit_val_expr(&mut expr.val.body)
    }

    fn dfs_visit_else_expr(&mut self, expr: &mut Augmented<ElseExpr>) {
        match expr.val {
            ElseExpr::Error => {}
            ElseExpr::Elif {
                cond,
                then_branch,
                else_branch,
            } => {
                self.visit_val_expr(&mut cond);
                self.visit_block_expr(&mut then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_else_expr(&mut else_branch);
                }
            }
            ElseExpr::Else(expr) => {
                self.visit_block_expr(&mut expr);
            }
        }
    }

    fn dfs_visit_val_expr(&mut self, expr: &mut Augmented<ValExpr>) {
        match expr.val {
            ValExpr::Error => {}
            ValExpr::Unit => {}
            ValExpr::Int(_) => {}
            ValExpr::Bool(_) => {}
            ValExpr::Float(_) => {}
            ValExpr::String(_) => {}
            ValExpr::Ref(expr) => {
                self.visit_val_expr(&mut expr);
            }
            ValExpr::Deref(expr) => {
                self.visit_val_expr(&mut expr);
            }
            ValExpr::StructLiteral(items) => {
                for (_, item) in items {
                    self.visit_val_expr(&mut item);
                }
            }
            ValExpr::BinaryOp {
                op: _,
                left_operand,
                right_operand,
            } => {
                self.visit_val_expr(&mut left_operand);
                self.visit_val_expr(&mut right_operand);
            }
            ValExpr::IfThen {
                cond,
                then_branch,
                else_branch,
            } => {
                self.visit_val_expr(&mut cond);
                self.visit_block_expr(&mut then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_else_expr(&mut else_branch);
                }
            }
            ValExpr::CaseOf { expr, cases } => {
                self.visit_val_expr(&mut expr);
                for case in cases {
                    self.visit_case_expr(&mut case);
                }
            }
            ValExpr::Block(expr) => {
                self.visit_block_expr(&mut expr);
            }
            ValExpr::ArrayLiteral(items) => {
                for item in items {
                    self.visit_val_expr(&mut item);
                }
            }
            ValExpr::Identifier(_) => {}
            ValExpr::FnLiteral {
                params,
                returnty,
                body,
            } => {
                for param in params {
                    self.visit_pat_expr(&mut param);
                }
                self.visit_type_expr(&mut returnty);
                self.visit_block_expr(&mut body);
            }
            ValExpr::ArrayAccess { root, index } => {
                self.visit_val_expr(&mut root);
                self.visit_val_expr(&mut index);
            }
            ValExpr::FieldAccess { root, field: _ } => {
                self.visit_val_expr(&mut root);
            }
            ValExpr::Concretization { generic, tyargs } => {
                self.visit_val_expr(&mut generic);
                for tyarg in tyargs {
                    self.visit_type_expr(&mut tyarg);
                }
            }
            ValExpr::App { fun, args } => {
                self.visit_val_expr(&mut fun);
                for arg in args {
                    self.visit_val_expr(&mut arg);
                }
            }
        }
    }

    fn visit_translation_unit(&mut self, tu: &mut TranslationUnit) {
        self.dfs_visit_translation_unit(tu);
    }
    fn visit_kind_expr(&mut self, expr: &mut Augmented<KindExpr>) {
        self.dfs_visit_kind_expr(expr);
    }
    fn visit_val_expr(&mut self, expr: &mut Augmented<ValExpr>) {
        self.dfs_visit_val_expr(expr);
    }
    fn visit_pat_expr(&mut self, expr: &mut Augmented<PatExpr>) {
        self.dfs_visit_pat_expr(expr);
    }
    fn visit_type_expr(&mut self, expr: &mut Augmented<TypeExpr>) {
        self.dfs_visit_type_expr(expr);
    }
    fn visit_block_expr(&mut self, expr: &mut Augmented<BlockExpr>) {
        self.dfs_visit_block_expr(expr);
    }
    fn visit_else_expr(&mut self, expr: &mut Augmented<ElseExpr>) {
        self.dfs_visit_else_expr(expr);
    }
    fn visit_file_statement(&mut self, statement: &mut Augmented<FileStatement>) {
        self.dfs_visit_file_statement(statement)
    }
    fn visit_block_statement(&mut self, statement: &mut Augmented<BlockStatement>) {
        self.dfs_visit_block_statement(statement)
    }
}
