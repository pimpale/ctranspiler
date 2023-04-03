use num_bigint::BigInt;
use num_rational::BigRational;

use crate::ast::Augmented;
use crate::ast::CaseTargetExpr;

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
pub enum StructItemExpr<T> {
    Error,
    Identified {
        identifier: String,
        expr: Box<Augmented<T>>,
    },
}

#[derive(Clone, Debug)]
pub enum KindExpr {
    Error,
    Type,
    Int,
    UInt,
    Float,
    Bool,
    // this is the kind of a generic function
    TypeLevelFn {
        args: Vec<Augmented<KindExpr>>,
        returnkind: Box<Augmented<KindExpr>>,
    },
}

#[derive(Clone, Debug)]
pub enum TypeExpr {
    // An error when parsing
    Error,
    Identifier(String),
    // types
    UnitTy,
    ArrayTy,
    SliceTy,
    IntTy,
    UIntTy,
    FloatTy,
    BoolTy,
    // const literals
    Int(BigInt),
    Bool(bool),
    Float(BigRational),
    // unary ops (syntax sugar)
    Ref(Box<Augmented<TypeExpr>>),
    // struct and enumify
    Struct(Vec<Augmented<StructItemExpr<TypeExpr>>>),
    Enum(Vec<Augmented<StructItemExpr<TypeExpr>>>),
    Union(Vec<Augmented<StructItemExpr<TypeExpr>>>),
    // generic
    Concretization {
        generic: Box<Augmented<TypeExpr>>,
        tyargs: Vec<Augmented<TypeExpr>>,
    },
    // type of a function
    Fn {
        args: Vec<Augmented<TypeExpr>>,
        returntype: Box<Augmented<TypeExpr>>,
    },
}

#[derive(Clone, Debug)]
pub enum TypePatExpr {
    Error,
    Identifier {
        identifier: String,
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
    StructLiteral(Vec<Augmented<StructItemExpr<PatExpr>>>),
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
    StructLiteral(Vec<Augmented<StructItemExpr<ValExpr>>>),
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
    // Lambda function
    FnLiteral {
        args: Vec<Augmented<PatExpr>>,
        returntype: Box<Augmented<TypeExpr>>,
        body: Box<Augmented<ValExpr>>,
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
    Error,
    TypeDef {
        tyargs: Vec<Augmented<TypePatExpr>>,
        identifier: String,
        value: Box<Augmented<TypeExpr>>,
    },
    Use {
        prefix: String,
    },
    Let {
        pattern: Box<Augmented<PatExpr>>,
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
    Error,
    TypeDef {
        tyargs: Vec<Augmented<TypePatExpr>>,
        identifier: String,
        value: Box<Augmented<TypeExpr>>,
    },
    Let {
        tyargs: Vec<Augmented<TypePatExpr>>,
        pattern: Box<Augmented<PatExpr>>,
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
            FileStatement::Error => {}
            FileStatement::TypeDef { tyargs, value, .. } => {
                for tyarg in tyargs {
                    self.visit_type_pat_expr(&mut tyarg);
                }
                self.visit_type_expr(&mut value);
            }
            FileStatement::Let {
                tyargs,
                pattern,
                value,
            } => {
                for tyarg in tyargs {
                    self.visit_type_pat_expr(&mut tyarg);
                }
                self.visit_pat_expr(&mut pattern);
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
            BlockStatement::Error => {}
            BlockStatement::TypeDef { tyargs, value, .. } => {
                for tyarg in tyargs {
                    self.visit_type_pat_expr(&mut tyarg);
                }
                self.visit_type_expr(&mut value);
            }
            BlockStatement::Use { prefix } => {}
            BlockStatement::Let { pattern, value } => {
                self.visit_pat_expr(&mut pattern);
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

    fn visit_struct_item_expr<T>(
        &mut self,
        expr: &mut Augmented<StructItemExpr<T>>,
        lower: impl Fn(&mut Self, &mut Augmented<T>),
    ) {
        match expr.val {
            StructItemExpr::Error => {}
            StructItemExpr::Identified { identifier, expr } => {
                lower(self, &mut expr);
            }
        }
    }

    fn dfs_visit_kind_expr(&mut self, expr: &mut Augmented<KindExpr>) {
        match expr.val {
            KindExpr::Error => {}
            KindExpr::Bool => {}
            KindExpr::Int => {}
            KindExpr::Float => {}
            KindExpr::TypeLevelFn { args, returnkind } => {
                for arg in args {
                    self.visit_kind_expr(&mut arg);
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
            TypeExpr::IntTy => {}
            TypeExpr::FloatTy => {}
            TypeExpr::ArrayTy => {}
            TypeExpr::SliceTy => {}
            TypeExpr::Int(_) => {}
            TypeExpr::Bool(_) => {}
            TypeExpr::Float(_) => {}
            TypeExpr::Ref(element) => {
                self.visit_type_expr(&mut element);
            }
            TypeExpr::Struct(items) => {
                for item in items {
                    self.visit_struct_item_expr(&mut item, Self::visit_type_expr);
                }
            }
            TypeExpr::Enum(items) => {
                for item in items {
                    self.visit_struct_item_expr(&mut item, Self::visit_type_expr);
                }
            }
            TypeExpr::Union(items) => {
                for item in items {
                    self.visit_struct_item_expr(&mut item, Self::visit_type_expr);
                }
            }
            TypeExpr::Concretization { generic, tyargs } => {
                self.visit_type_expr(&mut generic);
                for tyarg in tyargs {
                    self.visit_type_expr(&mut tyarg);
                }
            }
            TypeExpr::Fn { args, returntype } => {
                for arg in args {
                    self.visit_type_expr(&mut arg);
                }
                self.visit_type_expr(&mut returntype);
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
                for item in items {
                    self.visit_struct_item_expr(&mut item, Self::visit_pat_expr);
                }
            }
            PatExpr::Typed { pat, ty } => {
                self.visit_pat_expr(&mut pat);
                self.visit_type_expr(&mut ty);
            }
        }
    }

    fn visit_case_expr(&mut self, expr: &mut Augmented<CaseExpr>) {
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
                for item in items {
                    self.visit_struct_item_expr(&mut item, Self::visit_val_expr);
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
                args,
                returntype,
                body,
            } => {
                for arg in args {
                    self.visit_pat_expr(&mut arg);
                }
                self.visit_type_expr(&mut returntype);
                self.visit_val_expr(&mut body);
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
