use super::ast::*;
use super::codereader::union_of;
use super::dlogger::DiagnosticLogger;
use super::token::{Token, TokenKind};
use lsp_types::Range;
use peekmore::{PeekMore, PeekMoreIterator};

// clobbers the cursor
fn get_metadata<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
) -> Vec<Metadata> {
    let mut metadata = vec![];
    while let Token {
        kind: Some(TokenKind::Metadata { significant, value }),
        range,
    } = tkiter.peek_nth(0).unwrap().clone()
    {
        metadata.push(Metadata {
            range,
            significant,
            value,
        });
        // consume
        tkiter.next();
    }

    // return data
    metadata
}

fn parse_l_binary_op<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<ValExpr>,
    operator_fn: impl Fn(
        &mut PeekMoreIterator<TkIter>,
        &mut DiagnosticLogger,
    ) -> Option<(ValBinaryOpKind, Range)>,
) -> Augmented<ValExpr> {
    // parse lower expr
    let mut expr = lower_fn(tkiter, dlogger);

    loop {
        // operator function consumes operator, returning binop
        if let Some((op, _)) = operator_fn(tkiter, dlogger) {
            // define the old expr as our left side
            let left_operand = Box::new(expr);

            // parse rest of expression
            let right_operand = Box::new(lower_fn(tkiter, dlogger));

            // set new expr which contains the lhs and rhs
            expr = Augmented {
                metadata: vec![],
                range: union_of(left_operand.range, right_operand.range),
                val: ValExpr::BinaryOp {
                    op,
                    left_operand,
                    right_operand,
                },
            }
        } else {
            // if we dont have a definition just return the current expr
            return expr;
        }
    }
}

fn parse_r_binary_op<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<ValExpr>,
    operator_fn: impl Fn(
        &mut PeekMoreIterator<TkIter>,
        &mut DiagnosticLogger,
    ) -> Option<(ValBinaryOpKind, Range)>,
) -> Augmented<ValExpr> {
    // parse lower expr
    let expr = lower_fn(tkiter, dlogger);

    // operator function consumes operator, returning binop
    if let Some((op, _)) = operator_fn(tkiter, dlogger) {
        // define the old expr as our left side
        let left_operand = Box::new(expr);

        // parse rest of expression with same fn (this may stackoverflow)
        let right_operand = Box::new(parse_r_binary_op(tkiter, dlogger, lower_fn, operator_fn));

        // return
        Augmented {
            metadata: vec![],
            range: union_of(left_operand.range, right_operand.range),
            val: ValExpr::BinaryOp {
                op,
                left_operand,
                right_operand,
            },
        }
    } else {
        // if token is invalid we can just return the current expr
        expr
    }
}

// a simple operator that checks if the next token is valid, and then advances
fn simple_operator_fn<'a, TkIter: Iterator<Item = Token>, OpKind>(
    decide_fn: impl Fn(&TokenKind) -> Option<OpKind> + 'a,
) -> impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Option<(OpKind, Range)> + 'a {
    move |tkiter: &mut PeekMoreIterator<TkIter>, _: &mut DiagnosticLogger| {
        if let Token {
            kind: Some(kind),
            range,
        } = tkiter.peek_nth(0).unwrap()
        {
            if let Some(binop) = decide_fn(kind) {
                let range = *range;
                // drop peeked operator
                tkiter.next();
                return Some((binop, range));
            }
        }
        None
    }
}

// given a start token, and end token, and a sep token, will parse statements (with an optional terminating sep)
fn parse_exact_delimited_statement_seq<TkIter: Iterator<Item = Token>, T>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    parser_fn: dyn Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<T>,
    start_tok: TokenKind,
    end_tok: TokenKind,
    sep_tok: TokenKind,
) -> (Range, Metadata, Vec<Augmented<T>>, bool) {
    let metadata = get_metadata(tkiter);
    let Token {
        range: lrange,
        kind,
    } = tkiter.next().unwrap();

    // assert that the first token is the start_token
    assert!(kind == Some(start_tok));

    let mut rrange = lrange;

    let mut statements = vec![];

    // test match token to see if it sep or end
    let mut has_ending_sep = loop {
        // if next token is closing delimiter, break
        if tkiter.peek_nth(0).unwrap().kind == Some(end_tok) {
            let Token { range, .. } = tkiter.next().unwrap();
            rrange = range;
            break false;
        }

        // parse a statement
        statements.append(parser_fn(tkiter, dlogger));

        // parse sep and potentially closing delimiter
        let Token { range, kind } = tkiter.next().unwrap();
        if kind == Some(sep_tok) {
            // if sep, check next
            if tkiter.peek_nth(0).unwrap().kind == Some(end_tok) {
                let Token { range, .. } = tkiter.next().unwrap();
                rrange = range;
                break true;
            }
        } else if kind == Some(end_tok) {
            rrange = range;
            break false;
        } else {
            dlogger.log_unexpected_token_specific(range, vec![sep_tok, end_tok], kind);
        }
    };

    (
        union_of(lrange, rrange),
        metadata,
        statements,
        has_ending_sep,
    )
}

fn parse_struct_item_expr<TkIter: Iterator<Item = Token>, T>(
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<T>,
) -> impl FnMut(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<StructItemExpr<T>>
{
    move |tkiter: &mut PeekMoreIterator<TkIter>, dlogger: &mut DiagnosticLogger| {
        let metadata = get_metadata(tkiter);
        let Token { range, kind } = tkiter.next().unwrap();
        if let Some(TokenKind::Identifier(id)) = kind {
            if let Token {
                kind: Some(TokenKind::Constrain),
                ..
            } = tkiter.peek_nth(0).unwrap()
            {
                let _ = tkiter.next().unwrap();
                let val = lower_fn(tkiter, dlogger);
                return Augmented {
                    metadata,
                    range: union_of(range, val.range),
                    val: StructItemExpr::Identified(id, val),
                };
            } else {
                return Augmented {
                    metadata,
                    range,
                    val: StructItemExpr::Eponymous(id),
                };
            }
        } else {
            dlogger.log_unexpected_token_specific(
                range,
                vec![TokenKind::Identifier(String::new())],
                kind,
            );
            return Augmented {
                metadata,
                range,
                val: StructItemExpr::Error,
            };
        }
    }
}

fn parse_exact_valexpr_struct_literal<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let (range, metadata, statements, _) = parse_exact_delimited_statement_seq(
        tkiter,
        dlogger,
        parse_struct_item_expr(parse_valexpr),
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Semicolon,
    );

    Augmented {
        range,
        metadata,
        val: ValExpr::StructLiteral(statements),
    }
}

fn parse_exact_valexpr_group<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let (range, metadata, statements, _) = parse_exact_delimited_statement_seq(
        tkiter,
        dlogger,
        parse_bodystatement,
        TokenKind::ParenLeft,
        TokenKind::ParenRight,
        TokenKind::Semicolon,
    );

    Augmented {
        range,
        metadata,
        val: ValExpr::Group(statements),
    }
}

// parses an exact reference or panics
fn parse_exact_identifier<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Identifier(identifier)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: ValExpr::Identifier(identifier),
        }
    } else {
        unimplemented!()
    }
}

fn parse_casetargetexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<CaseTargetExpr> {
    let metadata = get_metadata(tkiter);
    let Token { range, kind } = tkiter.next().unwrap();
    let val = match kind {
        Some(TokenKind::Ignore) => CaseTargetExpr::Ignore,
        Some(TokenKind::Unit) => CaseTargetExpr::Unit,
        Some(TokenKind::Bool(x)) => CaseTargetExpr::Bool(x),
        Some(TokenKind::Int(x)) => CaseTargetExpr::Int(x),
        Some(TokenKind::Identifier(x)) => CaseTargetExpr::Identifier(x),
        k => {
            dlogger.log_unexpected_token_specific(
                range,
                vec![
                    TokenKind::Ignore,
                    TokenKind::Unit,
                    TokenKind::Bool(false),
                    TokenKind::Int(num_bigint::BigInt::zero()),
                    TokenKind::Identifier(vec![]),
                ],
                k,
            );
            CaseTargetExpr::Error
        }
    };

    Augmented {
        range,
        metadata,
        val,
    }
}

fn parse_caseexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<CaseExpr> {
    let metadata = get_metadata(tkiter);
    let target = Box::new(parse_casetargetexpr(tkiter, dlogger));
    let Token { range, kind } = tkiter.next().unwrap();
    if let Some(TokenKind::Defun) = kind {
    } else {
        dlogger.log_unexpected_token_specific(range, vec![TokenKind::Defun], kind);
    }
    let body = Box::new(parse_valexpr(tkiter, dlogger));
    Augmented {
        metadata,
        range: union_of(target.range, body.range),
        val: CaseExpr { target, body },
    }
}

// parses a case or panics
fn parse_exact_valexpr_caseof<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let metadata = get_metadata(tkiter);
    let Token { range, kind } = tkiter.next().unwrap();
    assert!(kind == Some(TokenKind::Case));
    let expr = Box::new(parse_valexpr(tkiter, dlogger));

    let case_tk = tkiter.next().unwrap();
    let expr = Box::new(parse_valexpr(tkiter, dlogger));
    let of_tk = tkiter.next().unwrap();
    if of_tk.kind != Some(TokenKind::Of) {
        dlogger.log_unexpected_token_specific(of_tk.range, Some(TokenKind::Of), of_tk.kind);
    }

    let (range, metadata, cases, _) = parse_exact_delimited_statement_seq(
        tkiter,
        dlogger,
        parse_caseexpr,
        TokenKind::ParenLeft,
        TokenKind::ParenRight,
        TokenKind::Semicolon,
    );

    Augmented {
        metadata,
        range: union_of(case_tk.range, cases.range),
        val: ValExpr::CaseOf { expr, cases },
    }
}

// parses a bool
fn parse_exact_valexpr_bool<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Bool(value)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: ValExpr::Bool(value),
        }
    } else {
        unreachable!();
    }
}

// parses a string
fn parse_exact_valexpr_string<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::String { value, block }),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: ValExpr::String { value, block },
        }
    } else {
        unreachable!();
    }
}

// parses a int
fn parse_exact_int<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Int(value)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: ValExpr::Int(value),
        }
    } else {
        unreachable!();
    }
}

// parses a rational
fn parse_exact_rational<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Float(value)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: ValExpr::Float(value),
        }
    } else {
        unreachable!();
    }
}

// creates a parser that parses a single token of the type specified
fn parse_simple<TkIter: Iterator<Item = Token>>(
    expected_kind: TokenKind,
    result_val: ValExpr,
) -> impl FnOnce(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<ValExpr> {
    move |tkiter: &mut PeekMoreIterator<TkIter>, _: &mut DiagnosticLogger| {
        let metadata = get_metadata(tkiter);
        let Token {
            range,
            kind: token_kind,
        } = tkiter.next().unwrap();
        let expected_kind_opt = Some(expected_kind);
        if token_kind == expected_kind_opt {
            Augmented {
                range,
                metadata,
                val: result_kind,
            }
        } else {
            unreachable!()
        }
    }
}
// creates a parser that parses a single token of the type specified
fn parse_unop<TkIter: Iterator<Item = Token>>(
    expected_kind: TokenKind,
    result_gen: fn(Box<Expr>) -> ExprKind,
) -> impl FnOnce(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented {
    move |tkiter: &mut PeekMoreIterator<TkIter>, dlogger: &mut DiagnosticLogger| {
        let metadata = get_metadata(tkiter);
        let Token {
            range,
            kind: token_kind,
        } = tkiter.next().unwrap();
        let expected_kind_opt = Some(expected_kind);
        if token_kind == expected_kind_opt {
            // now parse body
            let body = Box::new(parse_term(tkiter, dlogger));

            Augmented {
                range: union_of(range, body.range),
                metadata,
                kind: result_gen(body),
            }
        } else {
            unreachable!()
        }
    }
}

fn decide_term<TkIter: Iterator<Item = Token>>(
    tkkind: &TokenKind,
) -> Option<fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<ValExpr>> {
    match *tkkind {
        TokenKind::Bool(_) => Some(parse_exact_bool::<TkIter>),
        TokenKind::Int(_) => Some(parse_exact_int::<TkIter>),
        TokenKind::Float(_) => Some(parse_exact_rational::<TkIter>),
        TokenKind::String { .. } => Some(parse_exact_string::<TkIter>),
        TokenKind::BraceLeft => Some(parse_exact_valexpr_struct_literal::<TkIter>),
        TokenKind::ParenLeft => Some(parse_exact_valexpr_group::<TkIter>),
        TokenKind::Case => Some(parse_exact_valexpr_caseof::<TkIter>),
        TokenKind::Identifier(_) => Some(parse_exact_identifier::<TkIter>),
        _ => None,
    }
}

// parses basic term
fn parse_term<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented {
    tkiter.reset_cursor();

    let Token {
        kind: maybe_kind,
        range,
    } = loop {
        let token = tkiter.peek().unwrap();
        if let Token {
            kind: Some(TokenKind::Metadata { .. }),
            ..
        } = token
        {
            tkiter.advance_cursor();
        } else {
            break token;
        }
    }
    .clone();

    if let Some(kind) = maybe_kind {
        if let Some(parser) = decide_term(&kind) {
            parser(tkiter, dlogger)
        } else {
            // grab metadata
            let metadata = get_metadata(tkiter);
            // consume unexpected token
            dlogger.log_unexpected_token(range, "term", Some(kind));
            tkiter.next();
            Augmented {
                range,
                metadata,
                val: ValExpr::Error,
            }
        }
    } else {
        // grab metadata
        let metadata = get_metadata(tkiter);
        dlogger.log_unexpected_token(range, "term", None);
        Augmented {
            range,
            val: ValExpr::Error,
            metadata,
        }
    }
}

fn parse_tight_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_term,
        simple_operator_fn(|x| match x {
            TokenKind::ModuleAccess => Some(ValBinaryOpKind::ModuleAccess),
            _ => None,
        }),
    )
}

fn parse_apply_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented {
    parse_l_binary_op(tkiter, dlogger, parse_tight_operators, |tkiter, _| {
        if let Token {
            kind: Some(kind),
            range,
        } = tkiter.peek_nth(0).unwrap()
        {
            if decide_term::<TkIter>(kind).is_some() {
                return Some((ValBinaryOpKind::Apply, *range));
            }
        }
        None
    })
}

fn parse_range_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_apply_operators,
        simple_operator_fn(|x| match x {
            TokenKind::Range => Some(ValBinaryOpKind::Range),
            TokenKind::RangeInclusive => Some(ValBinaryOpKind::RangeInclusive),
            _ => None,
        }),
    )
}

fn parse_constrain<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented {
    parse_r_binary_op(
        tkiter,
        dlogger,
        parse_range_operators,
        simple_operator_fn(|x| match x {
            TokenKind::Constrain => Some(ValBinaryOpKind::Constrain),
            _ => None,
        }),
    )
}

fn parse_multiplication_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_constrain,
        simple_operator_fn(|x| match x {
            TokenKind::Mul => Some(ValBinaryOpKind::Mul),
            TokenKind::Div => Some(ValBinaryOpKind::Div),
            TokenKind::Rem => Some(ValBinaryOpKind::Rem),
            _ => None,
        }),
    )
}

fn parse_addition_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_multiplication_operators,
        simple_operator_fn(|x| match x {
            TokenKind::Plus => Some(ValBinaryOpKind::Add),
            TokenKind::Minus => Some(ValBinaryOpKind::Sub),
            _ => None,
        }),
    )
}

fn parse_compare_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_addition_operators,
        simple_operator_fn(|x| match x {
            TokenKind::Less => Some(ValBinaryOpKind::Less),
            TokenKind::Greater => Some(ValBinaryOpKind::Greater),
            TokenKind::LessEqual => Some(ValBinaryOpKind::LessEqual),
            TokenKind::GreaterEqual => Some(ValBinaryOpKind::GreaterEqual),
            TokenKind::Equal => Some(ValBinaryOpKind::Equal),
            TokenKind::NotEqual => Some(ValBinaryOpKind::NotEqual),
            _ => None,
        }),
    )
}

fn parse_binary_bool_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_compare_operators,
        simple_operator_fn(|x| match x {
            TokenKind::And => Some(ValBinaryOpKind::And),
            TokenKind::Or => Some(ValBinaryOpKind::Or),
            _ => None,
        }),
    )
}

fn parse_pipe<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_binary_type_operators,
        simple_operator_fn(|x| match x {
            TokenKind::Pipe => Some(ValBinaryOpKind::Pipe),
            _ => None,
        }),
    )
}

fn parse_valexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    parse_sequence(tkiter, dlogger)
}


// parses a let or panics
fn parse_exact_bodystatement_let<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BodyStatement> {
    let metadata = get_metadata(tkiter);
    assert!(matches!(
        tkiter.peek_nth(0).unwrap().kind,
        Some(TokenKind::Let | TokenKind::Const)
    ));
    let let_tk = tkiter.next().unwrap();
    let pattern = Box::new(parse_patexpr(tkiter, dlogger));
    let assign_tk = tkiter.next().unwrap();
    if assign_tk.kind != Some(TokenKind::Assign) {
        dlogger.log_unexpected_token_specific(
            assign_tk.range,
            Some(TokenKind::Assign),
            assign_tk.kind,
        );
    }

    let value = Box::new(parse_valexpr(tkiter, dlogger));

    Augmented {
        metadata,
        range: union_of(let_tk.range, value.range),
        val: BodyStatement::Let { pattern, value },
    }
}

// parses a set or panics
fn parse_exact_bodystatement_set<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BodyStatement> {
    let metadata = get_metadata(tkiter);
    assert!(matches!(
        tkiter.peek_nth(0).unwrap().kind,
        Some(TokenKind::Set)
    ));
    let mut_tk = tkiter.next().unwrap();
    let pattern = Box::new(parse_valexpr(tkiter, dlogger));
    let assign_tk = tkiter.next().unwrap();
    if assign_tk.kind != Some(TokenKind::Assign) {
        dlogger.log_unexpected_token_specific(
            assign_tk.range,
            vec![TokenKind::Assign],
            assign_tk.kind,
        );
    }

    let value = Box::new(parse_valexpr(tkiter, dlogger));

    Augmented {
        metadata,
        range: union_of(mut_tk.range, value.range),
        val: BodyStatement::Set { pattern, value },
    }
}

fn parse_bodystatement<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BodyStatement> {
    parse_sequence(tkiter, dlogger)
}

pub fn construct_ast<TkIterSource: IntoIterator<Item = Token>>(
    tokens: TkIterSource,
    mut dlogger: DiagnosticLogger,
) -> File {
    parse_expr(&mut tokens.into_iter().peekmore(), &mut dlogger)
}
