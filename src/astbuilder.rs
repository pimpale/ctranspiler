use super::ast::*;
use super::codereader::union_of;
use super::dlogger::DiagnosticLogger;
use super::token::{Token, TokenKind};
use lsp_types::Range;
use num_bigint::BigInt;
use num_traits::Zero;
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

fn expect_token<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    structure: &str,
    expected_tokens: Vec<TokenKind>,
) -> Token {
    let tk = tkiter.next().unwrap();
    if let Some(tkk) = tk.kind {
        if expected_tokens.contains(&tkk) {
            return tk;
        }
    }
    dlogger.log_unexpected_token_specific(tk.range, structure, expected_tokens, tk.kind);
    return tk;
}

fn expect_identifier<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    structure: &str,
) -> Vec<u8> {
    let tk = tkiter.next().unwrap();
    if let Some(TokenKind::Identifier(id)) = tk.kind {
        return id;
    }
    dlogger.log_unexpected_token_specific(
        tk.range,
        structure,
        vec![TokenKind::Identifier(vec![])],
        tk.kind,
    );
    vec![]
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
fn parse_delimited_statement_seq<TkIter: Iterator<Item = Token>, T>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    structure: &str,
    parser_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<T>,
    start_tok: TokenKind,
    end_tok: TokenKind,
    sep_tok: TokenKind,
) -> (Range, Vec<Metadata>, Vec<Augmented<T>>, bool) {
    let metadata = get_metadata(tkiter);

    let Token {
        range: lrange,
        kind,
    } = expect_token(tkiter, dlogger, structure, vec![start_tok]);

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
        statements.push(parser_fn(tkiter, dlogger));

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
            dlogger.log_unexpected_token_specific(range, structure, vec![sep_tok, end_tok], kind);
        }
    };

    (
        union_of(lrange, rrange),
        metadata,
        statements,
        has_ending_sep,
    )
}

// given a start token, and end token, and a sep token, will parse statements (with an optional terminating sep)
fn parse_delimited_statement_seq_opt_sep<TkIter: Iterator<Item = Token>, T>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    structure: &str,
    parser_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<T>,
    start_tok: TokenKind,
    end_tok: TokenKind,
    sep_tok: TokenKind,
) -> (Range, Vec<Metadata>, Vec<Augmented<T>>, bool) {
    let metadata = get_metadata(tkiter);

    let Token {
        range: lrange,
        kind,
    } = expect_token(tkiter, dlogger, structure, vec![start_tok]);

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
        statements.push(parser_fn(tkiter, dlogger));

        // parse sep and potentially closing delimiter
        match tkiter.peek_nth(0).unwrap().kind {
            k if k == Some(sep_tok) => {
                tkiter.next();
                // if sep, check next
                if tkiter.peek_nth(0).unwrap().kind == Some(end_tok) {
                    let Token { range, .. } = tkiter.next().unwrap();
                    rrange = range;
                    break true;
                }
            }
            k if kind == Some(end_tok) => {
                rrange = tkiter.next().unwrap().range;
                break false;
            }
            _ => (),
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
) -> impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<StructItemExpr<T>> {
    move |tkiter: &mut PeekMoreIterator<TkIter>, dlogger: &mut DiagnosticLogger| {
        let metadata = get_metadata(tkiter);
        let Token { range, kind } = tkiter.next().unwrap();
        if let Some(TokenKind::Identifier(identifier)) = kind {
            if let Token {
                kind: Some(TokenKind::Constrain),
                ..
            } = tkiter.peek_nth(0).unwrap()
            {
                let _ = tkiter.next().unwrap();
                let expr = Box::new(lower_fn(tkiter, dlogger));
                return Augmented {
                    metadata,
                    range: union_of(range, expr.range),
                    val: StructItemExpr::Identified { identifier, expr },
                };
            } else {
                return Augmented {
                    metadata,
                    range,
                    val: StructItemExpr::Eponymous(identifier),
                };
            }
        } else {
            dlogger.log_unexpected_token_specific(
                range,
                "struct item",
                vec![TokenKind::Identifier(vec![])],
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
    let (range, metadata, statements, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "struct literal",
        parse_struct_item_expr(parse_valexpr),
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Comma,
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
    let metadata = get_metadata(tkiter);
    let left_tk = tkiter.next().unwrap();
    assert!(left_tk.kind == Some(TokenKind::ParenLeft));
    let val = Box::new(parse_valexpr(tkiter, dlogger));
    let right_tk = expect_token(
        tkiter,
        dlogger,
        "group expression",
        vec![TokenKind::ParenRight],
    );

    Augmented {
        range: union_of(left_tk.range, right_tk.range),
        metadata,
        val: ValExpr::Group(val),
    }
}

fn parse_blockexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockExpr> {
    let (range, metadata, statements, trailing_semicolon) = parse_delimited_statement_seq_opt_sep(
        tkiter,
        dlogger,
        "group expression",
        parse_blockstatement,
        TokenKind::ParenLeft,
        TokenKind::ParenRight,
        TokenKind::Semicolon,
    );

    Augmented {
        range,
        metadata,
        val: BlockExpr {
            statements,
            trailing_semicolon,
        },
    }
}

fn parse_exact_valexpr_block<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let metadata = get_metadata(tkiter);
    let block_tk = tkiter.next().unwrap();
    assert!(block_tk.kind == Some(TokenKind::Block));

    let block = Box::new(parse_blockexpr(tkiter, dlogger));

    Augmented {
        range: union_of(block_tk.range, block.range),
        metadata,
        val: ValExpr::Block(block),
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
                "case target",
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
    expect_token(tkiter, dlogger, "case branch", vec![TokenKind::Defun]);
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
    expect_token(tkiter, dlogger, "case of expression", vec![TokenKind::Of]);

    let (range, metadata, cases, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "case branches",
        parse_caseexpr,
        TokenKind::ParenLeft,
        TokenKind::ParenRight,
        TokenKind::Semicolon,
    );

    Augmented {
        metadata,
        range: union_of(case_tk.range, range),
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
fn parse_exact_valexpr_int<TkIter: Iterator<Item = Token>>(
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
fn parse_exact_valexpr_rational<TkIter: Iterator<Item = Token>>(
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

fn decide_valexpr_term<TkIter: Iterator<Item = Token>>(
    tkkind: &TokenKind,
) -> Option<fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<ValExpr>> {
    match *tkkind {
        TokenKind::Bool(_) => Some(parse_exact_valexpr_bool::<TkIter>),
        TokenKind::Int(_) => Some(parse_exact_valexpr_int::<TkIter>),
        TokenKind::Float(_) => Some(parse_exact_valexpr_rational::<TkIter>),
        TokenKind::String { .. } => Some(parse_exact_valexpr_string::<TkIter>),
        TokenKind::BraceLeft => Some(parse_exact_valexpr_struct_literal::<TkIter>),
        TokenKind::ParenLeft => Some(parse_exact_valexpr_group::<TkIter>),
        TokenKind::Case => Some(parse_exact_valexpr_caseof::<TkIter>),
        TokenKind::Identifier(_) => Some(parse_exact_identifier::<TkIter>),
        _ => None,
    }
}

// parses basic term
fn parse_valexpr_term<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
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
        if let Some(parser) = decide_valexpr_term(&kind) {
            parser(tkiter, dlogger)
        } else {
            // grab metadata
            let metadata = get_metadata(tkiter);
            // consume unexpected token
            dlogger.log_unexpected_token_specific(range, "valexpr term", vec![], Some(kind));
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
        dlogger.log_unexpected_token_specific(range, "valexpr term", vec![], None);
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
) -> Augmented<ValExpr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_valexpr_term,
        simple_operator_fn(|x| match x {
            TokenKind::ModuleAccess => Some(ValBinaryOpKind::ModuleAccess),
            _ => None,
        }),
    )
}

fn parse_valexpr_apply_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    parse_l_binary_op(tkiter, dlogger, parse_tight_operators, |tkiter, _| {
        if let Token {
            kind: Some(kind),
            range,
        } = tkiter.peek_nth(0).unwrap()
        {
            if decide_valexpr_term::<TkIter>(kind).is_some() {
                return Some((ValBinaryOpKind::Apply, *range));
            }
        }
        None
    })
}

fn parse_valexpr_multiplication_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_valexpr_apply_operators,
        simple_operator_fn(|x| match x {
            TokenKind::Mul => Some(ValBinaryOpKind::Mul),
            TokenKind::Div => Some(ValBinaryOpKind::Div),
            TokenKind::Rem => Some(ValBinaryOpKind::Rem),
            _ => None,
        }),
    )
}

fn parse_valexpr_addition_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_valexpr_multiplication_operators,
        simple_operator_fn(|x| match x {
            TokenKind::Plus => Some(ValBinaryOpKind::Add),
            TokenKind::Minus => Some(ValBinaryOpKind::Sub),
            _ => None,
        }),
    )
}

fn parse_valexpr_compare_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_valexpr_addition_operators,
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

fn parse_valexpr_binary_bool_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_valexpr_compare_operators,
        simple_operator_fn(|x| match x {
            TokenKind::And => Some(ValBinaryOpKind::And),
            TokenKind::Or => Some(ValBinaryOpKind::Or),
            _ => None,
        }),
    )
}

fn parse_valexpr_pipe<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_valexpr_binary_bool_operators,
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
    parse_pipe(tkiter, dlogger)
}

fn parse_patexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<PatExpr> {
    let metadata = get_metadata(tkiter);
    match tkiter.peek_nth(0).unwrap().kind {
        // TODO: parse mutable patterns
        Some(TokenKind::Ignore) | Some(TokenKind::Identifier(_)) => {
            // actually get the tk
            let first_tk = tkiter.next().unwrap();

            // get the next token
            expect_token(
                tkiter,
                dlogger,
                "pattern expression",
                vec![TokenKind::Constrain],
            );

            let ty = Box::new(parse_typeexpr(tkiter, dlogger));
            Augmented {
                range: union_of(first_tk.range, ty.range),
                metadata,
                val: match first_tk.kind {
                    Some(TokenKind::Identifier(identifier)) => PatExpr::Identifier {
                        ty,
                        identifier,
                        mutable: true,
                    },
                    _ => PatExpr::Ignore { ty },
                },
            }
        }
        Some(TokenKind::BraceLeft) => {
            let (range, _, statements, _) = parse_delimited_statement_seq(
                tkiter,
                dlogger,
                "pattern struct literal",
                parse_struct_item_expr(parse_patexpr),
                TokenKind::BraceLeft,
                TokenKind::BraceRight,
                TokenKind::Comma,
            );
            Augmented {
                range,
                metadata,
                val: PatExpr::StructLiteral(statements),
            }
        }
        _ => {
            let tk = tkiter.next().unwrap();
            dlogger.log_unexpected_token_specific(
                tk.range,
                "pattern expression",
                vec![
                    TokenKind::Identifier(vec![]),
                    TokenKind::Ignore,
                    TokenKind::BraceLeft,
                ],
                tk.kind,
            );
            Augmented {
                range: tk.range,
                metadata,
                val: PatExpr::Error,
            }
        }
    }
}

fn parse_args_expr<TkIter: Iterator<Item = Token>, T>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<T>,
) -> Augmented<ArgsExpr<T>> {
    let (range, metadata, args, terminating_bool) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "arguments",
        lower_fn,
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Comma,
    );

    Augmented {
        range,
        metadata,
        val: ArgsExpr {
            args,
            terminating_bool,
        },
    }
}

// parses a let (whether in a function body or out of a function body) or panics
fn parse_exact_let<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> (
    Range,
    Vec<Metadata>,
    Box<Augmented<PatExpr>>,
    Box<Augmented<ValExpr>>,
) {
    let metadata = get_metadata(tkiter);
    let let_tk = tkiter.next().unwrap();
    assert!(let_tk.kind == Some(TokenKind::Let));
    let pattern = Box::new(parse_patexpr(tkiter, dlogger));
    expect_token(tkiter, dlogger, "let statement", vec![TokenKind::Assign]);

    let value = Box::new(parse_valexpr(tkiter, dlogger));

    (
        union_of(let_tk.range, value.range),
        metadata,
        pattern,
        value,
    )
}

// parses a fn or panics
fn parse_exact_fn<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> (
    Range,
    Vec<Metadata>,
    Vec<u8>,
    Box<Augmented<ArgsExpr<PatExpr>>>,
    Box<Augmented<TypeExpr>>,
    Box<Augmented<ValExpr>>,
) {
    let metadata = get_metadata(tkiter);
    let fn_tk = tkiter.next().unwrap();
    assert!(fn_tk.kind == Some(TokenKind::Fn));
    let identifier = expect_identifier(tkiter, dlogger, "function definition");
    let args = Box::new(parse_args_expr(tkiter, dlogger, parse_patexpr));
    expect_token(
        tkiter,
        dlogger,
        "function definition",
        vec![TokenKind::Defun],
    );
    let ty = Box::new(parse_typeexpr(tkiter, dlogger));
    let body = Box::new(parse_valexpr(tkiter, dlogger));
    (
        union_of(fn_tk.range, body.range),
        metadata,
        identifier,
        args,
        ty,
        body,
    )
}

// parses a function or panics
fn parse_exact_blockstatement_fndef<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let (range, metadata, identifier, args, returntype, body) = parse_exact_fn(tkiter, dlogger);
    Augmented {
        range,
        metadata,
        val: BlockStatement::FnDef {
            args,
            body,
            identifier,
            returntype,
        },
    }
}

// parses a let or panics
fn parse_exact_blockstatement_let<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let (range, metadata, pattern, value) = parse_exact_let(tkiter, dlogger);

    Augmented {
        metadata,
        range,
        val: BlockStatement::Let { pattern, value },
    }
}

// parses a set or panics
fn parse_exact_blockstatement_set<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let metadata = get_metadata(tkiter);
    assert!(matches!(
        tkiter.peek_nth(0).unwrap().kind,
        Some(TokenKind::Set)
    ));
    let set_tk = tkiter.next().unwrap();
    let pattern = Box::new(parse_patexpr(tkiter, dlogger));
    expect_token(tkiter, dlogger, "set statement", vec![TokenKind::Assign]);
    let value = Box::new(parse_valexpr(tkiter, dlogger));

    Augmented {
        metadata,
        range: union_of(set_tk.range, value.range),
        val: BlockStatement::Set { pattern, value },
    }
}

// parses a set or panics
fn parse_exact_blockstatement_while<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let metadata = get_metadata(tkiter);
    let while_tk = tkiter.next().unwrap();
    assert!(Some(TokenKind::While) == while_tk.kind);

    let cond = Box::new(parse_valexpr(tkiter, dlogger));

    let body = Box::new(parse_valexpr_term(tkiter, dlogger));

    Augmented {
        metadata,
        range: union_of(while_tk.range, body.range),
        val: BlockStatement::While { cond, body },
    }
}

// parse range expression
fn parse_rangeexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<RangeExpr> {
    let start = Box::new(parse_valexpr_term(tkiter, dlogger));

    let range_expr = tkiter.next().unwrap();
    let inclusive = match range_expr.kind {
        Some(TokenKind::Range) => false,
        Some(TokenKind::RangeInclusive) => true,
        k => {
            dlogger.log_unexpected_token_specific(
                range_expr.range,
                "range expression",
                vec![TokenKind::Range, TokenKind::RangeInclusive],
                k,
            );
            false
        }
    };

    let end = Box::new(parse_valexpr_term(tkiter, dlogger));

    Augmented {
        metadata: vec![],
        range: union_of(start.range, end.range),
        val: RangeExpr {
            start,
            end,
            inclusive,
        },
    }
}

// parses a set or panics
fn parse_exact_blockstatement_for<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let metadata = get_metadata(tkiter);
    let for_tk = tkiter.next().unwrap();
    assert!(Some(TokenKind::For) == for_tk.kind);

    let pattern = Box::new(parse_patexpr(tkiter, dlogger));

    expect_token(tkiter, dlogger, "for loop", vec![TokenKind::Of]);

    let range = Box::new(parse_rangeexpr(tkiter, dlogger));

    let by = if tkiter.peek_nth(0).unwrap().kind == Some(TokenKind::By) {
        let _ = tkiter.next().unwrap();
        Some(Box::new(parse_valexpr_term(tkiter, dlogger)))
    } else {
        None
    };

    let body = Box::new(parse_valexpr_term(tkiter, dlogger));

    Augmented {
        metadata,
        range: union_of(for_tk.range, body.range),
        val: BlockStatement::For {
            pattern,
            body,
            range,
            by,
        },
    }
}

// parses a function or panics
fn parse_exact_blockstatement_typedef<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let metadata = get_metadata(tkiter);
    let type_tk = tkiter.next().unwrap();
    assert!(type_tk.kind == Some(TokenKind::Type));

    let identifier = expect_identifier(tkiter, dlogger, "type definition statement");

    expect_token(
        tkiter,
        dlogger,
        "type definition statement",
        vec![TokenKind::Assign],
    );

    let tyexpr = Box::new(parse_typeexpr(tkiter, dlogger));

    Augmented {
        metadata,
        range: union_of(type_tk.range, tyexpr.range),
        val: BlockStatement::TypeDef {
            identifier,
            value: tyexpr,
        },
    }
}

// parses a set or panics
fn parse_exact_blockstatement_do<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let metadata = get_metadata(tkiter);

    let val = Box::new(parse_valexpr(tkiter, dlogger));

    Augmented {
        metadata,
        range: val.range,
        val: BlockStatement::Do(val),
    }
}

fn parse_blockstatement<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
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

    match maybe_kind {
        Some(TokenKind::Type) => parse_exact_blockstatement_typedef(tkiter, dlogger),
        Some(TokenKind::Let) => parse_exact_blockstatement_let(tkiter, dlogger),
        Some(TokenKind::Fn) => parse_exact_blockstatement_fndef(tkiter, dlogger),
        Some(TokenKind::Set) => parse_exact_blockstatement_set(tkiter, dlogger),
        Some(TokenKind::While) => parse_exact_blockstatement_while(tkiter, dlogger),
        Some(TokenKind::For) => parse_exact_blockstatement_for(tkiter, dlogger),
        Some(_) => parse_exact_blockstatement_do(tkiter, dlogger),
        None => {
            // grab metadata
            let metadata = get_metadata(tkiter);
            dlogger.log_unexpected_token_specific(range, "block statement", vec![], None);
            Augmented {
                range,
                val: BlockStatement::Error,
                metadata,
            }
        }
    }
}

pub fn parse_typeexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
}

pub fn construct_ast<TkIterSource: IntoIterator<Item = Token>>(
    tokens: TkIterSource,
    mut dlogger: DiagnosticLogger,
) -> File {
    parse_expr(&mut tokens.into_iter().peekmore(), &mut dlogger)
}
