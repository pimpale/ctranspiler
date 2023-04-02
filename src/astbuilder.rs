use super::ast::*;
use super::codereader::union_of;
use super::dlogger::DiagnosticLogger;
use super::token::{Token, TokenKind};
use lsp_types::Range;
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

// resets the cursor back to 0
fn peek_past_metadata<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
) -> Token {
    tkiter.reset_cursor();
    let token = loop {
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
    tkiter.reset_cursor();
    token
}

fn exact_token<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    expected_token: TokenKind,
) -> Token {
    let tk = tkiter.next().unwrap();
    assert!(tk.kind == Some(expected_token));
    return tk;
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
) -> (Vec<u8>, Range) {
    let tk = tkiter.next().unwrap();
    if let Some(TokenKind::Identifier(id)) = tk.kind {
        return (id, tk.range);
    }
    dlogger.log_unexpected_token_specific(
        tk.range,
        structure,
        vec![TokenKind::Identifier(vec![])],
        tk.kind,
    );
    (vec![], tk.range)
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

fn parse_postfix_op<TkIter: Iterator<Item = Token>, T>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<T>,
    decide_fn: impl Fn(
        Option<TokenKind>,
    ) -> Option<
        Box<
            dyn Fn(
                &mut PeekMoreIterator<TkIter>,
                &mut DiagnosticLogger,
                Augmented<T>,
            ) -> Augmented<T>,
        >,
    >,
) -> Augmented<T> {
    // parse lower expr
    let mut expr = lower_fn(tkiter, dlogger);

    loop {
        if let Some(operator_fn) = decide_fn(tkiter.peek_nth(0).unwrap().kind) {
            // if the token is valid, then we parse apply the function to try to transform it
            expr = operator_fn(tkiter, dlogger, expr);
        } else {
            // if token is invalid we can just return the current expr
            break expr;
        }
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
    let left_tk = exact_token(tkiter, TokenKind::ParenLeft);
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
    let block_tk = exact_token(tkiter, TokenKind::Block);
    let block = Box::new(parse_blockexpr(tkiter, dlogger));

    Augmented {
        range: union_of(block_tk.range, block.range),
        metadata,
        val: ValExpr::Block(block),
    }
}

// parses an exact reference or panics
fn parse_exact_valexpr_identifier<TkIter: Iterator<Item = Token>>(
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
fn parse_exact_valexpr_case<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let metadata = get_metadata(tkiter);
    let Token {
        range: lrange,
        kind,
    } = exact_token(tkiter, TokenKind::Case);

    let expr = Box::new(parse_valexpr(tkiter, dlogger));

    let (range, metadata, cases, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "case branches",
        parse_caseexpr,
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Semicolon,
    );

    Augmented {
        metadata,
        range: union_of(lrange, range),
        val: ValExpr::CaseOf { expr, cases },
    }
}

fn parse_exact_if<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> (
    Range,
    Vec<Metadata>,
    Box<Augmented<ValExpr>>,
    Box<Augmented<BlockExpr>>,
    Option<Box<Augmented<ElseExpr>>>,
) {
    let metadata = get_metadata(tkiter);
    let Token {
        range: lrange,
        kind,
    } = exact_token(tkiter, TokenKind::If);

    let cond = Box::new(parse_valexpr(tkiter, dlogger));
    let then_branch = Box::new(parse_blockexpr(tkiter, dlogger));

    // add else branch if it exists
    let else_branch = if let Some(TokenKind::Else) = tkiter.peek_nth(0).unwrap().kind {
        Some(Box::new(parse_exact_elseexpr(tkiter, dlogger)))
    } else {
        None
    };

    // rightmost boundary of range
    let rrange = match else_branch {
        Some(x) => x.range,
        None => lrange,
    };

    (
        union_of(lrange, rrange),
        metadata,
        cond,
        then_branch,
        else_branch,
    )
}

// parses an else expression
fn parse_exact_elseexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ElseExpr> {
    let metadata = get_metadata(tkiter);
    let Token {
        range: lrange,
        kind,
    } = exact_token(tkiter, TokenKind::Else);

    match tkiter.peek_nth(0).unwrap().kind {
        Some(TokenKind::If) => {
            let (rrange, _, cond, then_branch, else_branch) = parse_exact_if(tkiter, dlogger);
            Augmented {
                metadata,
                range: union_of(lrange, rrange),
                val: ElseExpr::Elif {
                    cond,
                    then_branch,
                    else_branch,
                },
            }
        }
        _ => {
            let block = Box::new(parse_blockexpr(tkiter, dlogger));
            Augmented {
                metadata,
                range: union_of(lrange, block.range),
                val: ElseExpr::Else(block),
            }
        }
    }
}

// parses an if or panics
fn parse_exact_valexpr_if<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let (range, metadata, cond, then_branch, else_branch) = parse_exact_if(tkiter, dlogger);
    Augmented {
        metadata,
        range,
        val: ValExpr::IfThen {
            cond,
            then_branch,
            else_branch,
        },
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
        TokenKind::Case => Some(parse_exact_valexpr_case::<TkIter>),
        TokenKind::If => Some(parse_exact_valexpr_if::<TkIter>),
        TokenKind::Identifier(_) => Some(parse_exact_valexpr_identifier::<TkIter>),
        _ => None,
    }
}

// parses basic term
fn parse_valexpr_term<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    let Token {
        kind: maybe_kind,
        range,
    } = peek_past_metadata(tkiter);

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

fn parse_exact_valexpr_postfix_apply<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    prefix: Augmented<ValExpr>,
) -> Augmented<ValExpr> {
    let args = Box::new(parse_args_expr(tkiter, dlogger, parse_valexpr));
    Augmented {
        metadata: vec![],
        range: union_of(prefix.range, args.range),
        val: ValExpr::App {
            args,
            root: Box::new(prefix),
        },
    }
}

fn parse_exact_valexpr_postfix_concretize<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    prefix: Augmented<ValExpr>,
) -> Augmented<ValExpr> {
    let tyargs = Box::new(parse_tyargs_expr(tkiter, dlogger, parse_typeexpr));

    Augmented {
        metadata: vec![],
        range: tyargs.range,
        val: ValExpr::Concretize {
            root: Box::new(prefix),
            tyargs,
        },
    }
}

fn parse_exact_valexpr_postfix_fieldaccess_or_arrayaccess<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    prefix: Augmented<ValExpr>,
) -> Augmented<ValExpr> {
    let dot_tk = exact_token(tkiter, TokenKind::ModuleAccess);
    let next_tk = tkiter.next().unwrap();
    match next_tk.kind {
        Some(TokenKind::Identifier(field)) => Augmented {
            metadata: vec![],
            range: union_of(dot_tk.range, next_tk.range),
            val: ValExpr::FieldAccess {
                root: Box::new(prefix),
                field,
            },
        },
        Some(TokenKind::BracketLeft) => {
            let index = Box::new(parse_valexpr(tkiter, dlogger));
            let bracket_right = exact_token(tkiter, TokenKind::BracketRight);
            Augmented {
                metadata: vec![],
                range: union_of(dot_tk.range, bracket_right.range),
                val: ValExpr::ArrayAccess {
                    root: Box::new(prefix),
                    index,
                },
            }
        }
        k => {
            dlogger.log_unexpected_token_specific(
                next_tk.range,
                "field access or array access",
                vec![],
                k,
            );
            Augmented {
                metadata: vec![],
                range: next_tk.range,
                val: ValExpr::Error,
            }
        }
    }
}

fn parse_valexpr_postfix_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    parse_postfix_op(tkiter, dlogger, parse_valexpr_term, |t| match t {
        Some(TokenKind::ModuleAccess) => Some(Box::new(
            parse_exact_valexpr_postfix_fieldaccess_or_arrayaccess,
        )),
        Some(TokenKind::ParenLeft) => Some(Box::new(parse_exact_valexpr_postfix_apply)),
        Some(TokenKind::BracketLeft) => Some(Box::new(parse_exact_valexpr_postfix_concretize)),
        Some(TokenKind::Ref) => Some(Box::new(
            |tkiter: &mut PeekMoreIterator<TkIter>,
             dlogger: &mut DiagnosticLogger,
             prefix: Augmented<ValExpr>| {
                let tk = tkiter.next().unwrap();
                Augmented {
                    metadata: vec![],
                    range: union_of(prefix.range, tk.range),
                    val: ValExpr::Ref(Box::new(prefix)),
                }
            },
        )),
        Some(TokenKind::Deref) => Some(Box::new(
            |tkiter: &mut PeekMoreIterator<TkIter>,
             dlogger: &mut DiagnosticLogger,
             prefix: Augmented<ValExpr>| {
                let tk = tkiter.next().unwrap();
                Augmented {
                    metadata: vec![],
                    range: union_of(prefix.range, tk.range),
                    val: ValExpr::Deref(Box::new(prefix)),
                }
            },
        )),
        _ => None,
    })
}

fn parse_valexpr_multiplication_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<ValExpr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_valexpr_postfix_operators,
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
    parse_valexpr_pipe(tkiter, dlogger)
}

fn parse_patexpr_term<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<PatExpr> {
    let metadata = get_metadata(tkiter);
    match tkiter.peek_nth(0).unwrap().kind {
        Some(TokenKind::Ignore) => {
            let tk = tkiter.next().unwrap();
            Augmented {
                range: tk.range,
                metadata,
                val: PatExpr::Ignore,
            }
        }
        Some(TokenKind::Mut) => {
            let mut_tk = tkiter.next().unwrap();
            assert!(mut_tk.kind == Some(TokenKind::Mut));
            let (identifier, identifier_range) =
                expect_identifier(tkiter, dlogger, "mutable identifier pattern");
            Augmented {
                range: union_of(mut_tk.range, identifier_range),
                metadata,
                val: PatExpr::Identifier {
                    identifier,
                    mutable: true,
                },
            }
        }
        Some(TokenKind::Identifier(identifier)) => {
            let tk = tkiter.next().unwrap();
            Augmented {
                range: tk.range,
                metadata,
                val: PatExpr::Identifier {
                    identifier,
                    mutable: false,
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

fn parse_patexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<PatExpr> {
    parse_postfix_op(tkiter, dlogger, parse_patexpr_term, |t| match t {
        Some(TokenKind::Constrain) => Some(Box::new(
            |tkiter: &mut PeekMoreIterator<TkIter>,
             dlogger: &mut DiagnosticLogger,
             prefix: Augmented<PatExpr>| {
                let ty = Box::new(parse_typeexpr(tkiter, dlogger));
                Augmented {
                    metadata: vec![],
                    range: union_of(prefix.range, ty.range),
                    val: PatExpr::Typed {
                        pat: Box::new(prefix),
                        ty,
                    },
                }
            },
        )),
        _ => None,
    })
}

fn parse_args_expr<TkIter: Iterator<Item = Token>, T>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<T>,
) -> Augmented<ArgsExpr<T>> {
    let (range, metadata, args, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "arguments",
        lower_fn,
        TokenKind::ParenLeft,
        TokenKind::ParenRight,
        TokenKind::Comma,
    );

    Augmented {
        range,
        metadata,
        val: ArgsExpr { args },
    }
}

fn parse_tyargs_expr<TkIter: Iterator<Item = Token>, T>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<T>,
) -> Augmented<ArgsExpr<T>> {
    let (range, metadata, args, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "type arguments",
        lower_fn,
        TokenKind::BracketLeft,
        TokenKind::BracketRight,
        TokenKind::Comma,
    );

    Augmented {
        range,
        metadata,
        val: ArgsExpr { args },
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
    Option<Box<Augmented<ArgsExpr<TypePatExpr>>>>,
    Box<Augmented<ArgsExpr<PatExpr>>>,
    Box<Augmented<TypeExpr>>,
    Box<Augmented<BlockExpr>>,
) {
    let metadata = get_metadata(tkiter);
    let fn_tk = tkiter.next().unwrap();
    assert!(fn_tk.kind == Some(TokenKind::Fn));
    let (identifier, _) = expect_identifier(tkiter, dlogger, "function definition");
    let tyargs = match tkiter.peek_nth(0).unwrap().kind {
        Some(TokenKind::BracketLeft) => Some(Box::new(parse_tyargs_expr(
            tkiter,
            dlogger,
            parse_typepatexpr,
        ))),
        _ => None,
    };
    let args = Box::new(parse_args_expr(tkiter, dlogger, parse_patexpr));
    expect_token(
        tkiter,
        dlogger,
        "function definition",
        vec![TokenKind::Defun],
    );
    let ty = Box::new(parse_typeexpr(tkiter, dlogger));
    let body = Box::new(parse_blockexpr(tkiter, dlogger));
    (
        union_of(fn_tk.range, body.range),
        metadata,
        identifier,
        tyargs,
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
    let (range, metadata, identifier, tyargs, args, returntype, body) =
        parse_exact_fn(tkiter, dlogger);
    Augmented {
        range,
        metadata,
        val: BlockStatement::FnDef {
            tyargs,
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
    let place = Box::new(parse_valexpr(tkiter, dlogger));
    expect_token(tkiter, dlogger, "set statement", vec![TokenKind::Assign]);
    let value = Box::new(parse_valexpr(tkiter, dlogger));

    Augmented {
        metadata,
        range: union_of(set_tk.range, value.range),
        val: BlockStatement::Set { place, value },
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

    let body = Box::new(parse_blockexpr(tkiter, dlogger));

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

    expect_token(tkiter, dlogger, "for loop", vec![TokenKind::In]);

    let range = Box::new(parse_rangeexpr(tkiter, dlogger));

    let by = if tkiter.peek_nth(0).unwrap().kind == Some(TokenKind::By) {
        let _ = tkiter.next().unwrap();
        Some(Box::new(parse_valexpr_term(tkiter, dlogger)))
    } else {
        None
    };

    let body = Box::new(parse_blockexpr(tkiter, dlogger));

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

fn parse_exact_use<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> (Range, Vec<Metadata>, Vec<u8>) {
    let metadata = get_metadata(tkiter);
    let use_tk = tkiter.next().unwrap();
    assert!(use_tk.kind == Some(TokenKind::Use));

    let (identifier, id_tk) = expect_identifier(tkiter, dlogger, "use statement");
    (union_of(use_tk.range, id_tk), metadata, identifier)
}

fn parse_exact_blockstatement_use<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let (range, metadata, prefix) = parse_exact_use(tkiter, dlogger);
    Augmented {
        range,
        metadata,
        val: BlockStatement::Use { prefix },
    }
}

// parses a type declaration or panics
fn parse_exact_typedef<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> (Range, Vec<Metadata>, Vec<u8>, Box<Augmented<TypeExpr>>) {
    let metadata = get_metadata(tkiter);
    let type_tk = tkiter.next().unwrap();
    assert!(type_tk.kind == Some(TokenKind::Type));

    let (identifier, _) = expect_identifier(tkiter, dlogger, "type definition statement");

    expect_token(
        tkiter,
        dlogger,
        "type definition statement",
        vec![TokenKind::Assign],
    );

    let value = Box::new(parse_typeexpr(tkiter, dlogger));

    (
        union_of(type_tk.range, value.range),
        metadata,
        identifier,
        value,
    )
}

// parses a type declaration or panics
fn parse_exact_blockstatement_typedef<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let (range, metadata, identifier, value) = parse_exact_typedef(tkiter, dlogger);
    Augmented {
        metadata,
        range,
        val: BlockStatement::TypeDef { identifier, value },
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
    let Token {
        kind: maybe_kind,
        range,
    } = peek_past_metadata(tkiter);

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

// parses an exact reference or panics
fn parse_exact_typeexpr_identifier<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Identifier(identifier)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: TypeExpr::Identifier(identifier),
        }
    } else {
        unimplemented!()
    }
}

// parses an exact reference or panics
fn parse_exact_typeexpr_bool<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Bool(bool)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: TypeExpr::Bool(bool),
        }
    } else {
        unimplemented!()
    }
}

// parses an exact reference or panics
fn parse_exact_typeexpr_float<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Float(float)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: TypeExpr::Float(float),
        }
    } else {
        unimplemented!()
    }
}

// parses an exact reference or panics
fn parse_exact_typeexpr_int<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Int(int)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: TypeExpr::Int(int),
        }
    } else {
        unimplemented!()
    }
}

// parses an exact unit or panics
fn parse_exact_typeexpr_unitty<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let tk = exact_token(tkiter, TokenKind::Unit);
    Augmented {
        metadata,
        range: tk.range,
        val: TypeExpr::UnitTy,
    }
}

// parses an exact bool or panics
fn parse_exact_typeexpr_boolty<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let tk = exact_token(tkiter, TokenKind::BoolTy);
    Augmented {
        metadata,
        range: tk.range,
        val: TypeExpr::BoolTy,
    }
}

// parses an exact int or panics
fn parse_exact_typeexpr_intty<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let tk = exact_token(tkiter, TokenKind::IntTy);
    Augmented {
        metadata,
        range: tk.range,
        val: TypeExpr::IntTy,
    }
}

// parses an exact uint or panics
fn parse_exact_typeexpr_uintty<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let tk = exact_token(tkiter, TokenKind::UIntTy);
    Augmented {
        metadata,
        range: tk.range,
        val: TypeExpr::UIntTy,
    }
}

// parses an exact float or panics
fn parse_exact_typeexpr_floatty<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let tk = exact_token(tkiter, TokenKind::FloatTy);
    Augmented {
        metadata,
        range: tk.range,
        val: TypeExpr::FloatTy,
    }
}

// parses an exact array or panics
fn parse_exact_typeexpr_arrayty<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let tk = exact_token(tkiter, TokenKind::ArrayTy);
    Augmented {
        metadata,
        range: tk.range,
        val: TypeExpr::ArrayTy,
    }
}

// parses an exact slice or panics
fn parse_exact_typeexpr_slicety<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let tk = exact_token(tkiter, TokenKind::SliceTy);
    Augmented {
        metadata,
        range: tk.range,
        val: TypeExpr::SliceTy,
    }
}

fn parse_exact_typeexpr_group<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let left_tk = exact_token(tkiter, TokenKind::ParenLeft);
    let val = Box::new(parse_typeexpr(tkiter, dlogger));
    let right_tk = expect_token(
        tkiter,
        dlogger,
        "group expression",
        vec![TokenKind::ParenRight],
    );
    Augmented {
        range: union_of(left_tk.range, right_tk.range),
        metadata,
        val: TypeExpr::Group(val),
    }
}

fn parse_exact_typeexpr_struct<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let (range, metadata, statements, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "struct definition",
        parse_struct_item_expr(parse_typeexpr),
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Comma,
    );

    Augmented {
        range,
        metadata,
        val: TypeExpr::Struct(statements),
    }
}

fn parse_exact_typeexpr_enum<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let enum_tk = exact_token(tkiter, TokenKind::Enum);

    let (range, _, statements, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "enum definition",
        parse_struct_item_expr(parse_typeexpr),
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Comma,
    );

    Augmented {
        range: union_of(enum_tk.range, range),
        metadata,
        val: TypeExpr::Struct(statements),
    }
}

fn parse_exact_typeexpr_union<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let union_tk = exact_token(tkiter, TokenKind::Union);

    let (range, _, statements, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "union definition",
        parse_struct_item_expr(parse_typeexpr),
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Comma,
    );

    Augmented {
        range: union_of(union_tk.range, range),
        metadata,
        val: TypeExpr::Struct(statements),
    }
}

fn parse_exact_typeexpr_fn<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let metadata = get_metadata(tkiter);
    let fn_tk = exact_token(tkiter, TokenKind::Fn);
    let args = Box::new(parse_args_expr(tkiter, dlogger, parse_typeexpr));
    expect_token(
        tkiter,
        dlogger,
        "type function definition",
        vec![TokenKind::Defun],
    );
    let returntype = Box::new(parse_typeexpr(tkiter, dlogger));
    Augmented {
        metadata,
        range: union_of(fn_tk.range, returntype.range),
        val: TypeExpr::Fn { args, returntype },
    }
}

pub fn parse_typeexpr_term<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    let Token {
        kind: maybe_kind,
        range,
    } = peek_past_metadata(tkiter);

    match maybe_kind {
        Some(TokenKind::Identifier(_)) => parse_exact_typeexpr_identifier(tkiter, dlogger),
        Some(TokenKind::Unit) => parse_exact_typeexpr_unitty(tkiter, dlogger),
        Some(TokenKind::BoolTy) => parse_exact_typeexpr_boolty(tkiter, dlogger),
        Some(TokenKind::IntTy) => parse_exact_typeexpr_intty(tkiter, dlogger),
        Some(TokenKind::UIntTy) => parse_exact_typeexpr_uintty(tkiter, dlogger),
        Some(TokenKind::FloatTy) => parse_exact_typeexpr_floatty(tkiter, dlogger),
        Some(TokenKind::ArrayTy) => parse_exact_typeexpr_arrayty(tkiter, dlogger),
        Some(TokenKind::SliceTy) => parse_exact_typeexpr_slicety(tkiter, dlogger),
        Some(TokenKind::Bool(_)) => parse_exact_typeexpr_bool(tkiter, dlogger),
        Some(TokenKind::Int(_)) => parse_exact_typeexpr_int(tkiter, dlogger),
        Some(TokenKind::Float(_)) => parse_exact_typeexpr_float(tkiter, dlogger),
        Some(TokenKind::BraceLeft) => parse_exact_typeexpr_struct(tkiter, dlogger),
        Some(TokenKind::Enum) => parse_exact_typeexpr_enum(tkiter, dlogger),
        Some(TokenKind::Union) => parse_exact_typeexpr_union(tkiter, dlogger),
        Some(TokenKind::ParenLeft) => parse_exact_typeexpr_group(tkiter, dlogger),
        Some(TokenKind::Fn) => parse_exact_typeexpr_fn(tkiter, dlogger),
        k => {
            // grab metadata
            let metadata = get_metadata(tkiter);
            dlogger.log_unexpected_token_specific(range, "type expression", vec![], k);
            Augmented {
                range,
                metadata,
                val: TypeExpr::Error,
            }
        }
    }
}

pub fn parse_typeexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypeExpr> {
    parse_postfix_op(tkiter, dlogger, parse_typeexpr_term, |t| match t {
        // evaluate a generic
        Some(TokenKind::BracketLeft) => Some(Box::new(
            |tkiter: &mut PeekMoreIterator<TkIter>,
             dlogger: &mut DiagnosticLogger,
             prefix: Augmented<TypeExpr>| {
                let args = Box::new(parse_args_expr(tkiter, dlogger, parse_typeexpr));
                Augmented {
                    metadata: vec![],
                    range: union_of(prefix.range, args.range),
                    val: TypeExpr::Generic {
                        fun: Box::new(prefix),
                        args,
                    },
                }
            },
        )),
        // reference of a type
        Some(TokenKind::Ref) => Some(Box::new(
            |tkiter: &mut PeekMoreIterator<TkIter>,
             dlogger: &mut DiagnosticLogger,
             prefix: Augmented<TypeExpr>| {
                let tk = tkiter.next().unwrap();
                Augmented {
                    metadata: vec![],
                    range: union_of(prefix.range, tk.range),
                    val: TypeExpr::Ref(Box::new(prefix)),
                }
            },
        )),
        _ => None,
    })
}

fn parse_kind_expr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<KindExpr> {
    let Token {
        kind: maybe_kind,
        range,
    } = peek_past_metadata(tkiter);

    match maybe_kind {
        Some(TokenKind::IntKind) => {
            let metadata = get_metadata(tkiter);
            let Token { range, .. } = exact_token(tkiter, TokenKind::IntKind);
            Augmented {
                range,
                metadata,
                val: KindExpr::Int,
            }
        }
        Some(TokenKind::UIntKind) => {
            let metadata = get_metadata(tkiter);
            let Token { range, .. } = exact_token(tkiter, TokenKind::UIntKind);
            Augmented {
                range,
                metadata,
                val: KindExpr::UInt,
            }
        }
        Some(TokenKind::FloatKind) => {
            let metadata = get_metadata(tkiter);
            let Token { range, .. } = exact_token(tkiter, TokenKind::FloatKind);
            Augmented {
                range,
                metadata,
                val: KindExpr::Float,
            }
        }
        Some(TokenKind::BoolKind) => {
            let metadata = get_metadata(tkiter);
            let Token { range, .. } = exact_token(tkiter, TokenKind::BoolKind);
            Augmented {
                range,
                metadata,
                val: KindExpr::Bool,
            }
        }
        Some(TokenKind::BracketLeft) => {
            let metadata = get_metadata(tkiter);
            let (range, metadata, args, _) = parse_delimited_statement_seq(
                tkiter,
                dlogger,
                "type arguments",
                parse_kind_expr,
                TokenKind::BracketLeft,
                TokenKind::BracketRight,
                TokenKind::Comma,
            );
            let _ = expect_token(tkiter, dlogger, "kind expression", vec![TokenKind::Defun]);
            let returnkind = Box::new(parse_kind_expr(tkiter, dlogger));
            Augmented {
                range: union_of(range, returnkind.range),
                metadata,
                val: KindExpr::GenericFn { args, returnkind },
            }
        }
        k => {
            let metadata = get_metadata(tkiter);
            dlogger.log_unexpected_token_specific(
                range,
                "type pattern expression",
                vec![TokenKind::Identifier(vec![])],
                k,
            );
            Augmented {
                range,
                metadata,
                val: KindExpr::Error,
            }
        }
    }
}

fn parse_typepatexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<TypePatExpr> {
    let metadata = get_metadata(tkiter);
    let Token {
        kind: maybe_kind,
        range,
    } = tkiter.peek_nth(0).unwrap().clone();

    match maybe_kind {
        Some(TokenKind::Identifier(_)) => {
            let (identifier, range) = expect_identifier(tkiter, dlogger, "type patern expression");
            match tkiter.peek_nth(0).unwrap().kind {
                Some(TokenKind::Constrain) => {
                    tkiter.next().unwrap();
                    let kind = Box::new(parse_kind_expr(tkiter, dlogger));
                    Augmented {
                        range,
                        metadata,
                        val: TypePatExpr::Identifier {
                            identifier,
                            kind: Some(kind),
                        },
                    }
                }
                _ => Augmented {
                    range,
                    metadata,
                    val: TypePatExpr::Identifier {
                        identifier,
                        kind: None,
                    },
                },
            }
        }
        k => {
            dlogger.log_unexpected_token_specific(
                range,
                "type pattern expression",
                vec![TokenKind::Identifier(vec![])],
                k,
            );
            Augmented {
                range,
                metadata,
                val: TypePatExpr::Error,
            }
        }
    }
}

// parses a function or panics
fn parse_exact_filestatement_fndef<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<FileStatement> {
    let (range, metadata, identifier, tyargs, args, returntype, body) =
        parse_exact_fn(tkiter, dlogger);
    Augmented {
        range,
        metadata,
        val: FileStatement::FnDef {
            tyargs,
            args,
            body,
            identifier,
            returntype,
        },
    }
}

// parses a let or panics
fn parse_exact_filestatement_let<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<FileStatement> {
    let (range, metadata, pattern, value) = parse_exact_let(tkiter, dlogger);
    Augmented {
        metadata,
        range,
        val: FileStatement::Let { pattern, value },
    }
}

// parses a let or panics
fn parse_exact_filestatement_typedef<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<FileStatement> {
    let (range, metadata, identifier, value) = parse_exact_typedef(tkiter, dlogger);
    Augmented {
        metadata,
        range,
        val: FileStatement::TypeDef { identifier, value },
    }
}

pub fn parse_exact_filestatement_prefix<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<FileStatement> {
    let metadata = get_metadata(tkiter);
    let prefix_tk = tkiter.next().unwrap();
    assert!(prefix_tk.kind == Some(TokenKind::Prefix));

    let (prefix, _) = expect_identifier(tkiter, dlogger, "prefix block");

    let (range, metadata, items, _) = parse_delimited_statement_seq_opt_sep(
        tkiter,
        dlogger,
        "prefix block",
        parse_filestatement,
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Semicolon,
    );

    Augmented {
        metadata,
        range: union_of(prefix_tk.range, range),
        val: FileStatement::Prefix { prefix, items },
    }
}

fn parse_exact_filestatement_use<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<FileStatement> {
    let (range, metadata, prefix) = parse_exact_use(tkiter, dlogger);
    Augmented {
        range,
        metadata,
        val: FileStatement::Use { prefix },
    }
}

pub fn parse_filestatement<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<FileStatement> {
    let Token {
        kind: maybe_kind,
        range,
    } = peek_past_metadata(tkiter);

    match maybe_kind {
        Some(TokenKind::Type) => parse_exact_filestatement_typedef(tkiter, dlogger),
        Some(TokenKind::Let) => parse_exact_filestatement_let(tkiter, dlogger),
        Some(TokenKind::Fn) => parse_exact_filestatement_fndef(tkiter, dlogger),
        Some(TokenKind::Prefix) => parse_exact_filestatement_prefix(tkiter, dlogger),
        Some(TokenKind::Use) => parse_exact_filestatement_use(tkiter, dlogger),
        k => {
            // grab metadata
            let metadata = get_metadata(tkiter);
            dlogger.log_unexpected_token_specific(range, "file statement", vec![], k);
            Augmented {
                range,
                val: FileStatement::Error,
                metadata,
            }
        }
    }
}

pub fn parse_translationunit<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> TranslationUnit {
    let mut statements = vec![];

    let sep_tok = TokenKind::Semicolon;

    loop {
        // if next token is none, break
        if tkiter.peek_nth(0).unwrap().kind == None {
            break;
        }

        // parse a statement
        statements.push(parse_filestatement(tkiter, dlogger));

        // parse sep (if it exists)
        let Token { range, kind } = tkiter.next().unwrap();
        if tkiter.peek_nth(0).unwrap().kind == Some(sep_tok) {
            tkiter.next();
        }
    }

    TranslationUnit {
        declarations: statements,
    }
}

pub fn construct_ast<TkIterSource: IntoIterator<Item = Token>>(
    tokens: TkIterSource,
    mut dlogger: DiagnosticLogger,
) -> TranslationUnit {
    parse_translationunit(&mut tokens.into_iter().peekmore(), &mut dlogger)
}
