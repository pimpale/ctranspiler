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
    if let Some(ref tkk) = tk.kind {
        if expected_tokens.contains(tkk) {
            return tk;
        }
    }
    dlogger.log_unexpected_token_specific(tk.range, structure, expected_tokens, &tk.kind);
    return tk;
}

fn expect_identifier<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    structure: &str,
) -> Identifier {
    let tk = tkiter.next().unwrap();
    if let Some(TokenKind::Identifier(id)) = tk.kind {
        return Identifier {
            identifier: Some(id),
            range: tk.range,
        };
    }
    dlogger.log_unexpected_token_specific(
        tk.range,
        structure,
        vec![TokenKind::Identifier(String::new())],
        &tk.kind,
    );
    return Identifier {
        identifier: None,
        range: tk.range,
    };
}

fn expect_label<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    structure: &str,
) -> Label {
    let tk = tkiter.next().unwrap();
    if let Some(TokenKind::Label(id)) = tk.kind {
        return Label {
            label: Some(id),
            range: tk.range,
        };
    }
    dlogger.log_unexpected_token_specific(
        tk.range,
        structure,
        vec![TokenKind::Label(String::new())],
        &tk.kind,
    );
    return Label {
        label: None,
        range: tk.range,
    };
}

fn parse_l_binary_op<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<Expr>,
    operator_fn: impl Fn(
        &mut PeekMoreIterator<TkIter>,
        &mut DiagnosticLogger,
    ) -> Option<(ValBinaryOpKind, Range)>,
) -> Augmented<Expr> {
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
                val: Expr::BinaryOp {
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
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<Expr>,
    operator_fn: impl Fn(
        &mut PeekMoreIterator<TkIter>,
        &mut DiagnosticLogger,
    ) -> Option<(ValBinaryOpKind, Range)>,
) -> Augmented<Expr> {
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
            val: Expr::BinaryOp {
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
        if let Some(operator_fn) = decide_fn(tkiter.peek_nth(0).unwrap().kind.clone()) {
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
) -> (Range, Vec<Augmented<T>>, bool) {
    let Token { range: lrange, .. } = expect_token(tkiter, dlogger, structure, vec![start_tok]);

    let rrange;

    let mut statements = vec![];

    // test match token to see if it sep or end
    let has_ending_sep = loop {
        // if next token is closing delimiter, break
        if tkiter.peek_nth(0).unwrap().kind.as_ref() == Some(&end_tok) {
            let Token { range, .. } = tkiter.next().unwrap();
            rrange = range;
            break false;
        }

        // parse a statement
        statements.push(parser_fn(tkiter, dlogger));

        // parse sep and potentially closing delimiter
        let Token { range, kind } = tkiter.next().unwrap();
        if kind.as_ref() == Some(&sep_tok) {
            // if sep, check next
            if tkiter.peek_nth(0).unwrap().kind.as_ref() == Some(&end_tok) {
                let Token { range, .. } = tkiter.next().unwrap();
                rrange = range;
                break true;
            }
        } else if kind.as_ref() == Some(&end_tok) {
            rrange = range;
            break false;
        } else {
            dlogger.log_unexpected_token_specific(
                range,
                structure,
                vec![sep_tok.clone(), end_tok.clone()],
                &kind,
            );
        }
    };

    (union_of(lrange, rrange), statements, has_ending_sep)
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
) -> (Range, Vec<Augmented<T>>, bool) {
    let Token {
        range: lrange,
        kind,
    } = expect_token(tkiter, dlogger, structure, vec![start_tok]);

    let rrange;

    let mut statements = vec![];

    // test match token to see if it sep or end
    let has_ending_sep = loop {
        // if next token is closing delimiter, break
        if tkiter.peek_nth(0).unwrap().kind.as_ref() == Some(&end_tok) {
            let Token { range, .. } = tkiter.next().unwrap();
            rrange = range;
            break false;
        }

        // parse a statement
        statements.push(parser_fn(tkiter, dlogger));

        // parse sep and potentially closing delimiter
        match tkiter.peek_nth(0).unwrap().kind.as_ref() {
            k if k == Some(&sep_tok) => {
                tkiter.next();
                // if sep, check next
                if tkiter.peek_nth(0).unwrap().kind.as_ref() == Some(&end_tok) {
                    let Token { range, .. } = tkiter.next().unwrap();
                    rrange = range;
                    break true;
                }
            }
            _k if kind.as_ref() == Some(&end_tok) => {
                rrange = tkiter.next().unwrap().range;
                break false;
            }
            _ => (),
        }
    };

    (union_of(lrange, rrange), statements, has_ending_sep)
}

fn parse_struct_item_expr<TkIter: Iterator<Item = Token>, T>(
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<T>,
) -> impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<StructItemExpr<T>> {
    move |tkiter: &mut PeekMoreIterator<TkIter>, dlogger: &mut DiagnosticLogger| {
        let metadata = get_metadata(tkiter);
        let Token { range, kind } = tkiter.next().unwrap();
        if let Some(TokenKind::Identifier(identifier)) = kind {
            let identifier = Identifier {
                identifier: Some(identifier),
                range,
            };

            if let Token {
                kind: Some(TokenKind::ConstrainType),
                ..
            } = tkiter.peek_nth(0).unwrap()
            {
                tkiter.next();

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
                vec![TokenKind::Identifier(String::new())],
                &kind,
            );
            return Augmented {
                metadata,
                range,
                val: StructItemExpr::Error,
            };
        }
    }
}

fn parse_exact_expr_new_literal<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);

    let new_tk = exact_token(tkiter, TokenKind::New);

    let ty = Box::new(parse_expr(tkiter, dlogger));

    let val = Box::new(parse_expr_term(tkiter, dlogger));

    Augmented {
        range: union_of(new_tk.range, val.range),
        metadata,
        val: Expr::New { ty, val },
    }
}
fn parse_exact_expr_struct_literal<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);

    let (range, items, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "struct literal",
        parse_struct_item_expr(parse_expr),
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Comma,
    );

    Augmented {
        range,
        metadata,
        val: Expr::StructLiteral(items),
    }
}

fn parse_exact_expr_group<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let left_tk = exact_token(tkiter, TokenKind::ParenLeft);
    let val = Box::new(parse_expr(tkiter, dlogger));
    let right_tk = expect_token(
        tkiter,
        dlogger,
        "group expression",
        vec![TokenKind::ParenRight],
    );
    Augmented {
        range: union_of(left_tk.range, right_tk.range),
        metadata,
        val: Expr::Group(val),
    }
}

fn parse_exact_expr_block<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let label = expect_label(tkiter, dlogger, "block expression");

    let (block_range, statements, trailing_semicolon) = parse_delimited_statement_seq_opt_sep(
        tkiter,
        dlogger,
        "block expression",
        parse_blockstatement,
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Semicolon,
    );

    Augmented {
        range: union_of(label.range, block_range),
        metadata,
        val: Expr::Block {
            label,
            statements,
            trailing_semicolon,
        },
    }
}

// parses an exact reference or panics
fn parse_exact_expr_identifier<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let identifier = expect_identifier(tkiter, dlogger, "identifier");
    Augmented {
        range: identifier.range,
        metadata,
        val: Expr::Identifier(identifier),
    }
}

fn parse_casetargetexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<CaseTargetExpr> {
    let metadata = get_metadata(tkiter);
    let Token { kind, .. } = tkiter.peek_nth(0).unwrap().clone();
    let (range, val) = match kind {
        Some(TokenKind::Bool(x)) => {
            let Token { range, .. } = tkiter.next().unwrap();
            (range, CaseTargetExpr::Bool(x))
        }
        Some(TokenKind::Int(x)) => {
            let Token { range, .. } = tkiter.next().unwrap();
            (range, CaseTargetExpr::Int(x))
        }
        _ => {
            let pat = Box::new(parse_patexpr(tkiter, dlogger));
            (pat.range, CaseTargetExpr::PatExpr(pat))
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
    let body = Box::new(parse_expr(tkiter, dlogger));
    Augmented {
        metadata,
        range: union_of(target.range, body.range),
        val: CaseExpr { target, body },
    }
}

// parses a case or panics
fn parse_exact_expr_case<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let Token { range: lrange, .. } = exact_token(tkiter, TokenKind::Case);
    let expr = Box::new(parse_expr(tkiter, dlogger));
    let (range, cases, _) = parse_delimited_statement_seq(
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
        val: Expr::CaseOf { expr, cases },
    }
}

fn parse_exact_expr_extern<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let Token { range: lrange, .. } = exact_token(tkiter, TokenKind::Extern);
    let Token {
        kind,
        range: srange,
    } = tkiter.next().unwrap();
    let name = if let Some(TokenKind::String { value, .. }) = kind {
        value
    } else {
        dlogger.log_unexpected_token_specific(
            srange,
            "extern",
            vec![TokenKind::String {
                value: vec![],
                block: false,
            }],
            &kind,
        );
        vec![]
    };

    let ty = Box::new(parse_expr(tkiter, dlogger));
    Augmented {
        metadata,
        range: union_of(lrange, ty.range),
        val: Expr::Extern { ty, name },
    }
}

fn parse_exact_expr_if<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let Token { range: lrange, .. } = exact_token(tkiter, TokenKind::If);

    let cond = Box::new(parse_expr(tkiter, dlogger));
    let then_branch = Box::new(parse_expr(tkiter, dlogger));

    // add else branch if it exists
    let else_branch = if let Some(TokenKind::Else) = tkiter.peek_nth(0).unwrap().kind {
        tkiter.next();
        Some(Box::new(parse_expr(tkiter, dlogger)))
    } else {
        None
    };

    // rightmost boundary of range
    let rrange = match else_branch {
        Some(ref x) => x.range,
        None => lrange,
    };

    Augmented {
        range: union_of(lrange, rrange),
        metadata,
        val: Expr::If {
            cond,
            then_branch,
            else_branch,
        },
    }
}

fn parse_exact_expr_loop<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let Token { range: lrange, .. } = exact_token(tkiter, TokenKind::Loop);

    let label = expect_label(tkiter, dlogger, "loop expression");

    let body = Box::new(parse_expr(tkiter, dlogger));

    Augmented {
        range: union_of(lrange, body.range),
        metadata,
        val: Expr::Loop { label, body },
    }
}

fn parse_exact_expr_ret<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let Token { range: lrange, .. } = exact_token(tkiter, TokenKind::Ret);

    let label = expect_label(tkiter, dlogger, "ret expression");

    let value = Box::new(parse_expr(tkiter, dlogger));

    Augmented {
        range: union_of(lrange, value.range),
        metadata,
        val: Expr::Ret { label, value },
    }
}

// parses a string
fn parse_exact_expr_string<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::String { value, block }),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: Expr::String { value, block },
        }
    } else {
        unreachable!();
    }
}

// parses a int
fn parse_exact_expr_int<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Int(value)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: Expr::Int(value),
        }
    } else {
        unreachable!();
    }
}

// parses a rational
fn parse_exact_expr_rational<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Float(value)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: Expr::Float(value),
        }
    } else {
        unreachable!();
    }
}

// parses a int
fn parse_exact_expr_bool<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    _dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    if let Token {
        range,
        kind: Some(TokenKind::Bool(value)),
    } = tkiter.next().unwrap()
    {
        Augmented {
            metadata,
            range,
            val: Expr::Bool(value),
        }
    } else {
        unreachable!();
    }
}

fn decide_expr_term<'a, TkIter: Iterator<Item = Token> + 'a>(
    tkkind: &TokenKind,
) -> Option<Box<dyn Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<Expr> + 'a>>
{
    match *tkkind {
        TokenKind::Bool(_) => Some(Box::new(parse_exact_expr_bool::<TkIter>)),
        TokenKind::Int(_) => Some(Box::new(parse_exact_expr_int::<TkIter>)),
        TokenKind::Float(_) => Some(Box::new(parse_exact_expr_rational::<TkIter>)),
        TokenKind::String { .. } => Some(Box::new(parse_exact_expr_string::<TkIter>)),
        TokenKind::New => Some(Box::new(parse_exact_expr_new_literal::<TkIter>)),
        TokenKind::BraceLeft => Some(Box::new(parse_exact_expr_struct_literal::<TkIter>)),
        TokenKind::ParenLeft => Some(Box::new(parse_exact_expr_group::<TkIter>)),
        TokenKind::Label(_) => Some(Box::new(parse_exact_expr_block::<TkIter>)),
        TokenKind::Case => Some(Box::new(parse_exact_expr_case::<TkIter>)),
        TokenKind::If => Some(Box::new(parse_exact_expr_if::<TkIter>)),
        TokenKind::Loop => Some(Box::new(parse_exact_expr_loop::<TkIter>)),
        TokenKind::Ret => Some(Box::new(parse_exact_expr_ret::<TkIter>)),
        TokenKind::Fn => Some(Box::new(parse_exact_expr_fn::<TkIter>)),
        TokenKind::Identifier(_) => Some(Box::new(parse_exact_expr_identifier::<TkIter>)),
        TokenKind::Struct => Some(Box::new(parse_exact_expr_structty::<TkIter>)),
        TokenKind::Enum => Some(Box::new(parse_exact_expr_enumty::<TkIter>)),
        TokenKind::Union => Some(Box::new(parse_exact_expr_unionty::<TkIter>)),
        TokenKind::FnTy => Some(Box::new(parse_exact_expr_fnty::<TkIter>)),
        TokenKind::Generic => Some(Box::new(parse_exact_expr_generic::<TkIter>)),
        TokenKind::Extern => Some(Box::new(parse_exact_expr_extern::<TkIter>)),
        _ => None,
    }
}

// parses basic term
fn parse_expr_term<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let Token {
        kind: maybe_kind,
        range,
    } = peek_past_metadata(tkiter);

    if let Some(kind) = maybe_kind {
        if let Some(parser) = decide_expr_term(&kind) {
            parser(tkiter, dlogger)
        } else {
            // grab metadata
            let metadata = get_metadata(tkiter);
            // consume unexpected token
            dlogger.log_unexpected_token_specific(range, "valexpr term", vec![], &Some(kind));
            tkiter.next();
            Augmented {
                range,
                metadata,
                val: Expr::Error,
            }
        }
    } else {
        // grab metadata
        let metadata = get_metadata(tkiter);
        dlogger.log_unexpected_token_specific(range, "valexpr term", vec![], &None);
        Augmented {
            range,
            val: Expr::Error,
            metadata,
        }
    }
}

fn parse_exact_expr_postfix_apply<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    prefix: Augmented<Expr>,
) -> Augmented<Expr> {
    let (args, args_range) = parse_args_expr(tkiter, dlogger, parse_expr);
    Augmented {
        metadata: vec![],
        range: union_of(prefix.range, args_range),
        val: Expr::App {
            args,
            root: Box::new(prefix),
        },
    }
}

fn parse_exact_expr_postfix_concretize<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    prefix: Augmented<Expr>,
) -> Augmented<Expr> {
    let (tyargs, tyargs_range) = parse_tyargs_expr(tkiter, dlogger, parse_expr);

    Augmented {
        metadata: vec![],
        range: tyargs_range,
        val: Expr::Concretization {
            root: Box::new(prefix),
            tyargs,
        },
    }
}

fn parse_exact_expr_postfix_fieldaccess_or_arrayaccess<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    prefix: Augmented<Expr>,
) -> Augmented<Expr> {
    let dot_tk = exact_token(tkiter, TokenKind::ModuleAccess);
    let next_tk = tkiter.next().unwrap();
    match next_tk.kind {
        Some(TokenKind::Identifier(field)) => Augmented {
            metadata: vec![],
            range: union_of(dot_tk.range, next_tk.range),
            val: Expr::FieldAccess {
                root: Box::new(prefix),
                field,
            },
        },
        Some(TokenKind::BracketLeft) => {
            let index = Box::new(parse_expr(tkiter, dlogger));
            let bracket_right = exact_token(tkiter, TokenKind::BracketRight);
            Augmented {
                metadata: vec![],
                range: union_of(dot_tk.range, bracket_right.range),
                val: Expr::ArrayAccess {
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
                &k,
            );
            Augmented {
                metadata: vec![],
                range: next_tk.range,
                val: Expr::Error,
            }
        }
    }
}

fn parse_expr_postfix_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    parse_postfix_op(tkiter, dlogger, parse_expr_term, |t| match t {
        Some(TokenKind::ModuleAccess) => Some(Box::new(|tkiter, dlogger, prefix| {
            parse_exact_expr_postfix_fieldaccess_or_arrayaccess(tkiter, dlogger, prefix)
        })),
        Some(TokenKind::ParenLeft) => Some(Box::new(|tkiter, dlogger, prefix| {
            parse_exact_expr_postfix_apply(tkiter, dlogger, prefix)
        })),
        Some(TokenKind::BracketLeft) => Some(Box::new(|tkiter, dlogger, prefix| {
            parse_exact_expr_postfix_concretize(tkiter, dlogger, prefix)
        })),
        Some(TokenKind::Ref) => Some(Box::new(
            |tkiter: &mut PeekMoreIterator<TkIter>,
             _dlogger: &mut DiagnosticLogger,
             prefix: Augmented<Expr>| {
                let tk = tkiter.next().unwrap();
                Augmented {
                    metadata: vec![],
                    range: union_of(prefix.range, tk.range),
                    val: Expr::Ref(Box::new(prefix)),
                }
            },
        )),
        Some(TokenKind::Deref) => Some(Box::new(
            |tkiter: &mut PeekMoreIterator<TkIter>,
             _dlogger: &mut DiagnosticLogger,
             prefix: Augmented<Expr>| {
                let tk = tkiter.next().unwrap();
                Augmented {
                    metadata: vec![],
                    range: union_of(prefix.range, tk.range),
                    val: Expr::Deref(Box::new(prefix)),
                }
            },
        )),
        _ => None,
    })
}

fn parse_expr_multiplication_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_expr_postfix_operators,
        simple_operator_fn(|x| match x {
            TokenKind::Mul => Some(ValBinaryOpKind::Mul),
            TokenKind::Div => Some(ValBinaryOpKind::Div),
            TokenKind::Rem => Some(ValBinaryOpKind::Rem),
            _ => None,
        }),
    )
}

fn parse_expr_addition_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_expr_multiplication_operators,
        simple_operator_fn(|x| match x {
            TokenKind::Plus => Some(ValBinaryOpKind::Add),
            TokenKind::Minus => Some(ValBinaryOpKind::Sub),
            _ => None,
        }),
    )
}

fn parse_expr_compare_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_expr_addition_operators,
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

fn parse_expr_binary_bool_operators<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_expr_compare_operators,
        simple_operator_fn(|x| match x {
            TokenKind::And => Some(ValBinaryOpKind::And),
            TokenKind::Or => Some(ValBinaryOpKind::Or),
            _ => None,
        }),
    )
}

fn parse_expr_pipe<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_expr_binary_bool_operators,
        simple_operator_fn(|x| match x {
            TokenKind::Pipe => Some(ValBinaryOpKind::Pipe),
            _ => None,
        }),
    )
}

fn parse_expr_assign<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    parse_l_binary_op(
        tkiter,
        dlogger,
        parse_expr_pipe,
        simple_operator_fn(|x| match x {
            TokenKind::Assign => Some(ValBinaryOpKind::Assign),
            TokenKind::AddAssign => Some(ValBinaryOpKind::AssignAdd),
            TokenKind::SubAssign => Some(ValBinaryOpKind::AssignSub),
            TokenKind::MulAssign => Some(ValBinaryOpKind::AssignMul),
            TokenKind::DivAssign => Some(ValBinaryOpKind::AssignDiv),
            _ => None,
        }),
    )
}

fn parse_expr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    parse_expr_assign(tkiter, dlogger)
}

fn parse_patexpr_term<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<PatExpr> {
    let metadata = get_metadata(tkiter);
    match tkiter.peek_nth(0).unwrap().kind.as_ref() {
        Some(TokenKind::Ignore) => {
            let tk = tkiter.next().unwrap();
            Augmented {
                range: tk.range,
                metadata,
                val: PatExpr::Ignore,
            }
        }
        Some(TokenKind::Nominal) => {
            let nominal_tk = tkiter.next().unwrap();
            assert!(nominal_tk.kind == Some(TokenKind::Nominal));
            let identifier = expect_identifier(tkiter, dlogger, "nominal identifier pattern");
            Augmented {
                range: union_of(nominal_tk.range, identifier.range),
                metadata,
                val: PatExpr::Identifier {
                    identifier,
                    modifier: IdentifierModifier::Nominal,
                },
            }
        }
        Some(TokenKind::Mut) => {
            let mut_tk = tkiter.next().unwrap();
            assert!(mut_tk.kind == Some(TokenKind::Mut));
            let identifier = expect_identifier(tkiter, dlogger, "mutable identifier pattern");
            Augmented {
                range: union_of(mut_tk.range, identifier.range),
                metadata,
                val: PatExpr::Identifier {
                    identifier,
                    modifier: IdentifierModifier::Mutable,
                },
            }
        }
        Some(TokenKind::Identifier(identifier)) => {
            let identifier = identifier.clone();
            let tk = tkiter.next().unwrap();
            Augmented {
                range: tk.range,
                metadata,
                val: PatExpr::Identifier {
                    identifier: Identifier {
                        identifier: Some(identifier),
                        range: tk.range,
                    },
                    modifier: IdentifierModifier::None,
                },
            }
        }
        Some(TokenKind::BraceLeft) => {
            let (range, statements, _) = parse_delimited_statement_seq(
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
                    TokenKind::Identifier(String::new()),
                    TokenKind::Ignore,
                    TokenKind::BraceLeft,
                ],
                &tk.kind,
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
        Some(TokenKind::ConstrainType) => Some(Box::new(
            |tkiter: &mut PeekMoreIterator<TkIter>,
             dlogger: &mut DiagnosticLogger,
             prefix: Augmented<PatExpr>| {
                // discard constrain type token
                let _ = tkiter.next().unwrap();
                // parse type
                let ty = Box::new(parse_expr(tkiter, dlogger));
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
        Some(TokenKind::ConstrainKind) => Some(Box::new(
            |tkiter: &mut PeekMoreIterator<TkIter>,
             dlogger: &mut DiagnosticLogger,
             prefix: Augmented<PatExpr>| {
                // discard constrain kind token
                let _ = tkiter.next().unwrap();
                // parse kind
                let kind = Box::new(parse_kind_expr(tkiter, dlogger));
                Augmented {
                    metadata: vec![],
                    range: union_of(prefix.range, kind.range),
                    val: PatExpr::Kinded {
                        pat: Box::new(prefix),
                        kind,
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
) -> (Vec<Augmented<T>>, Range) {
    let (range, args, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "arguments",
        lower_fn,
        TokenKind::ParenLeft,
        TokenKind::ParenRight,
        TokenKind::Comma,
    );

    return (args, range);
}

fn parse_tyargs_expr<TkIter: Iterator<Item = Token>, T>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
    lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Augmented<T>,
) -> (Vec<Augmented<T>>, Range) {
    let (range, args, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "type arguments",
        lower_fn,
        TokenKind::BracketLeft,
        TokenKind::BracketRight,
        TokenKind::Comma,
    );

    return (args, range);
}

// parses a let (whether in a function body or out of a function body) or panics
fn parse_exact_valdef<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> (
    Range,
    Vec<Metadata>,
    Box<Augmented<PatExpr>>,
    Box<Augmented<Expr>>,
) {
    let metadata = get_metadata(tkiter);
    let let_tk = exact_token(tkiter, TokenKind::Let);

    let pattern = Box::new(parse_patexpr(tkiter, dlogger));
    expect_token(tkiter, dlogger, "let statement", vec![TokenKind::Define]);

    let value = Box::new(parse_expr(tkiter, dlogger));

    (
        union_of(let_tk.range, value.range),
        metadata,
        pattern,
        value,
    )
}

// parses a valdef or panics
fn parse_exact_blockstatement_valdef<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let (range, metadata, pat, value) = parse_exact_valdef(tkiter, dlogger);

    Augmented {
        metadata,
        range,
        val: BlockStatement::Let { pat, value },
    }
}

// parse range expression
fn parse_rangeexpr<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<RangeExpr> {
    let start = Box::new(parse_expr_term(tkiter, dlogger));

    let range_expr = tkiter.next().unwrap();
    let inclusive = match range_expr.kind {
        Some(TokenKind::Range) => false,
        Some(TokenKind::RangeInclusive) => true,
        k => {
            dlogger.log_unexpected_token_specific(
                range_expr.range,
                "range expression",
                vec![TokenKind::Range, TokenKind::RangeInclusive],
                &k,
            );
            false
        }
    };

    let end = Box::new(parse_expr_term(tkiter, dlogger));

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

fn parse_exact_use<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> (Range, Vec<Metadata>, Identifier) {
    let metadata = get_metadata(tkiter);
    let use_tk = exact_token(tkiter, TokenKind::Use);

    let identifier = expect_identifier(tkiter, dlogger, "use statement");
    (
        union_of(use_tk.range, identifier.range),
        metadata,
        identifier,
    )
}

fn parse_exact_blockstatement_use<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let (range, metadata, prefix) = parse_exact_use(tkiter, dlogger);
    Augmented {
        range,
        metadata,
        val: BlockStatement::Use { namespace: prefix },
    }
}

// parses a set or panics
fn parse_exact_blockstatement_do<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<BlockStatement> {
    let metadata = get_metadata(tkiter);

    let val = Box::new(parse_expr(tkiter, dlogger));

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
        Some(TokenKind::Let) => parse_exact_blockstatement_valdef(tkiter, dlogger),
        Some(_) => parse_exact_blockstatement_do(tkiter, dlogger),
        None => {
            // grab metadata
            let metadata = get_metadata(tkiter);
            dlogger.log_unexpected_token_specific(range, "block statement", vec![], &None);
            Augmented {
                range,
                val: BlockStatement::Error,
                metadata,
            }
        }
    }
}

fn parse_exact_expr_fn<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let fn_tk = exact_token(tkiter, TokenKind::Fn);

    let typarams = if tkiter.peek_nth(0).unwrap().kind == Some(TokenKind::BracketLeft) {
        Some(parse_tyargs_expr(tkiter, dlogger, parse_patexpr).0)
    } else {
        None
    };

    let (params, _) = parse_args_expr(tkiter, dlogger, parse_patexpr);

    let returnty = if tkiter.peek_nth(0).unwrap().kind == Some(TokenKind::ConstrainType) {
        let _ = tkiter.next().unwrap();
        Some(Box::new(parse_expr(tkiter, dlogger)))
    } else {
        None
    };

    expect_token(
        tkiter,
        dlogger,
        "function definition",
        vec![TokenKind::Defun],
    );

    let body = Box::new(parse_expr(tkiter, dlogger));
    Augmented {
        metadata,
        range: union_of(fn_tk.range, body.range),
        val: Expr::FnDef {
            typarams,
            params,
            returnty,
            body,
        },
    }
}

fn parse_exact_expr_structty<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let struct_tk = exact_token(tkiter, TokenKind::Struct);

    let (range, statements, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "struct definition",
        parse_struct_item_expr(parse_expr),
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Comma,
    );

    Augmented {
        range: union_of(struct_tk.range, range),
        metadata,
        val: Expr::StructTy(statements),
    }
}

fn parse_exact_expr_enumty<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let enum_tk = exact_token(tkiter, TokenKind::Enum);

    let (range, statements, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "enum definition",
        parse_struct_item_expr(parse_expr),
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Comma,
    );

    Augmented {
        range: union_of(enum_tk.range, range),
        metadata,
        val: Expr::EnumTy(statements),
    }
}

fn parse_exact_expr_unionty<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let union_tk = exact_token(tkiter, TokenKind::Union);

    let (range, statements, _) = parse_delimited_statement_seq(
        tkiter,
        dlogger,
        "union definition",
        parse_struct_item_expr(parse_expr),
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Comma,
    );

    Augmented {
        range: union_of(union_tk.range, range),
        metadata,
        val: Expr::UnionTy(statements),
    }
}

fn parse_exact_expr_fnty<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let fn_tk = exact_token(tkiter, TokenKind::FnTy);
    let (paramtys, _) = parse_args_expr(tkiter, dlogger, parse_expr);
    expect_token(
        tkiter,
        dlogger,
        "type function definition",
        vec![TokenKind::Defun],
    );
    let returnty = Box::new(parse_expr(tkiter, dlogger));
    Augmented {
        metadata,
        range: union_of(fn_tk.range, returnty.range),
        val: Expr::FnTy { paramtys, returnty },
    }
}
fn parse_exact_expr_generic<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<Expr> {
    let metadata = get_metadata(tkiter);
    let generic_tk = exact_token(tkiter, TokenKind::Generic);
    let (params, _) = parse_tyargs_expr(tkiter, dlogger, parse_patexpr);

    let returnkind = if tkiter.peek_nth(0).unwrap().kind == Some(TokenKind::ConstrainKind) {
        tkiter.next();
        Some(Box::new(parse_kind_expr(tkiter, dlogger)))
    } else {
        None
    };

    expect_token(
        tkiter,
        dlogger,
        "generic definition",
        vec![TokenKind::Defun],
    );

    let body = Box::new(parse_expr(tkiter, dlogger));

    Augmented {
        metadata,
        range: union_of(generic_tk.range, body.range),
        val: Expr::Generic {
            params,
            returnkind,
            body,
        },
    }
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
        Some(TokenKind::TypeKind) => {
            let metadata = get_metadata(tkiter);
            let Token { range, .. } = exact_token(tkiter, TokenKind::TypeKind);
            Augmented {
                range,
                metadata,
                val: KindExpr::Int,
            }
        }
        Some(TokenKind::BracketLeft) => {
            let metadata = get_metadata(tkiter);
            let (range, args, _) = parse_delimited_statement_seq(
                tkiter,
                dlogger,
                "generic argument kinds",
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
                val: KindExpr::Generic { args, returnkind },
            }
        }
        k => {
            let metadata = get_metadata(tkiter);
            dlogger.log_unexpected_token_specific(
                range,
                "kind expression",
                vec![
                    TokenKind::IntKind,
                    TokenKind::FloatKind,
                    TokenKind::BoolKind,
                    TokenKind::TypeKind,
                    TokenKind::BracketLeft,
                ],
                &k,
            );
            Augmented {
                range,
                metadata,
                val: KindExpr::Error,
            }
        }
    }
}

// parses a let or panics
fn parse_exact_filestatement_valdef<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<FileStatement> {
    let (range, metadata, pat, value) = parse_exact_valdef(tkiter, dlogger);
    Augmented {
        metadata,
        range,
        val: FileStatement::Let { pat, value },
    }
}

pub fn parse_exact_filestatement_namespace<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<FileStatement> {
    let metadata = get_metadata(tkiter);
    let prefix_tk = tkiter.next().unwrap();
    assert!(prefix_tk.kind == Some(TokenKind::Namespace));

    let identifier = expect_identifier(tkiter, dlogger, "namespace block");

    let (range, items, _) = parse_delimited_statement_seq_opt_sep(
        tkiter,
        dlogger,
        "namespace block",
        parse_filestatement,
        TokenKind::BraceLeft,
        TokenKind::BraceRight,
        TokenKind::Semicolon,
    );

    Augmented {
        metadata,
        range: union_of(prefix_tk.range, range),
        val: FileStatement::Namespace {
            namespace: identifier,
            items,
        },
    }
}

fn parse_exact_filestatement_use<TkIter: Iterator<Item = Token>>(
    tkiter: &mut PeekMoreIterator<TkIter>,
    dlogger: &mut DiagnosticLogger,
) -> Augmented<FileStatement> {
    let (range, metadata, namespace) = parse_exact_use(tkiter, dlogger);
    Augmented {
        range,
        metadata,
        val: FileStatement::Use { namespace },
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
        Some(TokenKind::Let) => parse_exact_filestatement_valdef(tkiter, dlogger),
        Some(TokenKind::Namespace) => parse_exact_filestatement_namespace(tkiter, dlogger),
        Some(TokenKind::Use) => parse_exact_filestatement_use(tkiter, dlogger),
        k => {
            // grab metadata
            let metadata = get_metadata(tkiter);
            dlogger.log_unexpected_token_specific(range, "file statement", vec![], &k);
            // drop offending token
            tkiter.next();
            Augmented {
                range,
                val: FileStatement::Error,
                metadata,
            }
        }
    }
}

struct StatementIterator<Source: Iterator<Item = Token>, Statement> {
    source: PeekMoreIterator<Source>,
    dlogger: DiagnosticLogger,
    parse_fn: fn(&mut PeekMoreIterator<Source>, &mut DiagnosticLogger) -> Augmented<Statement>,
}

impl<Source: Iterator<Item = Token>, Statement> StatementIterator<Source, Statement> {
    fn new(
        source: PeekMoreIterator<Source>,
        dlogger: DiagnosticLogger,
        parse_fn: fn(&mut PeekMoreIterator<Source>, &mut DiagnosticLogger) -> Augmented<Statement>,
    ) -> Self {
        Self {
            source,
            dlogger,
            parse_fn,
        }
    }
}

impl<Source: Iterator<Item = Token>, Statement> Iterator for StatementIterator<Source, Statement> {
    type Item = Augmented<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        // if next token is none, break
        if self.source.peek_nth(0).unwrap().kind == None {
            return None;
        }

        // parse a statement
        let statement = (self.parse_fn)(&mut self.source, &mut self.dlogger);

        // parse sep (if it exists)
        if self.source.peek_nth(0).unwrap().kind.as_ref() == Some(&TokenKind::Semicolon) {
            self.source.next();
        }

        Some(statement)
    }
}

pub fn construct_ast<Source: IntoIterator<Item = Token>>(
    source: Source,
    dlogger: DiagnosticLogger,
) -> impl Iterator<Item = Augmented<FileStatement>> {
    StatementIterator::new(source.into_iter().peekmore(), dlogger, parse_filestatement)
}
