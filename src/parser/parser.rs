use crate::lexer::token::{Token, Tokens};
use crate::parser::ast::{AstExp, AstOp, AstStatement, AstVal};
use crate::parser::util::{condition1_is_ok, condition2_is_ok, trim_block_parentheses};

pub fn parse_expression_entry(mut tokens: Tokens) -> AstVal {
    let token = tokens.pop_fin();
    let (args, mut tokens) = parse_function_args(tokens);
    let lhs = match args {
        Some(args) => match token {
            Some(token) => AstVal::new_from_tokens_call(args, token),
            None => panic!("Parse Error: Expect at least one token."),
        },
        None => match token {
            Some(token) => AstVal::new_from_token_fin(token),
            None => panic!("Parse Error: Expect at least one token."),
        },
    };
    parse_expression(lhs, 1, &mut tokens)
}

/// [Reference: Oprator-precedence parser](https://en.wikipedia.org/wiki/Operator-precedence_parser)
fn parse_expression(mut lhs: AstVal, min_precedence: u32, tokens: &mut Tokens) -> AstVal {
    let mut tokens = tokens;
    while condition1_is_ok(&mut tokens, min_precedence) {
        let op: Token = match tokens.pop_op() {
            Some(op) => Token::Op(op),
            None => panic!("Parse Error: Expect a number token"),
        };
        let precedence = tokens.get_precedence(op.get_op());

        let rhs: Token = match tokens.pop_fin() {
            Some(token) => token,
            None => panic!("Parse Error: Expect a number token"),
        };
        let mut rhs = AstVal::new_from_token_fin(rhs);
        while condition2_is_ok(&tokens, precedence) {
            rhs = parse_expression(rhs, precedence, &mut tokens);
        }
        lhs = AstVal::Exp(AstExp {
            lhs: Box::new(lhs),
            op: AstOp { op: op },
            rhs: Box::new(rhs),
        });
    }
    lhs
}

fn parse_function_args(tokens: Tokens) -> (Option<Tokens>, Tokens) {
    let mut itokens = tokens.clone();
    let mut ret_tokens: Vec<Token> = Vec::new();
    match itokens.pop() {
        Some(token) => match token {
            Token::ParenS => ret_tokens.push(token),
            _ => (),
        },
        None => (),
    }
    match itokens.pop() {
        Some(token) => match token {
            Token::ParenE => ret_tokens.push(token),
            _ => (),
        },
        None => (),
    }
    match ret_tokens.len() {
        0 => (None, tokens),
        _ => (Some(Tokens { tokens: ret_tokens }), itokens),
    }
}

pub fn parse_function_body(tokens: Tokens) -> Vec<AstStatement> {
    let mut tokens = trim_block_parentheses(tokens);
    tokens.reverse();
    let mut statements: Vec<AstStatement> = Vec::new();
    while let Some(_) = tokens.peak() {
        let statement = AstStatement::new(&mut tokens);
        statements.push(statement);
    }
    statements
}
