use crate::lexer::token::{Token, Tokens};

pub fn condition1_is_ok(tokens: &Tokens, min_precedence: u32) -> bool {
    let precedence: Option<u32> = match tokens.clone().pop_op() {
        Some(operator) => Some(tokens.get_precedence(operator)),
        None => None,
    };
    match precedence {
        Some(precedence) => precedence >= min_precedence,
        None => false,
    }
}
pub fn condition2_is_ok(tokens: &Tokens, given_precedence: u32) -> bool {
    let precedence: Option<u32> = match tokens.clone().pop_op() {
        Some(operator) => Some(tokens.get_precedence(operator)),
        None => None,
    };
    match precedence {
        Some(precedence) => precedence > given_precedence,
        None => false,
    }
}

pub fn trim_parentheses(mut tokens: Tokens) -> Tokens {
    tokens.reverse();
    match tokens.pop() {
        Some(token) => match token {
            Token::ParenS => (),
            _ => panic!("Expect ParenS"),
        },
        None => panic!("Expect ParenS"),
    }
    tokens.reverse();
    match tokens.pop() {
        Some(token) => match token {
            Token::ParenE => (),
            _ => panic!("Expect ParenE"),
        },
        None => panic!("Expect ParenE"),
    }
    tokens
}

pub fn trim_block_parentheses(mut tokens: Tokens) -> Tokens {
    tokens.reverse();
    match tokens.pop() {
        Some(token) => match token {
            Token::BlockS => (),
            _ => panic!("Expect BlockS"),
        },
        None => panic!("Expect BlockS"),
    }
    tokens.reverse();
    match tokens.pop() {
        Some(token) => match token {
            Token::BlockE => (),
            _ => panic!("Expect BlockE"),
        },
        None => panic!("Expect BlockE"),
    }
    tokens
}
