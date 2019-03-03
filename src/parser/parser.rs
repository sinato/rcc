use crate::lexer::token::{Token, Tokens};
use crate::parser::ast::{Ast, AstNode, AstOp, AstIde, AstBinding, AstBinaryExp, AstNum, Statements};
use log::debug;

fn condition1_is_ok(tokens: &Tokens, min_precedence: u32) -> bool {
    let precedence: Option<u32> = match tokens.clone().pop_op() {
        Some(operator) => Some(tokens.get_precedence(operator)),
        None => None,
    };
    match precedence {
        Some(precedence) => precedence >= min_precedence,
        None => false,
    }
}
fn condition2_is_ok(tokens: &Tokens, given_precedence: u32) -> bool {
    let precedence: Option<u32> = match tokens.clone().pop_op() {
        Some(operator) => Some(tokens.get_precedence(operator)),
        None => None,
    };
    match precedence {
        Some(precedence) => precedence > given_precedence,
        None => false,
    }
}

/// [Reference: Oprator-precedence parser](https://en.wikipedia.org/wiki/Operator-precedence_parser)
fn parse_expression(mut lhs: AstNode, min_precedence: u32, tokens: Tokens) -> (AstNode, Tokens) {
    let mut tokens = tokens.clone();
    while condition1_is_ok(&tokens, min_precedence) {
        let op: Token = match tokens.pop_op() {
            Some(op) => Token::Op(op),
            None => panic!("Parse Error: Expect a number token"),
        };
        let precedence = tokens.get_precedence(op.get_op());
        let rhs: Token = match tokens.pop_num() {
            Some(num) => Token::Num(num),
            None => panic!("Parse Error: Expect a number token"),
        };
        let mut rhs = AstNode::Num(AstNum { num: rhs });
        while condition2_is_ok(&tokens, precedence) {
            let (ret_rhs, ret_tokens) = parse_expression(rhs, precedence, tokens);
            rhs = ret_rhs;
            tokens = ret_tokens;
        }
        lhs = AstNode::Exp(AstBinaryExp {
            lhs: Box::new(lhs),
            op: AstOp { op: op },
            rhs: Box::new(rhs),
        });
    }
    (lhs, tokens)
}

fn parse_expression_entry(mut tokens: Tokens) -> AstNode {
    let token = tokens.pop();
    let mut lhs = match token {
        Some(token) => match token {
            Token::Num(_) => AstNode::Num(AstNum { num: token }),
            _ => panic!("Parse Error: Expect a number token."),
        },
        None => panic!("Parse Error: Expect at least one token."),
    };
    let (returned_lhs, _returned_tokens) = parse_expression(lhs, 0, tokens);
    lhs = returned_lhs;
    lhs
}

fn parse_binding(mut tokens: Tokens) -> AstNode {

    let token = tokens.pop();
    let ide = match token {
        Some(token) => match token {
            Token::Ide(_) => AstIde { ide: token },
            _ => panic!("Parse Error: Expect an identifier token."),
        },
        None => panic!("Parse Error: Expect at least one token."),
    };

    let token = tokens.pop();
    match token {
        Some(token) => match token {
            Token::Op(op) => match op.as_ref() {
                "=" => (),
                _ => panic!("Parse Error: Expect = operator."),
            }
            _ => panic!("Parse Error: Expect an identifier token."),
        },
        None => panic!("Parse Error: Expect at least one token."),
    };

    let val = Box::new(parse_expression_entry(tokens));

    AstNode::Bind(AstBinding {
            ide, val
        })
}

fn parse_statement(mut tokens: Tokens) -> Ast {
    tokens.reverse();
    let token = tokens.pop().expect("Parse Error: Expect at least one token.");
    let lhs = match token {
        Token::Ide(_) => {
            tokens.push(token);
            parse_binding(tokens)
        },
        Token::Num(_) => {
            tokens.push(token);
            parse_expression_entry(tokens)
        },
        _ => panic!("Parse Error: Unexpected token."),
    };
    debug!("AST:\n {}", lhs);
    Ast { ast: lhs }
}

fn get_statements(tokens: Tokens) -> Vec<Tokens> {
    let tokens: Vec<Token> = tokens.get_tokens();
    let mut parsed_tokens: Vec<Tokens> = Vec::new();
    let mut tmp_tokens: Vec<Token> = Vec::new();
    for token in tokens {
        match token {
            Token::Semi => {
                let t = tmp_tokens.clone();
                parsed_tokens.push(Tokens { tokens: t });
                tmp_tokens.clear();
            },
            _ => tmp_tokens.push(token),
        }
    }
    if tmp_tokens.len() != 0 {
        panic!("Parse Error: Expected the last semicolon, but not found.");
    }
    parsed_tokens
}

pub fn parser(tokens: Tokens) -> Statements {
    let statements = get_statements(tokens);
    debug!("STATEMENTS: {:?}", statements);
    let mut asts: Vec<Ast> = Vec::new();
    for statement in statements {
        asts.push(parse_statement(statement));
    }
    Statements::new(asts)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_statements() {
        let tokens1 = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(10)];
        let tokens2 = vec![Token::Num(77)];
        let expect = vec![Tokens { tokens: tokens1 }, Tokens { tokens: tokens2 }];

        let tokens = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(10), Token::Semi, Token::Num(77), Token::Semi];
        let tokens  = Tokens { tokens };
        assert_eq!(get_statements(tokens), expect);
    }

    #[test]
    #[should_panic]
    fn test_parse_statement_illegal() {
        let tokens = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(10)];
        let tokens  = Tokens { tokens };
        get_statements(tokens);
    }

    #[test]
    fn test_parser_num() {
        let expect = Ast {
            ast: AstNode::Num(AstNum {
                num: Token::Num(2434),
            }),
        };
        let tokens = vec![Token::Num(2434)];
        let tokens = Tokens { tokens };
        let actual = parse_statement(tokens);
        assert_eq!(actual, expect);
    }

    #[test]
    #[should_panic]
    fn test_parser_num_illegal() {
        let tokens = vec![Token::Op(String::from("+"))];
        let tokens = Tokens { tokens };
        parse_statement(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_illegal_tokens() {
        let tokens = vec![Token::Num(2434), Token::Op(String::from("+"))];
        let tokens = Tokens { tokens };
        parse_statement(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_illegal_tokens() {
        let tokens = vec![Token::Num(2434), Token::Num(2434), Token::Num(2434)];
        let tokens = Tokens { tokens };
        parse_statement(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_illegal_tokens2() {
        let tokens = vec![
            Token::Num(2434),
            Token::Op(String::from("+")),
            Token::Op(String::from("+")),
        ];
        let tokens = Tokens { tokens };
        parse_statement(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_noimplemented_operator() {
        let tokens = vec![
            Token::Num(2434),
            Token::Op(String::from("-")),
            Token::Num(2434),
        ];
        let tokens = Tokens { tokens };
        parse_statement(tokens);
    }

    #[test]
    fn test_parser_exp() {
        let lhs = Box::new(AstNode::Num(AstNum {
            num: Token::Num(10),
        }));
        let op = AstOp {
            op: Token::Op(String::from("+")),
        };
        let rhs = Box::new(AstNode::Num(AstNum {
            num: Token::Num(20),
        }));
        let expect = Ast {
            ast: AstNode::Exp(AstBinaryExp { lhs, op, rhs }),
        };
        let tokens = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20)];
        let tokens = Tokens { tokens };
        let actual = parse_statement(tokens);
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_parser_exp_three_terms() {
        // make expected rhs
        let lhs = Box::new(AstNode::Num(AstNum {
            num: Token::Num(20),
        }));
        let op = AstOp {
            op: Token::Op(String::from("*")),
        };
        let rhs = Box::new(AstNode::Num(AstNum {
            num: Token::Num(30),
        }));
        let rhs = Box::new(AstNode::Exp(AstBinaryExp { lhs, op, rhs }));

        // make expected lhs
        let lhs = Box::new(AstNode::Num(AstNum {
            num: Token::Num(10),
        }));
        // make expected operator
        let op = AstOp {
            op: Token::Op(String::from("+")),
        };
        // make expected ast
        let expect = Ast {
            ast: AstNode::Exp(AstBinaryExp { lhs, op, rhs }),
        };

        let tokens = vec![
            Token::Num(10),
            Token::Op(String::from("+")),
            Token::Num(20),
            Token::Op(String::from("*")),
            Token::Num(30),
        ];
        let tokens = Tokens { tokens };
        let actual = parse_statement(tokens);
        assert_eq!(actual, expect);
    }

}
