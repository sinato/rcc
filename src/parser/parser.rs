use crate::lexer::token::{Token, Tokens};
use crate::parser::ast::{AstInstruction, AstReturn, AstVal, AstOp, AstIde, AstBinding, AstExp, AstNum, AstFunction, AstFin};
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
fn parse_expression(mut lhs: AstVal, min_precedence: u32, tokens: Tokens) -> (AstVal, Tokens) {
    let mut tokens = tokens.clone();
    while condition1_is_ok(&tokens, min_precedence) {
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
            let (ret_rhs, ret_tokens) = parse_expression(rhs, precedence, tokens);
            rhs = ret_rhs;
            tokens = ret_tokens;
        }
        lhs = AstVal::Exp(AstExp {
            lhs: Box::new(lhs),
            op: AstOp { op: op },
            rhs: Box::new(rhs),
        });
    }
    (lhs, tokens)
}

fn parse_expression_entry(mut tokens: Tokens) -> AstVal {
    let token = tokens.pop_fin();
    let lhs = match token {
        Some(token) => AstVal::new_from_token_fin(token),
        None => panic!("Parse Error: Expect at least one token."),
    };
    let (lhs, _returned_tokens) = parse_expression(lhs, 0, tokens);
    lhs
}

fn parse_return(mut tokens: Tokens) -> AstInstruction {
    // TODO: implement validation?
    tokens.pop();
    let ast = AstReturn{  val: parse_expression_entry(tokens)};
    AstInstruction::Return(ast)
}

fn parse_binding(mut tokens: Tokens) -> AstInstruction {
    let token = tokens.pop_ide();
    let ide = match token {
        Some(identifier) => AstIde { ide: Token::Ide(identifier) } ,
        None => panic!("Parse Error: Expect at least one token."),
    };
    let token = tokens.pop_op();
    match token {
        Some(op) => match op.as_ref() {
            "=" => (), 
            _ => panic!("Parse Error: Expect an equal operator."),
        },
        None => panic!("Parse Error: Expect at least one token."),
    };
    let val = parse_expression_entry(tokens);
    AstInstruction::Bind(AstBinding { ide, val })
}

fn parse_instruction(mut tokens: Tokens) -> AstInstruction {
    tokens.reverse();
    let token = tokens.peak().expect("Parse Error: Expect at least one token.");
    match token {
        Token::Ide(_) => parse_binding(tokens),
        Token::Ret => parse_return(tokens),
        _ => panic!("Parse Error: Unexpected token."),
    }
}

fn get_instructions(tokens: Tokens) -> Vec<Tokens> {
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

pub fn parser(tokens: Tokens) -> AstFunction{
    let instructions = get_instructions(tokens);
    debug!("INSTRUCTIONS: {:?}", instructions);
    let mut asts: Vec<AstInstruction> = Vec::new();
    for instruction in instructions {
        asts.push(parse_instruction(instruction));
    }
    AstFunction::new(asts)
}


#[cfg(test)]
mod tests {
    use super::*;

    fn get_astnode_num(num: u64) -> AstVal {
        AstVal::Fin(AstFin::Num(AstNum { num: Token::Num(num) }))
    }

    #[test]
    fn test_get_statements() {
        let tokens1 = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(10)];
        let tokens2 = vec![Token::Num(77)];
        let expect = vec![Tokens { tokens: tokens1 }, Tokens { tokens: tokens2 }];

        let tokens = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(10), Token::Semi, Token::Num(77), Token::Semi];
        let tokens  = Tokens { tokens };
        assert_eq!(get_instructions(tokens), expect);
    }

    #[test]
    #[should_panic]
    fn test_parse_instruction_illegal() {
        let tokens = vec![Token::Ret, Token::Num(10), Token::Op(String::from("+")), Token::Num(10)];
        let tokens  = Tokens { tokens };
        get_instructions(tokens);
    }

    #[test]
    fn test_parser_num() {
        let expect = AstInstruction::Return(AstReturn{ val: get_astnode_num(2434) });
        let tokens = vec![Token::Ret, Token::Num(2434)];
        let tokens = Tokens { tokens };
        let actual = parse_instruction(tokens);
        assert_eq!(actual, expect);
    }

    #[test]
    #[should_panic]
    fn test_parser_num_illegal() {
        let tokens = vec![Token::Ret, Token::Op(String::from("+"))];
        let tokens = Tokens { tokens };
        parse_instruction(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_illegal_tokens() {
        let tokens = vec![Token::Ret, Token::Num(2434), Token::Op(String::from("+"))];
        let tokens = Tokens { tokens };
        parse_instruction(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_illegal_tokens() {
        let tokens = vec![Token::Ret, Token::Num(2434), Token::Num(2434), Token::Num(2434)];
        let tokens = Tokens { tokens };
        parse_instruction(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_illegal_tokens2() {
        let tokens = vec![
            Token::Ret,
            Token::Num(2434),
            Token::Op(String::from("+")),
            Token::Op(String::from("+")),
        ];
        let tokens = Tokens { tokens };
        parse_instruction(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_noimplemented_operator() {
        let tokens = vec![
            Token::Ret,
            Token::Num(2434),
            Token::Op(String::from("-")),
            Token::Num(2434),
        ];
        let tokens = Tokens { tokens };
        parse_instruction(tokens);
    }

    #[test]
    fn test_parser_exp() {
        let lhs = Box::new(get_astnode_num(10));
        let op = AstOp {
            op: Token::Op(String::from("+")),
        };
        let rhs = Box::new(get_astnode_num(20));
        let expect = AstInstruction::Return(AstReturn{ val:  AstVal::Exp(AstExp{ lhs, op, rhs })});
        let tokens = vec![Token::Ret, Token::Num(10), Token::Op(String::from("+")), Token::Num(20)];
        let tokens = Tokens { tokens };
        let actual = parse_instruction(tokens);

        assert_eq!(actual, expect);
    }

    #[test]
    fn test_parser_exp_three_terms() {
        // make expected rhs
        let lhs = Box::new(get_astnode_num(20));
        let op = AstOp {
            op: Token::Op(String::from("*")),
        };
        let rhs = Box::new(get_astnode_num(30));
        let rhs = Box::new(AstVal::Exp(AstExp { lhs, op, rhs }));

        // make expected lhs
        let lhs = Box::new(get_astnode_num(10));
        // make expected operator
        let op = AstOp {
            op: Token::Op(String::from("+")),
        };
        // make expected ast
        let expect = AstInstruction::Return(AstReturn{ val: AstVal::Exp(AstExp { lhs, op, rhs })});

        let tokens = vec![
            Token::Ret,
            Token::Num(10),
            Token::Op(String::from("+")),
            Token::Num(20),
            Token::Op(String::from("*")),
            Token::Num(30),
        ];
        let tokens = Tokens { tokens };
        let actual = parse_instruction(tokens);
        assert_eq!(actual, expect);
    }

}
