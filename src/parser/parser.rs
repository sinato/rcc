use crate::lexer::token::{Token, Tokens};
use crate::parser::ast::{AstInstruction, AstIfStatement, AstCompoundStatement, AstStatement, AstReturn, AstVal, AstOp, AstIde, AstBinding, AstExp, AstFunction};
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

fn parse_compound_statement(tokens: Tokens) -> (AstStatement, Tokens) {
    let mut tokens: Vec<Token> = tokens.get_tokens();
    let mut compound_tokens: Vec<Token> = Vec::new();
    loop {
        let token = tokens.pop();
        match token {
            Some(token) => match token {
                Token::BlockE => break,
                _ => compound_tokens.push(token),
            },
            None => panic!("Expect Token::BlockE"),
        }
    }
    let mut instructions: Vec<AstInstruction> = Vec::new();
    let mut tmp_tokens: Vec<Token> = Vec::new();
    for token in compound_tokens {
        match token {
            Token::Semi => {
                let t = Tokens { tokens: tmp_tokens.clone() };
                let instruction = parse_instruction(t);
                instructions.push(instruction);
                tmp_tokens.clear();
            },
            _ => tmp_tokens.push(token),
        }
    }
    if tmp_tokens.len() != 0 {
        panic!("Parse Error: Expected the last semicolon, but not found.");
    }
    (AstStatement::CompoundStatement(AstCompoundStatement::Instructions(instructions)), Tokens{tokens})
}

fn parse_if_statement(mut tmp_tokens: Tokens, tokens: Tokens) -> (AstStatement, Tokens) {
    tmp_tokens.reverse();
    let first_token = tmp_tokens.pop();
    match first_token {
        Some(token) => match token {
            Token::If => (),
            _ => panic!("Expected if token"),
        }
        None => panic!("Expected if token"),
    }
    tmp_tokens.reverse();
    let condition_val = parse_condition(tmp_tokens);

    let (block, ret_tokens) = parse_compound_statement(tokens);
    let block = Box::new(block);
    let ast_if = AstStatement::IfStatement(AstIfStatement{ condition_val, block });
    (ast_if, ret_tokens)
}

fn parse_condition(mut tokens: Tokens) -> AstVal {
    // assertion
    match tokens.first() {
        Some(token) => match token {
            Token::ParenS => (),
            _ => panic!("Expected ParenS token"),
        }
        None => panic!("Expected ParenS token"),
    }
    match tokens.last() {
        Some(token) => match token {
            Token::ParenE => (),
            _ => panic!("Expected ParenE token"),
        }
        None => panic!("Expected ParenE token"),
    }
    // trim
    tokens.reverse();
    tokens = tokens.split_off(1);
    tokens.reverse();
    tokens = tokens.split_off(1);

    // parse
    parse_expression_entry(tokens)
}

fn parse_statement(tokens: Tokens) -> AstStatement {
    AstStatement::Instruction(parse_instruction(tokens))
}

fn parse_function(tokens: Tokens) -> Vec<AstStatement> {
    let mut tokens: Vec<Token> = tokens.get_tokens();
    tokens.reverse();
    let mut statements: Vec<AstStatement> = Vec::new();
    let mut tmp_tokens: Vec<Token> = Vec::new();
    loop {
        let token = tokens.pop();
        match token {
            Some(token) => match token{
                Token::Semi => {
                    let t = Tokens { tokens: tmp_tokens.clone() };
                    let statement = parse_statement(t);
                    statements.push(statement);
                    tmp_tokens.clear();
                },
                Token::BlockS => {
                    let (ret_statement, ret_tokens) = match tmp_tokens.len() {
                        0 => parse_compound_statement(Tokens{ tokens: tokens.clone() }),
                        _ => parse_if_statement(Tokens{ tokens: tmp_tokens.clone() }, Tokens{ tokens: tokens.clone() }),
                    };
                    tmp_tokens.clear();
                    tokens = ret_tokens.tokens;
                    statements.push(ret_statement);
                }
                _ => tmp_tokens.push(token),
            },
            None => break,
        }
    }
    if tmp_tokens.len() != 0 {
        panic!("Parse Error: Expected the last semicolon, but not found.");
    }
    statements
}

pub fn parser(tokens: Tokens) -> AstFunction{
    let statements = parse_function(tokens);
    debug!("INSTRUCTIONS: {:?}", statements);
    AstFunction::new(statements)
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{AstFin, AstNum};

    fn get_astnode_num(num: u64) -> AstVal {
        AstVal::Fin(AstFin::Num(AstNum { num: Token::Num(num) }))
    }

    #[test]
    fn test_parse_compound_statement() {
        let ast_return = AstReturn { val: AstVal::Fin(AstFin::Num(AstNum{ num: Token::Num(33) })) };
        let expect_instructions = vec![AstInstruction::Return(ast_return)];
        let expect_ast = AstCompoundStatement::Instructions(expect_instructions);
        let expect_ast = AstStatement::CompoundStatement(expect_ast);
        let expect_tokens = Tokens { tokens: vec![Token::Num(22)] };
        let expect = (expect_ast, expect_tokens);

        let mut tokens = vec![Token::Ret, Token::Num(33), Token::Semi, Token::BlockE, Token::Num(22)];
        tokens.reverse();
        let tokens = Tokens { tokens };
        assert_eq!(parse_compound_statement(tokens), expect);
    }

    #[test]
    #[should_panic]
    fn test_parse_instruction_illegal() {
        let tokens = vec![Token::Ret, Token::Num(10), Token::Op(String::from("+")), Token::Num(10)];
        let tokens  = Tokens { tokens };
        parse_function(tokens);
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
