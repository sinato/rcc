use crate::lexer::token::{Token, Tokens};
use crate::parser::ast::{
    AstBinding, AstCompoundStatement, AstConditionalStatement, AstExp, AstFunction, AstIde,
    AstIfStatement, AstInstruction, AstOp, AstProgram, AstReturn, AstStatement, AstVal,
    AstWhileStatement,
};
use crate::parser::util::{
    condition1_is_ok, condition2_is_ok, trim_block_parentheses, trim_parentheses,
};
use log::debug;

fn parse_expression_entry(mut tokens: Tokens) -> AstVal {
    let token = tokens.pop_fin();
    let (args, tokens) = parse_function_args(tokens);
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
    let (lhs, _returned_tokens) = parse_expression(lhs, 1, tokens);
    lhs
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

fn parse_return(mut tokens: Tokens) -> AstInstruction {
    match tokens.pop() {
        Some(token) => match token {
            Token::Ret => (),
            _ => panic!("Parse Error: Expect Token::Ret"),
        },
        None => panic!("Parse Error: Expect Token::Ret"),
    }
    let ast = AstReturn {
        val: parse_expression_entry(tokens),
    };
    AstInstruction::Return(ast)
}

fn parse_binding(mut tokens: Tokens) -> AstInstruction {
    let ide = match tokens.pop_ide() {
        Ok(identifier) => match identifier {
            Some(identifier) => AstIde {
                ide: Token::Ide(identifier),
            },
            None => panic!("Parse Error: Expect at least one token."),
        },
        Err(msg) => panic!(msg),
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
    let token = tokens
        .peak()
        .expect("Parse Error: Expect at least one token.");
    match token {
        Token::Ide(_) => parse_binding(tokens),
        Token::Ret => parse_return(tokens),
        _ => panic!("Parse Error: Unexpected token."),
    }
}

fn parse_compound_statement(mut tokens: Tokens) -> AstStatement {
    tokens.reverse();
    let compound_tokens: Tokens = match tokens.pop_block() {
        Ok(tokens) => match tokens {
            Some(tokens) => tokens,
            None => panic!(""),
        },
        Err(msg) => panic!(msg),
    };
    let mut compound_tokens = trim_block_parentheses(compound_tokens);
    compound_tokens.reverse();
    let mut instructions: Vec<AstInstruction> = Vec::new();
    while let Ok(instruction_tokens) = compound_tokens.pop_instruction() {
        let mut instruction_tokens = instruction_tokens;
        instruction_tokens.pop();
        instructions.push(parse_instruction(instruction_tokens));
    }
    AstStatement::CompoundStatement(AstCompoundStatement::Instructions(instructions))
}

fn parse_if_statement(condition_tokens: Tokens, body_tokens: Tokens) -> AstStatement {
    let condition_statement = parse_condition(condition_tokens);
    let block = Box::new(parse_compound_statement(body_tokens));
    AstStatement::IfStatement(AstIfStatement {
        condition_statement,
        block,
    })
}

fn parse_while_statement(condition_tokens: Tokens, body_tokens: Tokens) -> AstStatement {
    let condition_statement = parse_condition(condition_tokens);
    let block = Box::new(parse_compound_statement(body_tokens));
    AstStatement::WhileStatement(AstWhileStatement {
        condition_statement,
        block,
    })
}

fn parse_condition(mut tokens: Tokens) -> AstConditionalStatement {
    tokens = trim_parentheses(tokens);
    tokens.reverse();
    let identifier = match tokens.pop_ide() {
        Ok(identifier) => match identifier {
            Some(identifier) => identifier,
            None => panic!("Expect an identifier"),
        },
        Err(msg) => panic!(msg),
    };
    let condition_identifier = AstIde {
        ide: Token::Ide(identifier),
    };
    let token = tokens.pop_condop();
    let condition_operator = match token {
        Some(op) => match op.as_ref() {
            "==" => "==".to_string(),
            "!=" => "!=".to_string(),
            _ => panic!("Parse Error: Expect an equal operator."),
        },
        None => panic!("Parse Error: Expect at least one token."),
    };
    let condition_val = parse_expression_entry(tokens);
    AstConditionalStatement {
        condition_identifier,
        condition_operator,
        condition_val,
    }
}

fn parse_instruction_statement(mut tokens: Tokens) -> AstStatement {
    tokens.pop();
    AstStatement::Instruction(parse_instruction(tokens))
}

fn parse_function_body(tokens: Tokens) -> Vec<AstStatement> {
    let mut tokens = trim_block_parentheses(tokens);
    tokens.reverse();
    let mut statements: Vec<AstStatement> = Vec::new();
    while let Some(statement) = parse_statement(&mut tokens) {
        statements.push(statement);
    }
    statements
}

fn parse_statement(tokens: &mut Tokens) -> Option<AstStatement> {
    match tokens.peak() {
        Some(token) => match token {
            Token::If => {
                let (condition_tokens, body_tokens) = match tokens.pop_if_statement() {
                    Ok(ret) => ret,
                    Err(msg) => panic!(msg),
                };
                Some(parse_if_statement(condition_tokens, body_tokens))
            }
            Token::While => {
                let (condition_tokens, body_tokens) = match tokens.pop_while_statement() {
                    Ok(ret) => ret,
                    Err(msg) => panic!(msg),
                };
                Some(parse_while_statement(condition_tokens, body_tokens))
            }
            Token::BlockS => {
                let body_tokens = match tokens.pop_block() {
                    Ok(ret) => match ret {
                        Some(ret) => ret,
                        None => panic!("Expect a block"),
                    },
                    Err(msg) => panic!(msg),
                };
                Some(parse_compound_statement(body_tokens))
            }
            _ => {
                let instruction_tokens = match tokens.pop_instruction() {
                    Ok(ret) => ret,
                    Err(msg) => panic!(msg),
                };
                Some(parse_instruction_statement(instruction_tokens))
            }
        },
        None => None,
    }
}

fn parse_function(mut tokens: Tokens) -> AstProgram {
    let mut ast_functions: Vec<AstFunction> = Vec::new();
    loop {
        let identifier = match tokens.pop_ide() {
            Ok(identifier) => match identifier {
                Some(identifier) => identifier,
                None => break,
            },
            Err(msg) => panic!(msg),
        };

        let _parameters = match tokens.pop_parameters() {
            Ok(parameters) => match parameters {
                Some(parameters) => parameters,
                None => Tokens::new(),
            },
            Err(msg) => panic!(format!("{}", msg)),
        };

        let body_tokens = match tokens.pop_block() {
            Ok(block_tokens) => match block_tokens {
                Some(block_tokens) => block_tokens,
                None => panic!("Expect at least one statement in a block"),
            },
            Err(msg) => panic!(format!("{}", msg)),
        };
        let statements = parse_function_body(body_tokens);
        debug!("INSTRUCTIONS: {:?}", statements);
        ast_functions.push(AstFunction::new(identifier, statements));
    }
    AstProgram {
        functions: ast_functions,
    }
}

pub fn parser(mut tokens: Tokens) -> AstProgram {
    tokens.reverse();
    parse_function(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{AstFin, AstNum};

    fn get_astnode_num(num: u64) -> AstVal {
        AstVal::Fin(AstFin::Num(AstNum {
            num: Token::Num(num),
        }))
    }

    #[test]
    fn test_parse_compound_statement() {
        let ast_return = AstReturn {
            val: AstVal::Fin(AstFin::Num(AstNum {
                num: Token::Num(33),
            })),
        };
        let expect_instructions = vec![AstInstruction::Return(ast_return)];
        let expect_ast = AstCompoundStatement::Instructions(expect_instructions);
        let expect_ast = AstStatement::CompoundStatement(expect_ast);
        let expect = expect_ast;

        let tokens = vec![
            Token::BlockS,
            Token::Ret,
            Token::Num(33),
            Token::Semi,
            Token::BlockE,
            Token::Num(22),
        ];
        let tokens = Tokens { tokens };
        assert_eq!(parse_compound_statement(tokens), expect);
    }

    #[test]
    fn test_parser_num() {
        let expect = AstInstruction::Return(AstReturn {
            val: get_astnode_num(2434),
        });
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
        let tokens = vec![
            Token::Ret,
            Token::Num(2434),
            Token::Num(2434),
            Token::Num(2434),
        ];
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
        let expect = AstInstruction::Return(AstReturn {
            val: AstVal::Exp(AstExp { lhs, op, rhs }),
        });
        let tokens = vec![
            Token::Ret,
            Token::Num(10),
            Token::Op(String::from("+")),
            Token::Num(20),
        ];
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
        let expect = AstInstruction::Return(AstReturn {
            val: AstVal::Exp(AstExp { lhs, op, rhs }),
        });

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
