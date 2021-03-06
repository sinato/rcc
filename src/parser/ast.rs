use crate::lexer::token::{Token, Tokens};
use crate::parser::util::{
    condition1_is_ok, condition2_is_ok, trim_block_parentheses, trim_parentheses,
};

#[derive(Debug, PartialEq, Clone)]
/// program := function+
pub struct AstProgram {
    pub functions: Vec<AstFunction>,
}
impl AstProgram {
    pub fn new(tokens: &mut Tokens) -> AstProgram {
        tokens.reverse();
        let mut functions: Vec<AstFunction> = Vec::new();
        while let Some(_) = tokens.peak() {
            let function = AstFunction::new(tokens);
            functions.push(function);
        }
        AstProgram {
            functions: functions,
        }
    }
}

/// function := "int" identifier "()" "{" statement+ "}"
#[derive(Debug, PartialEq, Clone)]
pub struct AstFunction {
    pub identifier: String,
    pub parameters: Vec<String>,
    pub statements: Vec<AstStatement>,
}
impl AstFunction {
    pub fn new(tokens: &mut Tokens) -> AstFunction {
        let _function_type = match tokens.pop_type() {
            Ok(function_type) => function_type,
            Err(msg) => panic!(msg),
        };

        let identifier = match tokens.pop_identifier() {
            Ok(identifier) => identifier,
            Err(msg) => panic!(msg),
        };

        let mut parameter_tokens = match tokens.pop_paren() {
            Ok(mut tokens) => trim_parentheses(&mut tokens),
            Err(msg) => panic!(msg),
        };
        let mut parameters: Vec<String> = Vec::new();
        if let Some(parameter_tokens) = parameter_tokens.split_with_comma() {
            for mut tokens in parameter_tokens {
                let _parameter_type = match tokens.pop_type() {
                    Ok(parameter_type) => parameter_type,
                    Err(msg) => panic!(msg),
                };

                let parameter = match tokens.pop_identifier() {
                    Ok(identifier) => identifier,
                    Err(msg) => panic!(msg),
                };
                parameters.push(parameter);
            }
        }

        let body_tokens = match tokens.pop_block() {
            Ok(block_tokens) => block_tokens,
            Err(msg) => panic!(msg),
        };
        let statements = parse_function_body(body_tokens);
        AstFunction {
            identifier,
            parameters,
            statements,
        }
    }
}
pub fn parse_function_body(mut tokens: Tokens) -> Vec<AstStatement> {
    let mut tokens = trim_block_parentheses(&mut tokens);
    tokens.reverse();
    let mut statements: Vec<AstStatement> = Vec::new();
    while let Some(_) = tokens.peak() {
        let statement = AstStatement::new(&mut tokens);
        statements.push(statement);
    }
    statements
}

/// statement := [ instruction_statement | compound_statement | if_statement | while_statement ]
#[derive(Debug, PartialEq, Clone)]
pub enum AstStatement {
    InstructionStatement(AstInstructionStatement),
    CompoundStatement(AstCompoundStatement),
    IfStatement(AstIfStatement),
    WhileStatement(AstWhileStatement),
}
impl AstStatement {
    pub fn new(tokens: &mut Tokens) -> AstStatement {
        let token = match tokens.peak() {
            Some(token) => token,
            None => panic!("Expect at least one token".to_string()),
        };
        match token {
            Token::If => match tokens.pop_if_statement() {
                Ok((condition_tokens, body_tokens)) => {
                    AstStatement::IfStatement(AstIfStatement::new(condition_tokens, body_tokens))
                }
                Err(msg) => panic!(msg),
            },
            Token::While => match tokens.pop_while_statement() {
                Ok((mut condition_tokens, body_tokens)) => AstStatement::WhileStatement(
                    AstWhileStatement::new(&mut condition_tokens, body_tokens),
                ),
                Err(msg) => panic!(msg),
            },
            Token::BlockS => match tokens.pop_block() {
                Ok(body_tokens) => {
                    AstStatement::CompoundStatement(AstCompoundStatement::new(body_tokens))
                }
                Err(msg) => panic!(msg),
            },
            _ => match tokens.pop_instruction() {
                Ok(mut instruction_tokens) => AstStatement::InstructionStatement(
                    AstInstructionStatement::new(&mut instruction_tokens),
                ),
                Err(msg) => panic!(msg),
            },
        }
    }
}

/// instruction_statement := [ binding | return ]
#[derive(Debug, PartialEq, Clone)]
pub enum AstInstructionStatement {
    Bind(AstBinding),
    Return(AstReturn),
    Declare(AstDeclare),
}
impl AstInstructionStatement {
    fn new(tokens: &mut Tokens) -> AstInstructionStatement {
        match tokens.first() {
            Some(token) => match token {
                Token::Type(_) => AstInstructionStatement::Declare(AstDeclare::new(tokens)),
                Token::Ide(_) => AstInstructionStatement::Bind(AstBinding::new(tokens)),
                Token::Ret => AstInstructionStatement::Return(AstReturn::new(tokens)),
                _ => panic!("Parse Error: Unexpected token."),
            },
            None => panic!("Expect instruction tokens."),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstDeclare {
    BindDeclare(AstBindDeclare),
    SimpleDeclare(AstSimpleDeclare),
}
impl AstDeclare {
    fn new(raw_tokens: &mut Tokens) -> AstDeclare {
        let mut tokens: Tokens = raw_tokens.clone();
        match tokens.pop_semicolon() {
            Ok(_token) => (),
            Err(msg) => panic!(msg),
        }
        tokens.reverse();
        let _variable_type = match tokens.pop_type() {
            Ok(variable_type) => variable_type,
            Err(msg) => panic!(msg),
        };
        let ide = AstIde::new(&mut tokens);
        match tokens.peak() {
            Some(token) => match token {
                Token::Op(_) => {
                    let val = match tokens.pop_operator() {
                        Ok(op) => match op.as_ref() {
                            "=" => AstVal::new(&mut tokens),
                            _ => panic!("Parse Error: Expect an equal operator."),
                        },
                        Err(msg) => panic!(msg),
                    };
                    AstDeclare::BindDeclare(AstBindDeclare { ide, val })
                }
                _ => panic!("Unexpected pattern"),
            },
            None => AstDeclare::SimpleDeclare(AstSimpleDeclare { ide }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstBindDeclare {
    pub ide: AstIde,
    pub val: AstVal,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstSimpleDeclare {
    pub ide: AstIde,
}

/// binding_statement := identifier "=" val ";"
#[derive(Debug, PartialEq, Clone)]
pub struct AstBinding {
    pub ide: AstIde,
    pub val: AstVal,
}
impl AstBinding {
    pub fn new(tokens: &mut Tokens) -> AstBinding {
        match tokens.pop_semicolon() {
            Ok(_token) => (),
            Err(msg) => panic!(msg),
        }
        tokens.reverse();
        let ide = AstIde::new(tokens);
        let val = match tokens.pop_operator() {
            Ok(op) => match op.as_ref() {
                "=" => AstVal::new(tokens),
                _ => panic!("Parse Error: Expect an equal operator."),
            },
            Err(msg) => panic!(msg),
        };
        AstBinding { ide, val }
    }
}
/// return_statement := "return" val ";"
#[derive(Debug, PartialEq, Clone)]
pub struct AstReturn {
    pub val: AstVal,
}
impl AstReturn {
    pub fn new(tokens: &mut Tokens) -> AstReturn {
        match tokens.pop_semicolon() {
            Ok(_token) => (),
            Err(msg) => panic!(msg),
        }
        tokens.reverse();
        match tokens.pop_return() {
            Ok(_) => AstReturn {
                val: AstVal::new(tokens),
            },
            Err(msg) => panic!(msg),
        }
    }
}

/// compound_statement := "{" instruction_statement* "}"
#[derive(Debug, PartialEq, Clone)]
pub enum AstCompoundStatement {
    Instructions(Vec<AstInstructionStatement>),
}
impl AstCompoundStatement {
    pub fn new(mut tokens: Tokens) -> AstCompoundStatement {
        tokens.reverse();
        let mut compound_tokens: Tokens = match tokens.pop_block() {
            Ok(tokens) => tokens,
            Err(msg) => panic!(msg),
        };
        let mut compound_tokens = trim_block_parentheses(&mut compound_tokens);
        compound_tokens.reverse();
        let mut instructions: Vec<AstInstructionStatement> = Vec::new();
        while let Ok(mut instruction_tokens) = compound_tokens.pop_instruction() {
            instructions.push(AstInstructionStatement::new(&mut instruction_tokens));
        }
        AstCompoundStatement::Instructions(instructions)
    }
}

/// if_statement := "if" "(" condition_statement ")" compound_statement
#[derive(Debug, PartialEq, Clone)]
pub struct AstIfStatement {
    pub condition_statement: AstConditionalStatement,
    pub block: Box<AstStatement>,
}
impl AstIfStatement {
    pub fn new(mut condition_tokens: Tokens, body_tokens: Tokens) -> AstIfStatement {
        let condition_statement = AstConditionalStatement::new(&mut condition_tokens);
        let block = AstStatement::CompoundStatement(AstCompoundStatement::new(body_tokens));
        let block = Box::new(block);
        AstIfStatement {
            condition_statement,
            block,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstWhileStatement {
    pub condition_statement: AstConditionalStatement,
    pub block: Box<AstStatement>,
}
impl AstWhileStatement {
    pub fn new(condition_tokens: &mut Tokens, body_tokens: Tokens) -> AstWhileStatement {
        let condition_statement = AstConditionalStatement::new(condition_tokens);
        let block = AstStatement::CompoundStatement(AstCompoundStatement::new(body_tokens));
        let block = Box::new(block);
        AstWhileStatement {
            condition_statement,
            block,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstConditionalStatement {
    pub condition_identifier: AstIde,
    pub condition_operator: String,
    pub condition_val: AstVal,
}
impl AstConditionalStatement {
    fn new(tokens: &mut Tokens) -> AstConditionalStatement {
        let mut tokens: Tokens = trim_parentheses(tokens);
        tokens.reverse();
        let condition_identifier = AstIde::new(&mut tokens);
        let condition_operator = match tokens.pop_condop() {
            Ok(op) => op,
            Err(msg) => panic!(msg),
        };
        let condition_val = AstVal::new(&mut tokens);
        AstConditionalStatement {
            condition_identifier,
            condition_operator,
            condition_val,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstVal {
    Exp(AstExp),
    Fin(AstFin),
    Call(AstCall),
}
impl AstVal {
    pub fn new(tokens: &mut Tokens) -> AstVal {
        // validation
        match tokens.peak() {
            Some(token) => match token {
                Token::Ide(_) | Token::Num(_) => (),
                _ => panic!("Expect a value token"),
            },
            None => panic!("Expect a value token"),
        }
        // pattern matching
        match tokens.peak2() {
            Some(token) => match token {
                Token::ParenS => AstVal::Call(AstCall::new(tokens)), // Call
                Token::ArrOp(_) => AstVal::Fin(AstFin::Ide(AstIde::new(tokens))),
                _ => AstVal::Exp(AstExp::new(tokens)),
            },
            None => AstVal::Fin(AstFin::new(tokens)), // Fin
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstExp {
    pub op: AstOp,
    pub lhs: Box<AstVal>,
    pub rhs: Box<AstVal>,
}
impl AstExp {
    pub fn new(tokens: &mut Tokens) -> AstExp {
        let lhs = AstVal::Fin(AstFin::new(tokens));
        match parse_expression(lhs, 1, tokens) {
            AstVal::Exp(ast_exp) => return ast_exp,
            AstVal::Fin(_) => panic!("Unexpected pattern"),
            AstVal::Call(_) => panic!("Unexpected pattern"),
        }
    }
}
/// [Reference: Oprator-precedence parser](https://en.wikipedia.org/wiki/Operator-precedence_parser)
pub fn parse_expression(mut lhs: AstVal, min_precedence: u32, tokens: &mut Tokens) -> AstVal {
    let mut tokens = tokens;
    while condition1_is_ok(&tokens, min_precedence) {
        let op: AstOp = AstOp::new(tokens);
        let precedence = tokens.get_precedence(op.operator.clone());
        let mut rhs = AstVal::new(tokens);
        while condition2_is_ok(&tokens, precedence) {
            rhs = parse_expression(rhs, precedence, &mut tokens);
        }
        lhs = AstVal::Exp(AstExp {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        });
    }
    lhs
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstFin {
    Num(AstNum),
    Ide(AstIde),
}
impl AstFin {
    pub fn new(tokens: &mut Tokens) -> AstFin {
        match tokens.peak() {
            Some(token) => match token {
                Token::Num(_) => AstFin::Num(AstNum::new(tokens)),
                Token::Ide(_) => AstFin::Ide(AstIde::new(tokens)),
                _ => panic!("Expect a finite token"),
            },
            None => panic!("Expect a finite token"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstIde {
    Num(AstIdeNumber),
    Arr(AstIdeArray),
}
impl AstIde {
    pub fn new(tokens: &mut Tokens) -> AstIde {
        let identifier = match tokens.pop_identifier() {
            Ok(identifier) => identifier,
            Err(msg) => panic!(msg),
        };
        match tokens.peak() {
            Some(token) => match token {
                Token::ArrOp(_) => {
                    let num = match tokens.pop_arrop() {
                        Ok(num) => num,
                        Err(msg) => panic!(msg),
                    };
                    AstIde::Arr(AstIdeArray { identifier, num })
                }
                _ => AstIde::Num(AstIdeNumber { identifier }),
            },
            None => AstIde::Num(AstIdeNumber { identifier }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstIdeNumber {
    pub identifier: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstIdeArray {
    pub identifier: String,
    pub num: u32,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstNum {
    pub number: u64,
}
impl AstNum {
    pub fn new(tokens: &mut Tokens) -> AstNum {
        match tokens.pop_number() {
            Ok(number) => AstNum { number },
            Err(msg) => panic!(msg),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstCall {
    pub func_identifier: String,
    pub arguments: Vec<AstVal>,
}
impl AstCall {
    pub fn new(tokens: &mut Tokens) -> AstCall {
        let func_identifier = tokens.pop_identifier();
        let arguments = match parse_call_args(tokens) {
            Ok(arguments) => arguments,
            Err(msg) => panic!(msg),
        };
        match func_identifier {
            Ok(func_identifier) => AstCall {
                func_identifier,
                arguments,
            },
            Err(msg) => panic!(msg),
        }
    }
}
fn parse_call_args(tokens: &mut Tokens) -> Result<Vec<AstVal>, String> {
    let mut arguments: Vec<AstVal> = Vec::new();
    let mut tokens = match tokens.pop_paren() {
        Ok(mut tokens) => trim_parentheses(&mut tokens),
        Err(msg) => return Err(msg),
    };
    if let Some(tokens_vec) = tokens.split_with_comma() {
        for mut tokens in tokens_vec.into_iter() {
            let argument: AstVal = AstVal::new(&mut tokens);
            arguments.push(argument);
        }
    }
    Ok(arguments)
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstOp {
    pub operator: String,
}
impl AstOp {
    pub fn new(tokens: &mut Tokens) -> AstOp {
        match tokens.pop_operator() {
            Ok(operator) => AstOp { operator },
            Err(msg) => panic!(msg),
        }
    }
}
