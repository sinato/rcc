use crate::lexer::token::{Token, Tokens};
use crate::parser::util::{
    condition1_is_ok, condition2_is_ok, trim_block_parentheses, trim_parentheses,
};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct AstFunction {
    pub identifier: String,
    pub statements: Vec<AstStatement>,
}
impl AstFunction {
    pub fn new(tokens: &mut Tokens) -> AstFunction {
        let identifier = match tokens.pop_ide() {
            Ok(identifier) => match identifier {
                Some(identifier) => identifier,
                None => panic!("Expect an identifier".to_string()),
            },
            Err(msg) => panic!(msg),
        };

        let _parameters = match tokens.pop_parameters() {
            Ok(parameters) => match parameters {
                Some(parameters) => parameters,
                None => Tokens::new(),
            },
            Err(msg) => panic!(msg.to_string()),
        };

        let body_tokens = match tokens.pop_block() {
            Ok(block_tokens) => match block_tokens {
                Some(block_tokens) => block_tokens,
                None => panic!("Expect at least one statement in a block".to_string()),
            },
            Err(msg) => panic!(msg),
        };
        let statements = parse_function_body(body_tokens);
        AstFunction {
            identifier,
            statements,
        }
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

#[derive(Debug, PartialEq, Clone)]
pub enum AstStatement {
    Instruction(AstInstruction),
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
                Ok((condition_tokens, body_tokens)) => AstStatement::WhileStatement(
                    AstWhileStatement::new(condition_tokens, body_tokens),
                ),
                Err(msg) => panic!(msg),
            },
            Token::BlockS => match tokens.pop_block() {
                Ok(body_tokens) => match body_tokens {
                    Some(body_tokens) => {
                        AstStatement::CompoundStatement(AstCompoundStatement::new(body_tokens))
                    }
                    None => panic!("Expect a block".to_string()),
                },
                Err(msg) => panic!(msg),
            },
            _ => match tokens.pop_instruction() {
                Ok(instruction_tokens) => {
                    AstStatement::Instruction(AstInstruction::new(instruction_tokens))
                }
                Err(msg) => panic!(msg),
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstInstruction {
    Bind(AstBinding),
    Return(AstReturn),
}
impl fmt::Display for AstInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstInstruction::Bind(ast) => write!(f, "BIND: {:?}\n", ast),
            AstInstruction::Return(ast) => write!(f, "Ret: {:?}\n", ast),
        }
    }
}
impl AstInstruction {
    fn new(mut tokens: Tokens) -> AstInstruction {
        tokens.pop();
        tokens.reverse();
        let token = tokens
            .peak()
            .expect("Parse Error: Expect at least one token.");
        match token {
            Token::Ide(_) => AstInstruction::Bind(AstBinding::new(tokens)),
            Token::Ret => AstInstruction::Return(AstReturn::new(tokens)),
            _ => panic!("Parse Error: Unexpected token."),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstCompoundStatement {
    Instructions(Vec<AstInstruction>),
}
impl AstCompoundStatement {
    pub fn new(mut tokens: Tokens) -> AstCompoundStatement {
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
            instructions.push(AstInstruction::new(instruction_tokens));
        }
        AstCompoundStatement::Instructions(instructions)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstIfStatement {
    pub condition_statement: AstConditionalStatement,
    pub block: Box<AstStatement>,
}
impl AstIfStatement {
    pub fn new(condition_tokens: Tokens, body_tokens: Tokens) -> AstIfStatement {
        let condition_statement = AstConditionalStatement::new(condition_tokens);
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
    pub fn new(condition_tokens: Tokens, body_tokens: Tokens) -> AstWhileStatement {
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
    fn new(mut tokens: Tokens) -> AstConditionalStatement {
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
        let condition_val = AstVal::new(&mut tokens);
        AstConditionalStatement {
            condition_identifier,
            condition_operator,
            condition_val,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstReturn {
    pub val: AstVal,
}
impl AstReturn {
    pub fn new(mut tokens: Tokens) -> AstReturn {
        match tokens.pop() {
            Some(token) => match token {
                Token::Ret => (),
                _ => panic!("parse error: expect token::ret"),
            },
            None => panic!("parse error: expect token::ret"),
        }
        AstReturn {
            val: AstVal::new(&mut tokens),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstBinding {
    pub ide: AstIde,
    pub val: AstVal,
}
impl AstBinding {
    pub fn new(mut tokens: Tokens) -> AstBinding {
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
        let val = AstVal::new(&mut tokens);
        AstBinding { ide, val }
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
            AstVal::Fin(_) => panic!(),
            AstVal::Call(_) => panic!(),
        }
    }
    pub fn get_op_string(&self) -> String {
        self.op.get_op()
    }
}
/// [Reference: Oprator-precedence parser](https://en.wikipedia.org/wiki/Operator-precedence_parser)
pub fn parse_expression(mut lhs: AstVal, min_precedence: u32, tokens: &mut Tokens) -> AstVal {
    let mut tokens = tokens;
    while condition1_is_ok(&tokens, min_precedence) {
        let op: AstOp = AstOp::new(tokens);
        let precedence = tokens.get_precedence(op.get_op());
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
impl fmt::Display for AstExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "OP: {:?}\nLHS: {:?}\nRHS: {:?}\n",
            self.op, *self.lhs, *self.rhs
        )
    }
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
pub struct AstIde {
    pub ide: Token,
}
impl AstIde {
    pub fn new(tokens: &mut Tokens) -> AstIde {
        let token = tokens.pop_fin();
        match token {
            Some(token) => match token {
                Token::Ide(_) => AstIde { ide: token },
                _ => panic!("Expect an identifier"),
            },
            None => panic!("Expect an identifier"),
        }
    }
    pub fn get_identifier(&self) -> String {
        match self.ide.clone() {
            Token::Ide(s) => s,
            _ => panic!("expect identifier"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstNum {
    pub num: Token,
}
impl AstNum {
    pub fn new(tokens: &mut Tokens) -> AstNum {
        let token = tokens.pop_fin();
        match token {
            Some(token) => match token {
                Token::Num(_) => AstNum { num: token },
                _ => panic!("Expect a number"),
            },
            None => panic!("Expect a number"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstCall {
    pub func_identifier: String,
}
impl AstCall {
    pub fn new(tokens: &mut Tokens) -> AstCall {
        let token = tokens.pop_fin();
        let _args = parse_function_args(tokens);
        match token {
            Some(token) => match token {
                Token::Ide(func_identifier) => AstCall { func_identifier },
                _ => panic!("Expect an identifier"),
            },
            None => panic!("Expect an identifier"),
        }
    }
}
fn parse_function_args(tokens: &mut Tokens) -> Result<Tokens, String> {
    let mut ret_tokens: Vec<Token> = Vec::new();
    match tokens.pop() {
        Some(token) => match token {
            Token::ParenS => ret_tokens.push(token),
            _ => return Err("Error@parse_function_args: Expect ParenS".to_string()),
        },
        None => return Err("Error@parse_function_args: Expect ParenS".to_string()),
    }
    match tokens.pop() {
        Some(token) => match token {
            Token::ParenE => ret_tokens.push(token),
            _ => return Err("Error@parse_function_args: Expect ParenE".to_string()),
        },
        None => return Err("Error@parse_function_args: Expect ParenE".to_string()),
    }
    Ok(Tokens { tokens: ret_tokens })
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstOp {
    pub op: Token,
}
impl AstOp {
    pub fn new(tokens: &mut Tokens) -> AstOp {
        match tokens.pop() {
            Some(token) => match token {
                Token::Op(_) => AstOp { op: token },
                _ => panic!("Expect an operator."),
            },
            None => panic!("Expect an operator."),
        }
    }
    pub fn get_op(&self) -> String {
        match self.op.clone() {
            Token::Op(s) => s,
            _ => panic!("expect operator"),
        }
    }
}
