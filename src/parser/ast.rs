use crate::lexer::token::Token;
use std::fmt;


pub struct AstFunction {
    pub instructions: Vec<AstInstruction>,
}
impl AstFunction {
    pub fn new(instructions: Vec<AstInstruction>) -> AstFunction {
        AstFunction { instructions }
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
#[derive(Debug, PartialEq, Clone)]
pub struct AstReturn {
    pub val: AstVal,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstBinding {
    pub ide: AstIde,
    pub val: Box<AstVal>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstVal {
    Exp(AstExp),
    Fin(AstFin),
}
impl AstVal {
    pub fn new_from_token_fin(token: Token) -> AstVal {
        match token {
            Token::Num(num) => AstVal::Fin(AstFin::new_from_num_token(Token::Num(num))),
            Token::Ide(ide) => AstVal::Fin(AstFin::new_from_ide_token(Token::Ide(ide))),
            _ => panic!("Unexpected")
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
    pub fn get_op_string(&self) -> String {
        self.op.get_op()
    }
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
pub struct AstIde {
    pub ide: Token,
}
impl AstIde {
    pub fn get_identifier(&self) -> String {
        match self.ide.clone() {
            Token::Ide(s) => s,
            _ => panic!("expect identifier"),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum AstFin {
    Num(AstNum),
    Ide(AstIde),
}
impl AstFin {
    pub fn new_from_num_token(num: Token) -> AstFin {
        AstFin::Num(AstNum { num })
    }
    pub fn new_from_ide_token(ide: Token) -> AstFin {
        AstFin::Ide(AstIde { ide })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstNum {
    pub num: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstOp {
    pub op: Token,
}
impl AstOp {
    fn get_op(&self) -> String {
        match self.op.clone() {
            Token::Op(s) => s,
            _ => panic!("expect operator"),
        }
    }
}
