use crate::lexer::token::Token;
use inkwell::values::PointerValue;
use std::collections::HashMap;
use std::fmt;


#[derive(Debug, PartialEq, Clone)]
pub struct Statements {
    pub asts: Vec<Ast>,
    variables: HashMap<String, PointerValue>,
}
impl Statements {
    pub fn new(asts: Vec<Ast>) -> Statements {
        let variables = HashMap::new();
        Statements { asts, variables }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstBinding {
    pub ide: AstIde,
    pub val: Box<AstNode>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstBinaryExp {
    pub op: AstOp,
    pub lhs: Box<AstNode>,
    pub rhs: Box<AstNode>,
}
impl AstBinaryExp {
    pub fn get_op_string(&self) -> String {
        self.op.get_op()
    }
}
impl fmt::Display for AstBinaryExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "OP: {:?}\nLHS: {:?}\nRHS: {:?}\n",
            self.op, *self.lhs, *self.rhs
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstFin {
    pub fin: AstFinEnum,
}
impl AstFin {
    pub fn new_from_num_token(num: Token) -> AstFin {
        AstFin { fin: AstFinEnum::Num(AstNum { num })}
    }
    pub fn new_from_ide_token(ide: Token) -> AstFin {
        AstFin { fin: AstFinEnum::Ide(AstIde { ide })}
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum AstFinEnum {
    Num(AstNum),
    Ide(AstIde),
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
pub enum AstNode {
    Exp(AstBinaryExp),
    Fin(AstFin),
    Bind(AstBinding),
}
impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstNode::Exp(ast_binary_exp) => write!(f, "EXP: {}\n", ast_binary_exp),
            AstNode::Fin(ast) => write!(f, "FIN: {:?}\n", ast),
            AstNode::Bind(ast_binding) => write!(f, "BIND: {:?}\n", ast_binding),
        }
    }
}
impl AstNode {
    pub fn new_from_token_fin(token: Token) -> AstNode {
        match token {
            Token::Num(num) => AstNode::Fin(AstFin::new_from_num_token(Token::Num(num))),
            Token::Ide(ide) => AstNode::Fin(AstFin::new_from_ide_token(Token::Ide(ide))),
            _ => panic!("Unexpected")
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ast {
    pub ast: AstNode,
}
