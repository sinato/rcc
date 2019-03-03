use crate::lexer::token::Token;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::IntValue;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Statements {
    pub asts: Vec<Ast>
}
impl Statements {
    pub fn new(asts: Vec<Ast>) -> Statements {
        Statements { asts }
    }
    pub fn pop(&mut self) -> Option<Ast> {
        self.asts.pop()
    }
    pub fn emit(&self, context: &Context, builder: &Builder) {

        let ret = self.asts.iter().map(|ast| ast.emit(context, builder)).last();
        match ret {
            Some(ret) => builder.build_return(Some(&ret)),
            None => panic!("Emit Error: Expect at least an ast."),
        };
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct AstBinaryExp {
    pub op: AstOp,
    pub lhs: Box<AstNode>,
    pub rhs: Box<AstNode>,
}
impl AstBinaryExp {
    fn get_op_string(&self) -> String {
        self.op.get_op()
    }
    fn get_lhs_num(&self, context: &Context, builder: &Builder) -> IntValue {
        let node = *self.lhs.clone();
        node.emit(context, builder)
    }
    fn get_rhs_num(&self, context: &Context, builder: &Builder) -> IntValue {
        let node = *self.rhs.clone();
        node.emit(context, builder)
    }
    fn emit(&self, context: &Context, builder: &Builder) -> IntValue {
        let lhs_num = self.get_lhs_num(context, builder);
        let rhs_num = self.get_rhs_num(context, builder);
        let op = self.get_op_string();
        match op.as_ref() {
            "+" => builder.build_int_add(lhs_num, rhs_num, "sum"),
            "*" => builder.build_int_mul(lhs_num, rhs_num, "mul"),
            _ => panic!("Emit Error: Not implemented operator"),
        }
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
pub struct AstNum {
    pub num: Token,
}
impl AstNum {
    fn emit(&self, context: &Context, _builder: &Builder) -> IntValue {
        context.i32_type().const_int(self.num.get_num(), false)
    }
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
pub enum AstNode {
    Exp(AstBinaryExp),
    Num(AstNum),
}
impl AstNode {
    pub fn emit(&self, context: &Context, builder: &Builder) -> IntValue {
        match self {
            AstNode::Exp(ast_binary_exp) => ast_binary_exp.emit(context, builder),
            AstNode::Num(ast_num) => ast_num.emit(context, builder),
        }
    }
}
impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstNode::Exp(ast_binary_exp) => write!(f, "EXP: {}\n", ast_binary_exp),
            AstNode::Num(ast_num) => write!(f, "NUM: {:?}\n", ast_num),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ast {
    pub ast: AstNode,
}
impl Ast {
    pub fn emit(&self, context: &Context, builder: &Builder) -> IntValue {
        self.ast.emit(context, builder)
    }
}
