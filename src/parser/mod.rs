use crate::lexer::Token;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::IntValue;
use log::debug;

#[derive(Debug, PartialEq, Clone)]
pub struct AstBinaryExp {
    op: AstOp,
    lhs: Box<AstNode>,
    rhs: Box<AstNode>,
}
impl AstBinaryExp {
    fn get_op_string(&self) -> String {
        self.op.get_op()
    }
    fn get_lhs_num(&self, context: &Context, builder: &Builder) -> IntValue {
        let node = *self.lhs.clone();
        match node {
            AstNode::Exp(ast_exp) => ast_exp.emit(context, builder),
            AstNode::Num(ast_num) => ast_num.emit(context, builder),
        }
    }
    fn get_rhs_num(&self, context: &Context, builder: &Builder) -> IntValue {
        let node = *self.rhs.clone();
        match node {
            AstNode::Exp(ast_exp) => ast_exp.emit(context, builder),
            AstNode::Num(ast_num) => ast_num.emit(context, builder),
        }
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

#[derive(Debug, PartialEq, Clone)]
pub struct AstNum {
    num: Token,
}
impl AstNum {
    fn emit(&self, context: &Context, _builder: &Builder) -> IntValue {
        context.i32_type().const_int(self.num.get_num(), false)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstOp {
    op: Token,
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
    pub fn emit(&self, context: &Context, builder: &Builder) {
        let ret = match self {
            AstNode::Exp(ast_binary_exp) => ast_binary_exp.emit(context, builder),
            AstNode::Num(ast_num) => ast_num.emit(context, builder),
        };
        builder.build_return(Some(&ret));
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ast {
    ast: AstNode,
}
impl Ast {
    pub fn emit(&self, context: &Context, builder: &Builder) {
        self.ast.emit(context, builder);
    }
}

pub fn parser(mut tokens: Vec<Token>) -> Ast {
    tokens.reverse();
    let token = tokens.pop();
    let mut lhs = match token {
        Some(token) => match token {
            Token::Num(_) => AstNode::Num(AstNum { num: token }),
            _ => panic!("Parse Error: Expect a number token."),
        },
        None => panic!("Parse Error: Expect at least one token."),
    };

    debug!("FIRST LHS: {:?}", lhs);
    loop {
        let token = tokens.pop();
        let op = match token {
            Some(token) => match token {
                Token::Op(operator) => match operator.as_ref() {
                    "+" | "*" => AstOp {
                        op: Token::Op(operator),
                    },
                    _ => panic!("Parse Error: Not implemented oprator."),
                },
                _ => panic!("Parse Error: Expect operator token, but got otherwise."),
            },
            None => break,
        };
        let token = tokens.pop();
        let rhs = match token {
            Some(token) => match token {
                Token::Num(_) => AstNode::Num(AstNum { num: token }),
                _ => panic!("Parse Error: Expect number token, but got otherwise."),
            },
            None => panic!("Parse Error: Expect operator token, but not exist."),
        };
        let boxed_lhs = Box::new(lhs);
        let rhs = Box::new(rhs);
        lhs = AstNode::Exp(AstBinaryExp {
            lhs: boxed_lhs,
            op,
            rhs,
        });
        debug!("LHS:       {:?}", lhs);
    }
    debug!("LAST LHS:  {:?}", lhs);
    Ast { ast: lhs }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_num() {
        let expect = Ast {
            ast: AstNode::Num(AstNum {
                num: Token::Num(2434),
            }),
        };
        let tokens = vec![Token::Num(2434)];
        let actual = parser(tokens);
        assert_eq!(actual, expect);
    }

    #[test]
    #[should_panic]
    fn test_parser_num_illegal() {
        let tokens = vec![Token::Op(String::from("+"))];
        parser(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_illegal_tokens() {
        let tokens = vec![Token::Num(2434), Token::Op(String::from("+"))];
        parser(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_illegal_tokens() {
        let tokens = vec![Token::Num(2434), Token::Num(2434), Token::Num(2434)];
        parser(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_illegal_tokens2() {
        let tokens = vec![
            Token::Num(2434),
            Token::Op(String::from("+")),
            Token::Op(String::from("+")),
        ];
        parser(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_noimplemented_operator() {
        let tokens = vec![
            Token::Num(2434),
            Token::Op(String::from("-")),
            Token::Num(2434),
        ];
        parser(tokens);
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
        let actual = parser(tokens);
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_parser_exp_three_terms() {
        // make expected lhs
        let lhs = Box::new(AstNode::Num(AstNum {
            num: Token::Num(10),
        }));
        let op = AstOp {
            op: Token::Op(String::from("+")),
        };
        let rhs = Box::new(AstNode::Num(AstNum {
            num: Token::Num(20),
        }));
        let lhs = Box::new(AstNode::Exp(AstBinaryExp { lhs, op, rhs }));

        // make expected ast
        let op = AstOp {
            op: Token::Op(String::from("*")),
        };
        let rhs = Box::new(AstNode::Num(AstNum {
            num: Token::Num(30),
        }));
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
        let actual = parser(tokens);
        assert_eq!(actual, expect);
    }

}
