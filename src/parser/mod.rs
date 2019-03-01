use crate::lexer::{Token, Tokens};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::values::IntValue;
use log::debug;
use std::fmt;

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
    ast: AstNode,
}
impl Ast {
    pub fn emit(&self, context: &Context, builder: &Builder) {
        let ret = self.ast.emit(context, builder);
        builder.build_return(Some(&ret));
    }
}

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

fn parse_statement(tokens: Tokens) -> Tokens {
    let tokens: Vec<Token> = tokens.get_tokens();
    let mut parsed_tokens: Vec<Vec<Token>> = Vec::new();
    let mut tmp_tokens: Vec<Token> = Vec::new();
    for token in tokens {
        match token {
            Token::Semi => {
                let t = tmp_tokens.clone();
                parsed_tokens.push(t);
                tmp_tokens.clear();
            },
            _ => tmp_tokens.push(token),
        }
    }
    // TODO: Implement a pattern when the last Semi was not found.
    Tokens { tokens: parsed_tokens.pop().expect("Expect at least a token.") }
}

pub fn parser(tokens: Tokens) -> Ast {
    let mut tokens = parse_statement(tokens);
    tokens.reverse();
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
    debug!("AST:\n {}", lhs);

    Ast { ast: lhs }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_statement() {
        let tokens = vec![Token::Num(77)];
        let expect = Tokens { tokens };

        let tokens = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(10), Token::Semi, Token::Num(77), Token::Semi];
        let tokens  = Tokens { tokens };
        assert_eq!(parse_statement(tokens), expect);
    }

    #[test]
    fn test_parser_num() {
        let expect = Ast {
            ast: AstNode::Num(AstNum {
                num: Token::Num(2434),
            }),
        };
        let tokens = vec![Token::Num(2434), Token::Semi];
        let tokens = Tokens { tokens };
        let actual = parser(tokens);
        assert_eq!(actual, expect);
    }

    #[test]
    #[should_panic]
    fn test_parser_num_illegal() {
        let tokens = vec![Token::Op(String::from("+")), Token::Semi];
        let tokens = Tokens { tokens };
        parser(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_illegal_tokens() {
        let tokens = vec![Token::Num(2434), Token::Op(String::from("+")), Token::Semi];
        let tokens = Tokens { tokens };
        parser(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_illegal_tokens() {
        let tokens = vec![Token::Num(2434), Token::Num(2434), Token::Num(2434), Token::Semi];
        let tokens = Tokens { tokens };
        parser(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_illegal_tokens2() {
        let tokens = vec![
            Token::Num(2434),
            Token::Op(String::from("+")),
            Token::Op(String::from("+")),
            Token::Semi,
        ];
        let tokens = Tokens { tokens };
        parser(tokens);
    }

    #[test]
    #[should_panic]
    fn test_parser_exp_noimplemented_operator() {
        let tokens = vec![
            Token::Num(2434),
            Token::Op(String::from("-")),
            Token::Num(2434),
            Token::Semi,
        ];
        let tokens = Tokens { tokens };
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
        let tokens = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20), Token::Semi];
        let tokens = Tokens { tokens };
        let actual = parser(tokens);
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
            Token::Semi,
        ];
        let tokens = Tokens { tokens };
        let actual = parser(tokens);
        assert_eq!(actual, expect);
    }

}
