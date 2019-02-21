use crate::lexer::Token;
use inkwell::builder::Builder;
use inkwell::context::Context;

#[derive(Debug, PartialEq)]
pub struct AstExp {
    op: AstOp,
    lhs: AstNum,
    rhs: AstNum,
}
impl AstExp {
    fn get_op_string(&self) -> String {
        self.op.get_op()
    }
    fn get_lhs_num(&self) -> u64 {
        self.lhs.get_num()
    }
    fn get_rhs_num(&self) -> u64 {
        self.rhs.get_num()
    }
}

#[derive(Debug, PartialEq)]
pub struct AstNum {
    num: Token,
}
impl AstNum {
    fn get_num(&self) -> u64 {
        match self.num {
            Token::Num(n) => n,
            _ => panic!("expect num token"),
        }
    }
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Ast {
    Exp(AstExp),
    Num(AstNum),
}
impl Ast {
    pub fn emit(&self, context: &Context, builder: &Builder) {
        match self {
            Ast::Exp(ast_exp) => {
                let op = ast_exp.get_op_string();
                if op == String::from("+") {
                    let i32_type = context.i32_type();
                    let const_lhs_num = i32_type.const_int(ast_exp.get_lhs_num(), false);
                    let const_rhs_num = i32_type.const_int(ast_exp.get_rhs_num(), false);
                    let sum = builder.build_int_add(const_lhs_num, const_rhs_num, "main");
                    builder.build_return(Some(&sum));
                } else {
                    panic!("Emit Error: Not implemented operator");
                }
            }
            Ast::Num(ast_num) => {
                let num = ast_num.get_num();
                let a = context.i32_type().const_int(num, false);
                builder.build_return(Some(&a));
            }
        }
    }
}

pub fn parser(mut tokens: Vec<Token>) -> Ast {
    tokens.reverse();
    let token = tokens.pop();
    let lhs = match token {
        Some(token) => match token {
            Token::Num(_) => AstNum { num: token },
            _ => panic!("Parse Error: Expect a number token."),
        },
        None => panic!("Parse Error: Expect at least one token."),
    };
    if tokens.len() < 1 {
        return Ast::Num(lhs);
    }
    let token = tokens.pop();

    let op = match token {
        Some(token) => match token {
            Token::Op(operator) => match operator.as_ref() {
                "+" => AstOp {
                    op: Token::Op(operator),
                },
                _ => panic!("Parse Error: Not implemented oprator."),
            },
            _ => panic!("Parse Error: Expect operator token, but got otherwise."),
        },
        None => panic!("Parse Error: Expect operator token, but not exist."),
    };
    let token = tokens.pop();
    let rhs = match token {
        Some(token) => match token {
            Token::Num(_) => AstNum { num: token },
            _ => panic!("Parse Error: Expect number token, but got otherwise."),
        },
        None => panic!("Parse Error: Expect operator token, but not exist."),
    };

    Ast::Exp(AstExp { lhs, op, rhs })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_num() {
        let expect = Ast::Num(AstNum {
            num: Token::Num(2434),
        });
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
        let expect = Ast::Exp(AstExp {
            op: AstOp {
                op: Token::Op(String::from("+")),
            },
            lhs: AstNum {
                num: Token::Num(10),
            },
            rhs: AstNum {
                num: Token::Num(20),
            },
        });
        let tokens = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20)];
        let actual = parser(tokens);
        assert_eq!(actual, expect);
    }

}
