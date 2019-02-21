use crate::lexer::Token;
use inkwell::builder::Builder;
use inkwell::context::Context;

#[derive(Debug, PartialEq)]
pub struct AstExp {
    op: Token,
    lhs: Token,
    rhs: Token,
}
impl AstExp {
    fn get_op_string(&self) -> String {
        match self.op.clone() {
            Token::Op(s) => s,
            _ => panic!("expect op token"),
        }
    }
    fn get_lhs_num(&self) -> u64 {
        match self.lhs {
            Token::Num(n) => n,
            _ => panic!("expect num token"),
        }
    }
    fn get_rhs_num(&self) -> u64 {
        match self.rhs {
            Token::Num(n) => n,
            _ => panic!("expect num token"),
        }
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

pub fn parser(tokens: Vec<Token>) -> Ast {
    match tokens.len() {
        1 => match tokens[0] {
            Token::Num(_) => Ast::Num(AstNum {
                num: tokens[0].clone(),
            }),
            Token::Op(_) => panic!("Not implemented"),
        },
        3 => {
            let lhs = match tokens[0] {
                Token::Num(_) => tokens[0].clone(),
                _ => panic!(format!(
                    "Parse Error: Expect Token::Num, but got {:?}",
                    tokens[0]
                )),
            };
            let op = match tokens[1].clone() {
                Token::Op(s) => match s.as_ref() {
                    "+" => tokens[1].clone(),
                    _ => panic!("Parse Error: Not implemented operator"),
                },
                _ => panic!(format!(
                    "Parse Error: Expect Token::Op, but got {:?}",
                    tokens[1]
                )),
            };
            let rhs = match tokens[2] {
                Token::Num(_) => tokens[2].clone(),
                _ => panic!(format!(
                    "Parse Error: Expect Token::Num, but got {:?}",
                    tokens[2]
                )),
            };
            Ast::Exp(AstExp { op, lhs, rhs })
        }
        _ => panic!("Not implemented"),
    }
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
            op: Token::Op(String::from("+")),
            lhs: Token::Num(10),
            rhs: Token::Num(20),
        });
        let tokens = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20)];
        let actual = parser(tokens);
        assert_eq!(actual, expect);
    }

}
