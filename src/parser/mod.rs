use crate::lexer::Token;

#[derive(Debug, PartialEq)]
struct AstExp {
    op: Token,
    lhs: Token,
    rhs: Token,
}
#[derive(Debug, PartialEq)]
struct AstNum {
    num: Token,
}

#[derive(Debug, PartialEq)]
enum Ast {
    Exp(AstExp),
    Num(AstNum),
}

fn parser(tokens: Vec<Token>) -> Ast {
    match tokens.len() {
        1 => match tokens[0] {
            Token::Num(_) => Ast::Num(AstNum {
                num: tokens[0].clone(),
            }),
            Token::Op(_) => panic!("Not implemented"),
        },
        3 => {
            // TODO: token type validation
            let lhs = tokens[0].clone();
            let op = tokens[1].clone();
            let rhs = tokens[2].clone();
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
