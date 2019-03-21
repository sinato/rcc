use regex::Regex;
use log::debug;
use crate::lexer::token::*;

pub struct Lexer {
    re: Regex,
    names: Vec<&'static str>,
}
impl Lexer {
    // static constructor
    pub fn new() -> Lexer {
        let token_patterns = vec![
            ("NUM", r"\d+(\.\d)*"),
            ("CONDOP", r"==|!="),
            ("OP", r"[+*=]"),
            ("SEMI", r";"),
            ("PAREN_S", r"\("),
            ("PAREN_E", r"\)"),
            ("BLOCK_S", r"\{"),
            ("BLOCK_E", r"\}"),
            ("INT", r"int"),
            ("RET", r"return "),
            ("IF", r"if"),
            ("WHILE", r"while"),
            ("IDEN", r"[a-z]+"),
        ];
        let re = make_regex(&token_patterns);
        let names = get_names(&token_patterns);
        let re = Regex::new(&re).expect("something went wrong making the regex");
        Lexer { re, names }
    }
    pub fn lex(&self, code: String) -> Tokens {
        let tokens = self.tokenize(code);
        tokens
    }
    fn tokenize(&self, code: String) -> Tokens {
        let mut tokens: Vec<Token> = Vec::new();
        for caps in self.re.captures_iter(&code) {
            let mut typ = String::from("nil");
            let val = String::from(&caps[0]);
            for name in &self.names {
                if caps.name(name).is_some() {
                    typ = name.to_string();
                }
            }
            match typ.as_ref() {
                "NUM" => tokens.push(Token::Num(val.parse::<u64>().expect("something went wrong parsing a number"))),
                "CONDOP" => tokens.push(Token::CondOp(val)),
                "OP" => tokens.push(Token::Op(val)),
                "SEMI" => tokens.push(Token::Semi),
                "IDEN" => tokens.push(Token::Ide(val)),
                "PAREN_S" => tokens.push(Token::ParenS),
                "PAREN_E" => tokens.push(Token::ParenE),
                "BLOCK_S" => tokens.push(Token::BlockS),
                "BLOCK_E" => tokens.push(Token::BlockE),
                "INT" => (),
                "IF" => tokens.push(Token::If),
                "WHILE" => tokens.push(Token::While),
                "RET" => tokens.push(Token::Ret),
                _ => panic!("This is not an expected panic"),
            }
        }
        debug!("tokens:  {:?}", tokens);
        Tokens { tokens }
    }
}
fn make_regex(token_patterns: &Vec<(&str, &str)>) -> String {
    token_patterns
        .into_iter()
        .map(|pattern| format!("(?P<{}>{})", pattern.0, pattern.1))
        .collect::<Vec<String>>()
        .join("|")
}

fn get_names<'a, 'b>(token_patterns: &Vec<(&'a str, &'b str)>) -> Vec<&'a str> {
    token_patterns
        .into_iter()
        .map(|pattern| pattern.0)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_lexer() -> Lexer {
        Lexer::new()
    }

    #[test]
    fn test_lexer_add() {
        let lexer = get_lexer();
        let code = String::from("10 + 20");
        let expect = Tokens {
            tokens: vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20)],
        };
        let actual = lexer.lex(code);
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_lexer_mul() {
        let lexer = get_lexer();
        let code = String::from("10 * 20");
        let expect = Tokens {
            tokens: vec![Token::Num(10), Token::Op(String::from("*")), Token::Num(20)],
        };
        let actual = lexer.lex(code);
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_lex_binding() {
        let lexer = get_lexer();
        let code = String::from("abc = 10;");
        let expect = Tokens {
            tokens: vec![Token::Ide(String::from("abc")), Token::Op(String::from("=")), Token::Num(10), Token::Semi],
        };
        assert_eq!(lexer.tokenize(code), expect);
    }

    #[test]
    fn test_lex_expression() {
        let lexer = get_lexer();
        let code = String::from("      10 + 20;\n    100");
        let expect = Tokens{ tokens:  vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20), Token::Semi, Token::Num(100)] };
        assert_eq!(lexer.lex(code), expect);
    }
}
