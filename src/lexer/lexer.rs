use regex::Regex;
use log::debug;
use crate::lexer::token::*;

fn strip_mock_main(code: String) -> String {
    let code = code.trim_start_matches("int main() {\n");
    code.trim_end_matches("\n}\n").to_string()
}

fn lex(code: String) -> Tokens {
    tokenize(code)
}

fn tokenize(code: String) -> Tokens {
    let token_patterns = vec![
        ("NUM", r"\d+(\.\d)*"),
        ("OP", r"[+*]"),
        ("SEMI", r";"),
    ];
    let re = make_regex(&token_patterns);
    let names = get_names(&token_patterns);
    let re = Regex::new(&re).expect("something went wrong making the regex");

    let mut tokens: Vec<Token> = Vec::new();
    for caps in re.captures_iter(&code) {
        debug!("caps:  {:?}", caps);
        let mut typ = String::from("nil");
        let val = String::from(&caps[0]);
        for name in &names {
            if caps.name(name).is_some() {
                typ = name.to_string();
            }
        }
        match typ.as_ref() {
            "NUM" => tokens.push(Token::Num(val.parse::<u64>().expect("something went wrong parsing a number"))),
            "OP" => tokens.push(Token::Op(val)),
            "SEMI" => tokens.push(Token::Semi),
            _ => panic!("This is not an expected panic"),
        }
        debug!("tokens:  {:?}", tokens);
    }
    Tokens { tokens }
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


pub fn lexer(code: String) -> Tokens {
    let code = strip_mock_main(code);
    lex(code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::prelude::*;
    use std::fs::File;

    fn get_code(filename: &str) -> String {
        let filename = String::from("./tests/resources/") + filename;
        let mut f = File::open(filename).expect("file not found");
        let mut contents = String::new();
        f.read_to_string(&mut contents).expect("somethig went wrong reading the file");
        contents
    }

    #[test]
    fn test_lexer_add() {
        let code = String::from("10 + 20");
        let expect = Tokens {
            tokens: vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20)],
        };
        let actual = lex(code);
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_lexer_mul() {
        let code = String::from("10 * 20");
        let expect = Tokens {
            tokens: vec![Token::Num(10), Token::Op(String::from("*")), Token::Num(20)],
        };
        let actual = lex(code);
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_strip_mock_main() {
        let code = get_code("test_one_num");
        let code = strip_mock_main(code);
        let expect = String::from("    10;");
        assert_eq!(code, expect);
    }

    #[test]
    fn test_lex_expression() {
        let code = strip_mock_main(String::from("      10 + 20;\n    100"));
        let expect = Tokens{ tokens:  vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20), Token::Semi, Token::Num(100)] };
        assert_eq!(lex(code), expect);
    }
}
