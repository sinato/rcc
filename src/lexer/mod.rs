use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Num(u64),
    Op(String),
}
impl Token {
    pub fn get_num(&self) -> u64 {
        match self.clone() {
            Token::Num(num) => num,
            _ => panic!("Token Error: Expcet a number token"),
        }
    }
    pub fn get_op(&self) -> String {
        match self.clone() {
            Token::Op(op) => op,
            _ => panic!("Token Error: Expcet an operator token"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Tokens {
    pub tokens: Vec<Token>,
}
impl Tokens {
    pub fn get_precedence(&self, operator: String) -> u32 {
        // operator priorities
        let mut map = HashMap::new();
        map.insert(String::from("+"), 10);
        map.insert(String::from("*"), 20);
        map[&operator]
    }
    pub fn pop_num(&mut self) -> Option<u64> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::Num(num) => Some(num),
                _ => panic!("Expect a number token"),
            },
            None => None,
        }
    }
    pub fn pop_op(&mut self) -> Option<String> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::Op(op) => match op.as_ref() {
                    "+" | "*" => Some(op),
                    _ => panic!("Not implemented operator"),
                },
                _ => panic!("Expect an operator token"),
            },
            None => None,
        }
    }
    pub fn pop(&mut self) -> Option<Token> {
        self.tokens.pop()
    }
    pub fn reverse(&mut self) {
        self.tokens.reverse()
    }
}

pub fn lexer(code: String) -> Tokens {
    let elements = code.split(" ").collect::<Vec<&str>>();
    println!("elements: {:?}", elements);

    let mut tokens: Vec<Token> = Vec::new();
    for element in elements.iter() {
        let first_char = element
            .chars()
            .nth(0)
            .expect("Lexing error: illigal input.");
        if first_char.is_digit(10) {
            let num = element
                .parse::<u64>()
                .expect(&format!("Expect a number, but got {}", element));
            tokens.push(Token::Num(num));
        } else if first_char == '+' || first_char == '*' {
            tokens.push(Token::Op(element.to_string()));
        } else {
            panic!(format!("This token is not implemented: {:?}", element));
        }
    }
    Tokens { tokens }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_add() {
        let code = String::from("10 + 20");
        let expect = Tokens {
            tokens: vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20)],
        };
        let actual = lexer(code);
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_lexer_mul() {
        let code = String::from("10 * 20");
        let expect = Tokens {
            tokens: vec![Token::Num(10), Token::Op(String::from("*")), Token::Num(20)],
        };
        let actual = lexer(code);
        assert_eq!(actual, expect);
    }
}
