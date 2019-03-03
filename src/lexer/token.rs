use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Num(u64),
    Op(String),
    Ide(String),
    Semi,
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
    pub fn get_tokens(self) -> Vec<Token> {
        self.tokens
    }
}

