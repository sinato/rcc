use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Num(u64),
    Op(String),
    Ide(String),
    BlockE,
    BlockS,
    If,
    Ret,
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
    pub fn get_ide(&self) -> String {
        match self.clone() {
            Token::Ide(ide) => ide,
            _ => panic!("Token Error: Expcet an identifier token"),
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
    pub fn pop_op(&mut self) -> Option<String> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::Op(op) => match op.as_ref() {
                    "+" | "*" | "=" => Some(op),
                    _ => panic!("Not implemented operator"),
                },
                _ => panic!("Expect an operator token"),
            },
            None => None,
        }
    }
    pub fn pop_ide(&mut self) -> Option<String> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::Ide(identifier) => Some(identifier),
                _ => panic!("Expect an identifier token"),
            },
            None => None,
        }
    }
    pub fn pop_fin(&mut self) -> Option<Token> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::Ide(_) | Token::Num(_) => Some(token),
                _ => panic!("Expect an identifier or number token"),
            }
            None => None,
        }
    }
    pub fn pop(&mut self) -> Option<Token> {
        self.tokens.pop()
    }
    pub fn peak(&self) -> Option<Token> {
        self.clone().tokens.pop()
    }
    pub fn reverse(&mut self) {
        self.tokens.reverse()
    }
    pub fn get_tokens(self) -> Vec<Token> {
        self.tokens
    }
}

