use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Num(u64),
    Op(String),
    CondOp(String),
    Ide(String),
    ParenE,
    ParenS,
    BlockE,
    BlockS,
    If,
    While,
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
    pub fn new() -> Tokens {
        Tokens { tokens: vec![] }
    }
    pub fn get_precedence(&self, operator: String) -> u32 {
        // operator priorities
        let mut map = HashMap::new();
        map.insert(String::from("+"), 10);
        map.insert(String::from("*"), 20);
        map[&operator]
    }

    pub fn pop_while_statement(&mut self) -> Result<(Tokens, Tokens), String> {
        let msg = "Error@pop_while_statement: Expect an \"while\"".to_string();
        match self.pop() {
            Some(token) => match token {
                Token::While => (),
                _ => return Err(msg),
            },
            None => return Err(msg),
        }

        let msg = "Error@pop_while_statement: Expect a condition statement".to_string();
        let condition_tokens = match self.pop_paren() {
            Ok(tokens) => match tokens {
                Some(tokens) => tokens,
                None => return Err(msg),
            },
            Err(_) => return Err(msg),
        };

        let msg = "Error@pop_while_statement: Expect a compound statement".to_string();
        let body_tokens = match self.pop_block() {
            Ok(tokens) => match tokens {
                Some(tokens) => tokens,
                None => return Err(msg),
            },
            Err(_) => return Err(msg),
        };
        Ok((condition_tokens, body_tokens))
    }

    pub fn pop_if_statement(&mut self) -> Result<(Tokens, Tokens), String> {
        let msg = "Error@pop_if_statement: Expect an \"if\"".to_string();
        match self.pop() {
            Some(token) => match token {
                Token::If => (),
                _ => return Err(msg),
            },
            None => return Err(msg),
        };

        let msg = "Error@pop_if_statement: Expect a condition statement".to_string();
        let condition_tokens = match self.pop_paren() {
            Ok(tokens) => match tokens {
                Some(tokens) => tokens,
                None => return Err(msg),
            },
            Err(_) => return Err(msg),
        };

        let msg = "Error@pop_if_statement: Expect a compound statement".to_string();
        let body_tokens = match self.pop_block() {
            Ok(tokens) => match tokens {
                Some(tokens) => tokens,
                None => return Err(msg),
            },
            Err(_) => return Err(msg),
        };
        Ok((condition_tokens, body_tokens))
    }

    pub fn pop_parameters(&mut self) -> Result<Option<Tokens>, &str> {
        // parse function parameter
        let mut parameter_tokens: Vec<Token> = Vec::new();
        match self.tokens.pop() {
            Some(token) => {
                match token {
                    Token::ParenS => loop {
                        match self.tokens.pop() {
                            Some(token) => match token {
                                Token::Ide(_) => parameter_tokens.push(token),
                                Token::ParenE => break,
                                _ => return Err("Error&pop_function: Expect identifiers or ParenE, but got other"),
                            },
                            None => return Err("Error&pop_function: Expect identifiers or ParenE, but got nothing"),
                        }
                    },
                    _ => return Err("Error@pop_function: Expect ParenS, but got other"),
                }
            }
            None => return Ok(None),
        };
        Ok(Some(Tokens {
            tokens: parameter_tokens,
        }))
    }
    pub fn pop_paren(&mut self) -> Result<Option<Tokens>, &str> {
        let mut paren_tokens: Vec<Token> = Vec::new();
        let mut paren_s_cnt = 0;

        // Error handling for the first tokens
        match self.peak() {
            Some(token) => match token {
                Token::ParenS => (),
                _ => return Err("Error@pop_paren: Expect ParenS, but got other"),
            },
            None => return Err("Error@pop_paren: Expect a paren"),
        }
        loop {
            match self.tokens.pop() {
                Some(token) => {
                    paren_tokens.push(token.clone());
                    match token {
                            Token::ParenE => {
                                paren_s_cnt -= 1;
                                if paren_s_cnt == 0 {
                                    break;
                                }
                            },
                            Token::ParenS => {
                                paren_s_cnt += 1;
                            },
                            _ => (),
                    }
                },
                None => return Err("Error@pop_paren: It is expected that ParenE and ParenS exist in the same number."),
            }
        }
        Ok(Some(Tokens {
            tokens: paren_tokens,
        }))
    }
    pub fn pop_block(&mut self) -> Result<Option<Tokens>, String> {
        let mut block_tokens: Vec<Token> = Vec::new();
        let mut block_s_cnt = 0;

        // Error handling for the first tokens
        match self.peak() {
            Some(token) => match token {
                Token::BlockS => (),
                _ => return Err("Error@pop_block: Expect BlockS, but got other".to_string()),
            },
            None => return Err("Error@pop_block: Expect a block".to_string()),
        }
        loop {
            match self.tokens.pop() {
                Some(token) => {
                    block_tokens.push(token.clone());
                    match token {
                            Token::BlockE => {
                                block_s_cnt -= 1;
                                if block_s_cnt == 0 {
                                    break;
                                }
                            },
                            Token::BlockS => {
                                block_s_cnt += 1;
                            },
                            _ => (),
                    }
                },
                None => return Err("Error@pop_block: It is expected that BlockE and BlockS exist in the same number.".to_string()),
            }
        }
        Ok(Some(Tokens {
            tokens: block_tokens,
        }))
    }
    pub fn pop_instruction(&mut self) -> Result<Tokens, String> {
        let mut instruction_tokens: Vec<Token> = Vec::new();
        loop {
            match self.pop() {
                Some(token) => {
                    instruction_tokens.push(token.clone());
                    match token {
                        Token::Semi => {
                            return Ok(Tokens {
                                tokens: instruction_tokens,
                            })
                        }
                        _ => (),
                    }
                }
                None => return Err("Error@pop_instruction: Expect the last semicolon".to_string()),
            }
        }
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
    pub fn pop_condop(&mut self) -> Option<String> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::CondOp(op) => match op.as_ref() {
                    "==" => Some(op),
                    "!=" => Some(op),
                    _ => panic!("Not implemented operator"),
                },
                _ => panic!("Expect a conditional operator token"),
            },
            None => None,
        }
    }
    pub fn pop_ide(&mut self) -> Result<Option<String>, String> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::Ide(identifier) => Ok(Some(identifier)),
                _ => Err("Expect an identifier token".to_string()),
            },
            None => Ok(None),
        }
    }
    pub fn pop_fin(&mut self) -> Option<Token> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::Ide(_) | Token::Num(_) => Some(token),
                _ => panic!("Expect an identifier or number token"),
            },
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
}
