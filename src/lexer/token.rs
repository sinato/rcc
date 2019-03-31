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
        let msg = "Expect an \"while\"".to_string();
        match self.pop() {
            Some(token) => match token {
                Token::While => (),
                _ => return Err(msg),
            },
            None => return Err(msg),
        }

        let msg = "Expect a condition statement".to_string();
        let condition_tokens = match self.pop_paren() {
            Ok(tokens) => match tokens {
                Some(tokens) => tokens,
                None => return Err(msg),
            },
            Err(_) => return Err(msg),
        };

        let msg = "Expect a compound statement".to_string();
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
        let msg = "Expect an \"if\"".to_string();
        match self.pop() {
            Some(token) => match token {
                Token::If => (),
                _ => return Err(msg),
            },
            None => return Err(msg),
        };

        let msg = "Expect a condition statement".to_string();
        let condition_tokens = match self.pop_paren() {
            Ok(tokens) => match tokens {
                Some(tokens) => tokens,
                None => return Err(msg),
            },
            Err(_) => return Err(msg),
        };

        let msg = "Expect a compound statement".to_string();
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
            Some(token) => match token {
                Token::ParenS => loop {
                    match self.tokens.pop() {
                        Some(token) => match token {
                            Token::Ide(_) => parameter_tokens.push(token),
                            Token::ParenE => break,
                            _ => return Err("Expect identifiers or ParenE, but got other"),
                        },
                        None => return Err("Expect identifiers or ParenE, but got nothing"),
                    }
                },
                _ => return Err("Expect ParenS, but got other"),
            },
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
                _ => return Err("Expect ParenS"),
            },
            None => return Err(" Expect ParenS"),
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
                        }
                        Token::ParenS => {
                            paren_s_cnt += 1;
                        }
                        _ => (),
                    }
                }
                None => {
                    return Err("It is expected that ParenE and ParenS exist in the same number.")
                }
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
                _ => return Err("Expect BlockS, but got other".to_string()),
            },
            None => return Err("Expect a block".to_string()),
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
                        }
                        Token::BlockS => {
                            block_s_cnt += 1;
                        }
                        _ => (),
                    }
                }
                None => {
                    return Err(
                        "It is expected that BlockE and BlockS exist in the same number."
                            .to_string(),
                    )
                }
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
                None => return Err("Expect the last semicolon".to_string()),
            }
        }
    }
    pub fn pop_operator(&mut self) -> Result<String, String> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::Op(op) => Ok(op),
                _ => Err("Expect an operator token".to_string()),
            },
            None => Err("Expect an operator token".to_string()),
        }
    }
    pub fn pop_condop(&mut self) -> Result<String, String> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::CondOp(op) => Ok(op),
                _ => Err("Expect a conditional operator token".to_string()),
            },
            None => Err("Expect a conditional operator token".to_string()),
        }
    }
    pub fn pop_identifier(&mut self) -> Result<String, String> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::Ide(identifier) => Ok(identifier),
                _ => Err("Expect an identifier token".to_string()),
            },
            None => Err("Expect an identifier token".to_string()),
        }
    }
    pub fn pop_number(&mut self) -> Result<u64, String> {
        match self.tokens.pop() {
            Some(token) => match token {
                Token::Num(number) => Ok(number),
                _ => Err("Expect a number token".to_string()),
            },
            None => Err("Expect a number token".to_string()),
        }
    }
    pub fn pop(&mut self) -> Option<Token> {
        self.tokens.pop()
    }
    pub fn peak(&self) -> Option<Token> {
        self.clone().tokens.pop()
    }
    pub fn peak2(&self) -> Option<Token> {
        let mut tokens = self.clone();
        tokens.pop();
        tokens.pop()
    }
    pub fn reverse(&mut self) {
        self.tokens.reverse()
    }
}
