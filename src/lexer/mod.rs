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
}

pub fn lexer(code: String) -> Vec<Token> {
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
        } else if first_char == '+' {
            tokens.push(Token::Op(element.to_string()));
        } else {
            panic!(format!("This token is not implemented: {:?}", element));
        }
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_add() {
        let code = String::from("10 + 20");
        let expect = vec![Token::Num(10), Token::Op(String::from("+")), Token::Num(20)];
        let actual = lexer(code);
        assert_eq!(actual, expect);
    }
}
