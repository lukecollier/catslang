use crate::token::{lookup_ident, Token};
use anyhow::*;

//todo: The tokens could contain references instead of Strings
#[derive(Debug, Clone, Copy)]
pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Result::Ok(Token::EOF) => None,
            token => Some(token),
        }
    }
}

// todo: Impl iterator for this
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input: input.as_bytes(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        l
    }

    fn peek_char(&mut self) -> Option<u8> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input[self.read_position])
        }
    }

    fn has_next(&self) -> bool {
        self.position >= self.input.len()
    }

    fn is_escaped_quote(&mut self) -> bool {
        self.ch == b'\\' && self.peek_char().is_some_and(|ch| ch == b'"')
    }

    /**
     * Read's strings from the current position until a closing string is found
     * for example "hello" => hello
     */
    fn read_string(&mut self) -> Result<String> {
        self.read_char();
        let mut position = self.position;
        let mut string = String::new();

        while self.ch.is_ascii() && (self.ch != b'"' || self.is_escaped_quote()) && !self.has_next()
        {
            // new line
            if self.ch == b'\\' && self.peek_char().is_some_and(|ch| ch == b'n') {
                let utf8_str = std::str::from_utf8(&self.input[position..self.position])?;
                string.push_str(utf8_str);
                string.push(b'\n' as char);
                self.read_char();
                self.read_char();
                position = self.position;
                // tabs
            } else if self.ch == b'\\' && self.peek_char().is_some_and(|ch| ch == b't') {
                let utf8_str = std::str::from_utf8(&self.input[position..self.position])?;
                string.push_str(utf8_str);
                string.push(b'\t' as char);
                self.read_char();
                self.read_char();
                position = self.position;
                // "
            } else if self.is_escaped_quote() {
                let utf8_str = std::str::from_utf8(&self.input[position..self.position])?;
                string.push_str(utf8_str);
                self.read_char();
                position = self.position;
                self.read_char();
            } else {
                self.read_char();
            }
        }
        if self.ch == b'"' {
            let utf8_str = std::str::from_utf8(&self.input[position..self.position])?;
            string.push_str(utf8_str);
            Ok(string)
        } else {
            // todo: Error messages can be vastly imrpoved with line loc's
            Err(anyhow!("string was never closed"))
        }
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;
        while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
            self.read_char()
        }
        std::str::from_utf8(&self.input[position..self.position]).unwrap()
        // todo! Error handling
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char()
        }
        std::str::from_utf8(&self.input[position..self.position]).unwrap()
        // todo! Error handling
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0
        } else {
            self.ch = self.input[self.read_position]
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();
        if self.read_position > self.input.len() {
            return Ok(Token::EOF);
        }
        let token = match self.ch {
            b'+' => Token::Plus,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'[' => Token::LBracket,
            b']' => Token::RBracket,
            b'-' => Token::Minus,
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b':' => Token::Colon,
            b'>' => Token::Gt,
            b'<' => Token::Lt,
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'=' => {
                if self.peek_char().is_some_and(|byte| byte == b'=') {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            b'!' => {
                if self.peek_char().is_some_and(|byte| byte == b'=') {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            b'"' => Token::Str(self.read_string()?),
            _ => {
                if self.ch.is_ascii_alphabetic() {
                    return Ok(lookup_ident(self.read_identifier()));
                }
                if self.ch.is_ascii_digit() {
                    return Ok(Token::Int(self.read_number().to_string()));
                }
                Token::Illegal
            }
        };
        self.read_char();
        Ok(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn next_number() {
        let source = r#"let b = 500;"#;
        let expected_tokens: [Token; 5] = [
            Token::Let,
            Token::Ident("b".to_string()),
            Token::Assign,
            Token::Int(500.to_string()),
            Token::Semicolon,
        ];
        let lexer = Lexer::new(source);
        let tokens = lexer.into_iter().collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn ignores_whitespace_as_god_intended() {
        let source = r#"      =       +     "#;
        let expected_tokens: [Token; 2] = [Token::Assign, Token::Plus];
        let lexer = Lexer::new(source);
        let tokens = lexer.into_iter().collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn can_parse_sumbols() {
        let source = r#"(){}-+=,;><*/!"#;

        let expected_tokens: [Token; 14] = [
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Minus,
            Token::Plus,
            Token::Assign,
            Token::Comma,
            Token::Semicolon,
            Token::Gt,
            Token::Lt,
            Token::Asterisk,
            Token::Slash,
            Token::Bang,
        ];

        let lexer = Lexer::new(source);
        let tokens = lexer.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn string_can_use_escape_character() {
        let source =
            r#""\"" "hello\"there" "hello\t\tworld" "hello\n\nthere" "yeah\"yeah\"yeah\"""#;
        let lexer = Lexer::new(source);
        let tokens = lexer.collect::<Result<Vec<_>>>().unwrap();
        let expected_tokens: [Token; 5] = [
            Token::Str("\"".to_string()),
            Token::Str("hello\"there".to_string()),
            Token::Str("hello\t\tworld".to_string()),
            Token::Str("hello\n\nthere".to_string()),
            Token::Str("yeah\"yeah\"yeah\"".to_string()),
        ];
        println!("{}", tokens.first().unwrap());
        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn unenclosed_strings_will_error() {
        let source = r#""hello"#;
        let lexer = Lexer::new(source);
        let tokens = lexer.collect::<Result<Vec<_>>>();
        assert!(tokens.is_err());
    }

    #[test]
    fn can_parse_multiple_strings() {
        let source = r#""hello""hi""goodbye""#;

        let expected_tokens: [Token; 3] = [
            Token::Str("hello".to_string()),
            Token::Str("hi".to_string()),
            Token::Str("goodbye".to_string()),
        ];

        let lexer = Lexer::new(source);
        let tokens = lexer.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(tokens, expected_tokens);
    }

    // todo: the output when this fails is bad... Is there a better rust assert?
    #[test]
    fn can_parse_a_source_program() {
        let source = r#"
        let five = 5;
        let text = "Hello";
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
        return true;
        } else {
        return false;
        }

        10 == 10; 
        10 != 9;
        [1, 2];
        {"foo": "bar"};
        "#;

        let expected_tokens: Vec<Token> = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5.to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("text".to_string()),
            Token::Assign,
            Token::Str("Hello".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10.to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5.to_string()),
            Token::Semicolon,
            Token::Int(5.to_string()),
            Token::Lt,
            Token::Int(10.to_string()),
            Token::Gt,
            Token::Int(5.to_string()),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5.to_string()),
            Token::Lt,
            Token::Int(10.to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(10.to_string()),
            Token::Eq,
            Token::Int(10.to_string()),
            Token::Semicolon,
            Token::Int(10.to_string()),
            Token::NotEq,
            Token::Int(9.to_string()),
            Token::Semicolon,
            Token::LBracket,
            Token::Int(1.to_string()),
            Token::Comma,
            Token::Int(2.to_string()),
            Token::RBracket,
            Token::Semicolon,
            Token::LBrace,
            Token::Str("foo".to_string()),
            Token::Colon,
            Token::Str("bar".to_string()),
            Token::RBrace,
            Token::Semicolon,
        ];

        let lexer = Lexer::new(source);
        let tokens = lexer.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(tokens, expected_tokens);
    }
}
