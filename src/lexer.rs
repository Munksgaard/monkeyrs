use crate::token::Token;
use itertools::Itertools;
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.chars().peekable(),
        }
    }
}

fn is_letter(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn read_identifier<'a>(c: char, chars: &'a mut Peekable<Chars>) -> String {
    let mut res = String::new();
    res.push(c);

    for c_ in chars.by_ref().peeking_take_while(|c| is_letter(*c)) {
        res.push(c_);
    }

    res
}

fn read_integer<'a>(c: char, chars: &'a mut Peekable<Chars>) -> usize {
    let mut res = String::new();
    res.push(c);

    for c_ in chars.by_ref().peeking_take_while(char::is_ascii_digit) {
        res.push(c_);
    }

    res.parse()
        .unwrap_or_else(|e| panic!("Couldn't parse integer \"{}\": {}", res, e))
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        use Token::*;
        match self.chars.by_ref().skip_while(|c| c.is_whitespace()).next() {
            Some('=') => match self.chars.by_ref().peek() {
                Some('=') => {
                    let _ = self.chars.next();
                    Some(EQ)
                }
                _ => Some(ASSIGN),
            },
            Some(';') => Some(SEMICOLON),
            Some('(') => Some(LPAREN),
            Some(')') => Some(RPAREN),
            Some(',') => Some(COMMA),
            Some('+') => Some(PLUS),
            Some('{') => Some(LBRACE),
            Some('}') => Some(RBRACE),
            Some('-') => Some(MINUS),
            Some('!') => match self.chars.by_ref().peek() {
                Some('=') => {
                    let _ = self.chars.next();
                    Some(NOTEQ)
                }
                _ => Some(BANG),
            },
            Some('*') => Some(ASTERISK),
            Some('/') => Some(SLASH),
            Some('<') => Some(LT),
            Some('>') => Some(GT),
            Some('"') => {
                let mut result = String::new();
                loop {
                    match self.chars.next() {
                        Some('"') => break,
                        Some(c) => result.push(c),
                        None => break,
                    }
                }
                Some(STRING(result))
            }
            Some(c) => {
                if is_letter(c) {
                    let ident = read_identifier(c, &mut self.chars);
                    match &ident[..] {
                        "let" => Some(LET),
                        "fn" => Some(FUNCTION),
                        "true" => Some(TRUE),
                        "false" => Some(FALSE),
                        "if" => Some(IF),
                        "else" => Some(ELSE),
                        "return" => Some(RETURN),
                        _ => Some(IDENT(ident)),
                    }
                } else if c.is_ascii_digit() {
                    Some(INT(read_integer(c, &mut self.chars)))
                } else {
                    Some(ILLEGAL)
                }
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lex_1() {
        use Token::*;

        let input = "=+(){},;";

        let expected = [
            ASSIGN, PLUS, LPAREN, RPAREN, LBRACE, RBRACE, COMMA, SEMICOLON,
        ];

        let lexer = Lexer::new(input);

        let result: Vec<_> = lexer.collect();

        assert_eq!(expected[..], result[..]);
    }

    #[test]
    fn test_lex_2() {
        use Token::*;

        let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
"#;

        let expected = [
            LET,
            IDENT("five".to_string()),
            ASSIGN,
            INT(5),
            SEMICOLON,
            LET,
            IDENT("ten".to_string()),
            ASSIGN,
            INT(10),
            SEMICOLON,
            LET,
            IDENT("add".to_string()),
            ASSIGN,
            FUNCTION,
            LPAREN,
            IDENT("x".to_string()),
            COMMA,
            IDENT("y".to_string()),
            RPAREN,
            LBRACE,
            IDENT("x".to_string()),
            PLUS,
            IDENT("y".to_string()),
            SEMICOLON,
            RBRACE,
            SEMICOLON,
            LET,
            IDENT("result".to_string()),
            ASSIGN,
            IDENT("add".to_string()),
            LPAREN,
            IDENT("five".to_string()),
            COMMA,
            IDENT("ten".to_string()),
            RPAREN,
            SEMICOLON,
        ];

        let lexer = Lexer::new(input);

        let result: Vec<_> = lexer.collect();

        assert_eq!(expected[..], result[..]);
    }

    #[test]
    fn test_lex_3() {
        use Token::*;

        let input = r#"!-=/*5;
5 < 10 > 5;
"#;

        let expected = [
            BANG,
            MINUS,
            ASSIGN,
            SLASH,
            ASTERISK,
            INT(5),
            SEMICOLON,
            INT(5),
            LT,
            INT(10),
            GT,
            INT(5),
            SEMICOLON,
        ];

        let lexer = Lexer::new(input);

        let result: Vec<_> = lexer.collect();

        assert_eq!(expected[..], result[..]);
    }

    #[test]
    fn test_lex_4() {
        use Token::*;

        let input = r#"if (5 < 10) {
  return true;
} else {
  return false;
}
"#;

        let expected = [
            IF,
            LPAREN,
            INT(5),
            LT,
            INT(10),
            RPAREN,
            LBRACE,
            RETURN,
            TRUE,
            SEMICOLON,
            RBRACE,
            ELSE,
            LBRACE,
            RETURN,
            FALSE,
            SEMICOLON,
            RBRACE,
        ];

        let lexer = Lexer::new(input);

        let result: Vec<_> = lexer.collect();

        assert_eq!(expected[..], result[..]);
    }

    #[test]
    fn test_lex_5() {
        use Token::*;

        let input = r#"10 == 10;
10 != 9;
"#;

        let expected = [
            INT(10),
            EQ,
            INT(10),
            SEMICOLON,
            INT(10),
            NOTEQ,
            INT(9),
            SEMICOLON,
        ];

        let lexer = Lexer::new(input);

        let result: Vec<_> = lexer.collect();

        assert_eq!(expected[..], result[..]);
    }

    #[test]
    fn test_lex_6() {
        use Token::*;

        let input = r#""foobar";
"foo bar";
"#;

        let expected = [
            STRING("foobar".to_string()),
            SEMICOLON,
            STRING("foo bar".to_string()),
            SEMICOLON,
        ];

        let lexer = Lexer::new(input);

        let result: Vec<_> = lexer.collect();

        assert_eq!(expected[..], result[..]);
    }
}
